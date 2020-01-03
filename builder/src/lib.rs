extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, Data, DeriveInput, Error, Field, Ident, Index, Lit, LitStr, Member, Meta,
    NestedMeta, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(input)
        .map(proc_macro::TokenStream::from)
        .unwrap_or_else(|e| e.to_compile_error().into())
}

fn expand(input: DeriveInput) -> syn::Result<TokenStream> {
    let data = match &input.data {
        Data::Struct(data) => data,
        Data::Enum(_) => {
            return Err(Error::new_spanned(
                &input,
                format_args!("cannot derive `Builder` for enum `{}`", &input.ident),
            ))
        }
        Data::Union(_) => {
            return Err(Error::new_spanned(
                &input,
                format_args!("cannot derive `Builder` for union `{}`", &input.ident),
            ))
        }
    };

    if let Some(attr) = input.attrs.first() {
        return Err(Error::new_spanned(
            &attr.path,
            format_args!(
                "cannot find attribute `{}` in this scope",
                attr.path.to_token_stream()
            ),
        ));
    }

    let fields = data
        .fields
        .iter()
        .enumerate()
        .map(|(idx, field)| FieldInfo::parse(idx, field))
        .collect::<syn::Result<Vec<_>>>()?;

    let vis = &input.vis;
    let name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let error_name = Ident::new(&format!("{}BuilerError", name), Span::call_site());

    let builder_field_defs = fields.iter().map(FieldInfo::to_builder_field_def);
    let builder_field_inits = fields.iter().map(FieldInfo::to_builder_field_init);
    let builder_set_methods = fields.iter().map(FieldInfo::to_builder_set_method);
    let builder_field_builds = fields.iter().map(FieldInfo::to_builder_field_build);

    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_field_inits),*
                }
            }
        }

        #vis struct #builder_name {
            #(#builder_field_defs),*
        }

        impl #builder_name {
            #(#builder_set_methods)*

            fn build(&mut self) -> ::core::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                Ok(#name { #(#builder_field_builds),* })
            }
        }

        #[derive(Debug)]
        struct #error_name {
            field: &'static str,
        }

        impl ::std::error::Error for #error_name {}
        impl ::std::fmt::Display for #error_name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "builder method `{}` is not called", self.field)
            }
        }
        impl ::core::convert::From<&'static str> for #error_name {
            fn from(field: &'static str) -> Self {
                Self { field }
            }
        }
    };

    Ok(expanded)
}

fn strip_container_ty<'a>(ty: &'a Type, container_name: &str) -> Option<&'a Type> {
    use syn::*;
    fn path_container_segment<'a>(path: &'a Path, container_name: &str) -> Option<&'a PathSegment> {
        if path.leading_colon.is_none()
            && path.segments.len() == 1
            && path.segments.first().unwrap().ident == container_name
        {
            path.segments.first()
        } else {
            None
        }
    }

    let type_params = match &ty {
        Type::Path(typepath) if typepath.qself.is_none() => {
            &path_container_segment(&typepath.path, container_name)?.arguments
        }
        _ => return None,
    };
    let generic_arg = match type_params {
        PathArguments::AngleBracketed(params) => params.args.first()?,
        _ => return None,
    };
    match generic_arg {
        GenericArgument::Type(ty) => Some(ty),
        _ => None,
    }
}

struct FieldAttr {
    each: Option<(Ident, Type)>,
}

impl FieldAttr {
    fn parse(field: &Field) -> syn::Result<Self> {
        let mut result = FieldAttr { each: None };
        for attr in &field.attrs {
            let list = match attr.parse_meta()? {
                meta @ Meta::Path(..) | meta @ Meta::NameValue(..) => {
                    return Err(Error::new_spanned(&meta, "expected `#[builder(...)]`"))
                }
                Meta::List(list) => list,
            };
            for nested in &list.nested {
                result.parse_nested(field, nested)?;
            }
        }
        Ok(result)
    }

    fn parse_nested(&mut self, field: &Field, nested: &NestedMeta) -> syn::Result<()> {
        let nv = match nested {
            NestedMeta::Meta(Meta::NameValue(nv)) => nv,
            nested => {
                return Err(Error::new_spanned(
                    &nested,
                    format_args!("expected name-value list"),
                ))
            }
        };

        let ident = nv
            .path
            .get_ident()
            .ok_or_else(|| Error::new_spanned(&nv.path, "expected identifier"))?;

        if ident == "each" {
            let ident = match &nv.lit {
                Lit::Str(s) => s.parse()?,
                lit => return Err(Error::new_spanned(lit, "expected string literal")),
            };
            let elem_ty = strip_container_ty(&field.ty, "Vec")
                .ok_or_else(|| Error::new_spanned(&field.ty, "expected `Vec<_>`"))?;
            self.each = Some((ident, elem_ty.clone()));
        } else {
            return Err(Error::new_spanned(
                &ident,
                format_args!("unknown identifier `{}` found", ident),
            ));
        }
        Ok(())
    }
}

struct FieldInfo {
    builder_name: Ident,
    builder_ty: Type,
    target_member: Member,
    is_optional: bool,
    attr: FieldAttr,
}

impl FieldInfo {
    fn parse(idx: usize, field: &Field) -> syn::Result<Self> {
        let attr = FieldAttr::parse(field)?;
        let (builder_ty, is_optional) = strip_container_ty(&field.ty, "Option")
            .map(|ty| (ty.clone(), true))
            .unwrap_or((field.ty.clone(), false));

        let (builder_name, target_member) = if let Some(name) = &field.ident {
            let mut name = name.clone();
            name.set_span(Span::call_site());
            let member = Member::Named(name.clone());
            (name, member)
        } else {
            let name = Ident::new(&format!("_{}", idx), Span::call_site());
            let member = Member::Unnamed(Index {
                index: idx as _,
                span: Span::call_site(),
            });
            (name, member)
        };

        Ok(Self {
            builder_name,
            builder_ty,
            target_member,
            is_optional,
            attr,
        })
    }

    fn to_builder_field_def(&self) -> TokenStream {
        let name = &self.builder_name;
        let ty = &self.builder_ty;
        if self.attr.each.is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: ::core::option::Option<#ty> }
        }
    }

    fn to_builder_field_init(&self) -> TokenStream {
        let name = &self.builder_name;
        quote! { #name: ::core::default::Default::default() }
    }

    fn to_builder_set_method(&self) -> TokenStream {
        let builder_name = &self.builder_name;
        let builder_ty = &self.builder_ty;
        if let Some((each_name, elem_ty)) = self.attr.each.as_ref() {
            quote! {
                fn #each_name(&mut self, #each_name: #elem_ty) -> &mut Self {
                    <#builder_ty as ::core::iter::Extend<_>>::extend(&mut self.#builder_name, ::core::iter::once(#each_name));
                    self
                }
            }
        } else {
            quote! {
                fn #builder_name(&mut self, #builder_name: #builder_ty) -> &mut Self {
                    self.#builder_name = Some(#builder_name);
                    self
                }
            }
        }
    }

    fn to_builder_field_build(&self) -> TokenStream {
        let member = &self.target_member;
        let name = &self.builder_name;
        if self.attr.each.is_some() || self.is_optional {
            quote! { #member: ::core::mem::take(&mut self.#name) }
        } else {
            let member_str = LitStr::new(&format!("{}", name), Span::call_site());
            quote! { #member: ::core::mem::take(&mut self.#name).ok_or(#member_str)? }
        }
    }
}
