use derive_builder::Builder;

#[derive(Builder)]
pub enum Enum {}

#[derive(Builder)]
pub union Union {
    dummy: i32,
}

fn main() {}
