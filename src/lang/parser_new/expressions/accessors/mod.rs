use self::{index::{Index}, field::Field};

pub mod index;
pub mod field;

pub enum Accessor {
    Index(Index),
    Field(Field),
}

