pub struct Literal {
    value: Value,
}

impl Literal {
    pub fn new(value: Value) -> Self {
        Literal { value }
    }

    pub fn eval(&self) -> Result<Value, Error> {
        Ok(self.value.clone())
    }
}