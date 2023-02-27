pub struct Collection {
    elements: Vec<Expression>,
}

impl Collection {
    pub fn new(elements: Vec<Expression>) -> Self {
        Collection { elements }
    }

    pub fn eval(&self) -> Result<Value, Error> {
        let mut values = Vec::new();

        for element in self.elements {
            values.push(element.eval()?);
        }

        Ok(Value::Collection(values))
    }
}