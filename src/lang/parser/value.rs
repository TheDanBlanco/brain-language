pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Value>),
    Object(BTreeMap<String, Value>),
    Null,
}