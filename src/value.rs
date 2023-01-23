#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Integer(i32),
    String(String),
    Boolean(bool)
}
