use serde_json::Value;

#[derive(Debug, PartialEq)]
pub enum Notification {
    Unknown {
        method: Value,
        params: Option<Value>,
    },
}
