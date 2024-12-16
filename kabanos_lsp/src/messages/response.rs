#[derive(Debug, PartialEq)]
pub struct Response {
    pub id: i64,
    pub result: ResponseKind,
}

#[derive(Debug, PartialEq)]
pub enum ResponseKind {
    Success(serde_json::Value),
    Error(serde_json::Value),
}
