use lsp_types::InitializeParams;

#[derive(Debug, PartialEq)]
pub struct Request {
    pub id: i64,
    pub method: RequestMethod,
}

#[derive(Debug, PartialEq)]
pub enum RequestMethod {
    Unknown(String),
    Initialize(InitializeParams),
    Shutdown,
}
