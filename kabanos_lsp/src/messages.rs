#[derive(Debug)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

#[derive(Debug)]
pub struct Request {
    id: u32,
    method: RequestMethod,
}

#[derive(Debug)]
pub enum RequestMethod {}

#[derive(Debug)]
pub struct Response;

#[derive(Debug)]
pub struct Notification;
