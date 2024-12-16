pub mod notification;
pub mod request;
pub mod response;

use notification::Notification;
use request::Request;
use response::Response;

#[derive(Debug, PartialEq)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}
