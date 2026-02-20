use std::{error::Error, io};

pub type NoerResult<T> = Result<T, Box<dyn Error>>;

pub fn fail<T>(msg: impl Into<String>) -> NoerResult<T> {
    Err(Box::new(io::Error::new(io::ErrorKind::Other, msg.into())))
}
