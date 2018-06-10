use failure::Backtrace;
use failure::Context;
use failure::Fail;
use std::fmt;
use std::fmt::Display;

#[derive(Debug)]
pub struct TimError {
    inner: Context<TimErrorKind>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Fail)]
pub enum TimErrorKind {
    #[fail(display = "Failed to communicate with the database.")]
    Db,
    #[fail(display = "No item was found.")]
    ItemNotFound,
    #[fail(display = "Document does not exist or is empty.")]
    NonExistentOrEmptyDocument,
    #[fail(display = "Could not load the document.")]
    DocumentLoad,
    #[fail(display = "YAML is invalid.")]
    InvalidYaml,
}

impl Fail for TimError {
    fn cause(&self) -> Option<&Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl Display for TimError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl TimError {
    #[allow(dead_code)]
    pub fn kind(&self) -> TimErrorKind {
        *self.inner.get_context()
    }
}

impl From<TimErrorKind> for TimError {
    fn from(kind: TimErrorKind) -> TimError {
        TimError {
            inner: Context::new(kind),
        }
    }
}

impl From<Context<TimErrorKind>> for TimError {
    fn from(inner: Context<TimErrorKind>) -> TimError {
        TimError { inner }
    }
}
