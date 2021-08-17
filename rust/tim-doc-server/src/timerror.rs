use thiserror::Error;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Error)]
pub enum TimError {
    #[error("Failed to communicate with the database.")]
    Db,
    #[error("No item was found.")]
    ItemNotFound,
    #[error("Document does not exist or is empty.")]
    NonExistentOrEmptyDocument,
    #[error("Could not load the document.")]
    DocumentLoad,
    #[error("YAML is invalid.")]
    InvalidYaml,
}
