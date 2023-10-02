use std::collections::HashMap;

use anyhow::Context;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::document::dbpaths::get_doc_par_block_path;

/// A document paragraph block.
/// TIM documents are composed of paragraphs of text and plugins, which are grouped into blocks.
/// A user can add, edit and delete blocks.
/// Each block contains the saved raw text, cached HTML and block attributes.
#[derive(Deserialize, Serialize, Debug)]
pub struct DocumentParagraphBlock {
    pub id: String,
    pub md: String,
    pub t: String,
    pub attrs: HashMap<String, Value>,
    pub h: Option<HashMap<String, String>>,
}

impl DocumentParagraphBlock {

    /// Reads a paragraph block from a JSON file.
    /// Generates an error if the file cannot be read or parsed.
    ///
    /// # Arguments
    ///
    /// * `doc_id`: Document ID that contains the paragraph block.
    /// * `par_id`: Paragraph block ID.
    /// * `t`: Paragraph block hash.
    ///
    /// returns: Result<DocumentParagraphBlock, Error>
    pub fn read_json(doc_id: u64, par_id: &str, t: &str) -> Result<Self, anyhow::Error> {
        let file_path = get_doc_par_block_path(&doc_id, par_id, t);
        let file_text = std::fs::read_to_string(file_path).context("Could not read paragraph file")?;
        let json = serde_json::from_str(&file_text).context("Could not parse paragraph file")?;
        Ok(json)
    }

    /// Reads a paragraph block from a JSON file.
    /// If the file cannot be read or parsed, returns a paragraph with an error message.
    ///
    /// # Arguments
    ///
    /// * `doc_id`: Document ID that contains the paragraph block.
    /// * `par_id`: Paragraph block ID.
    /// * `t`: Paragraph block hash.
    ///
    /// returns: DocumentParagraphBlock
    pub fn read_json_safe(doc_id: u64, par_id: &str, t: &str) -> Self {
        Self::read_json(doc_id, par_id, t)
            .unwrap_or_else(|e| Self {
                id: par_id.to_string(),
                md: format!("[**ERROR**: Could not load {}/{}/{}: {:#}]{{.error}}", doc_id, par_id, t, e),
                attrs: HashMap::new(),
                h: None,
                t: t.to_string(),
            })
    }

    /// Creates a new empty paragraph block for a given paragraph ID and hash.
    ///
    /// # Arguments
    ///
    /// * `par_id`: Paragraph block ID.
    /// * `t`: Paragraph block hash.
    ///
    /// returns: DocumentParagraphBlock
    pub fn new(par_id: &str, t: &str) -> Self {
        Self {
            id: par_id.to_string(),
            md: String::new(),
            attrs: HashMap::new(),
            h: None,
            t: t.to_string(),
        }
    }
}