use std::collections::HashSet;
use std::path::Path;

use anyhow::Context;
use rayon::prelude::*;

use crate::document::parblock::DocumentParagraphBlock;

/// Reads all document paragraph blocks into a single vector.
///
/// # Arguments
///
/// * `doc_id`: ID of the document
/// * `doc_block_list_path`: Path to the block list file to read
/// * `skip_blocks`: Set of block IDs to skip parsing. The blocks will still be included into the list, but they will be stubbed.
///
/// returns: Vector of blocks
pub fn read_document_blocks(
    doc_id: u64,
    doc_block_list_path: impl AsRef<Path>,
    skip_blocks: HashSet<String>,
) -> Result<Vec<DocumentParagraphBlock>, anyhow::Error> {
    let doc_par_file_text = std::fs::read_to_string(doc_block_list_path).context("Could not read block list file")?;
    // Iterate the block list and extract the docblock id and type
    let block_lines = doc_par_file_text
        .split("\n")
        .into_iter()
        .filter(|line| !line.is_empty())
        .map(|line| {
            // FIXME: Some older files may still use the "current" symlink and thus not have the separator!
            let (doc_par_id, doc_par_hash) = line.split_once("/").unwrap();
            (doc_par_id, doc_par_hash, skip_blocks.contains(doc_par_id))
        })
        .collect::<Vec<_>>();

    // Read all blocks in parallel
    let blocks: Vec<DocumentParagraphBlock> = block_lines
        .par_iter()
        .map(|(doc_par_id, doc_par_hash, skip)|
            if *skip {
                DocumentParagraphBlock::new(doc_par_id, doc_par_hash)
            } else {
                DocumentParagraphBlock::read_json_safe(doc_id, doc_par_id, doc_par_hash)
            }
        )
        .collect();

    Ok(blocks)
}