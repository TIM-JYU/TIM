/// Path to the files directory in TIM DB.
#[allow(dead_code)]
pub const FILES_PATH: &str = r"/tim_files";

/// Returns the path to a document paragraph block file.
///
/// # Arguments
///
/// * `doc_id`: Document ID that contains the paragraph block.
/// * `doc_par_id`: Paragraph block ID.
/// * `doc_par_hash`: Hash of the paragraph block. Used for selecting the specific version of the block.
///
/// returns: Path to the paragraph block file.
#[inline(always)]
pub fn get_doc_par_block_path(doc_id: &u64, doc_par_id: &str, doc_par_hash: &str) -> String {
    #[cfg(not(test))]
    {
        format!("{}/pars/{}/{}/{}", FILES_PATH, doc_id, doc_par_id, doc_par_hash)
    }
    #[cfg(test)]
    {
        format!("test_data/pars/{}/{}/{}", doc_id, doc_par_id, doc_par_hash)
    }
}