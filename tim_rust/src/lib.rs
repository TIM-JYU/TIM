use std::collections::HashSet;

use pyo3::exceptions::PyException;
use pyo3::prelude::*;
use pythonize::pythonize;

use crate::document::pipeline::read_document_blocks;

mod document;
#[cfg(test)]
mod tests;


/// Reads all blocks in a document safely.
/// If a block cannot be read, it is replaced with an error indicator.
///
/// # Arguments
///
/// * `doc_id`: ID of the document
/// * `doc_block_list_path`: Path to the block list file to read
/// * `skip_blocks`: Set of block IDs to skip
///
/// returns: A list of blocks as dicts.
#[pyfunction]
fn read_all_blocks(
    doc_id: u64,
    doc_block_list_path: String,
    skip_blocks: HashSet<String>,
) -> PyResult<PyObject> {
    let blocks = read_document_blocks(doc_id, doc_block_list_path, skip_blocks)
        .map_err(|e| PyException::new_err(e.to_string()))?;
    Python::with_gil(|py| pythonize(py, &blocks))
        .map_err(|e| PyException::new_err(e.to_string()))
}

/// A Python module implemented in Rust.
#[pymodule]
fn tim_rust(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(read_all_blocks, m)?)?;
    Ok(())
}