#![feature(box_patterns)]

use document::{document::DocumentStore, processing::PipelineInput, DocId};
use pyo3::{exceptions::PyException, prelude::*};
use pythonize::pythonize;
pub mod document;
pub mod timerror;

#[pyfunction]
fn load_document(doc_id: i32) -> PyResult<PyObject> {
    let mut store = DocumentStore::new();
    let doc = store
        .drain_document(DocId(doc_id))
        .map_err(|e| PyException::new_err(e.to_string()))?;
    Python::with_gil(|py| pythonize(py, &doc.get_blocks()))
        .map_err(|e| PyException::new_err(e.to_string()))
}

#[pymodule]
fn tim_core(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(load_document, m)?)?;
    Ok(())
}
