use std::collections::HashSet;

use crate::document::pipeline::read_document_blocks;

#[test]
fn test_read_all_blocks() {
    let doc_id = 335;
    let doc_block_list_path = r"test_data/docs/335/3/0";
    let blocks = read_document_blocks(doc_id, doc_block_list_path, HashSet::new()).unwrap();
    assert_eq!(blocks.len(), 3);
    assert_eq!(blocks[0].md, "Par 1");
    assert_eq!(blocks[1].md, "Par 2");
    assert_eq!(blocks[2].md, "Par 3");
}