use crate::document::docblock::AttributeContainer;
use crate::document::document::BlockPath;
use crate::document::document::DocRef;
use crate::document::document::DocumentStore;
use crate::document::BlockId;
use crate::document::DocBlock;
use crate::document::DocId;
use crate::document::Document;
use std::collections::HashSet;

#[derive(Debug)]
pub enum PP {
    Norm(CtxBlock),
    DerefBlock(Box<PP>, Box<PP>),

    // TODO maybe include CtxPar
    Error(String),

    Area(Vec<PP>),

    // TODO: CtxBlock maybe not needed
    Ref(CtxBlock, DocRef),

    Setting(CtxBlock),
    Plugin(CtxBlock),
}

impl From<CtxBlock> for PP {
    fn from(c: CtxBlock) -> Self {
        PP::Norm(c)
    }
}

#[derive(Debug)]
pub struct CtxBlock {
    doc: DocId,
    p: DocBlock,
}

impl CtxBlock {
    pub fn new(doc: DocId, p: DocBlock) -> Self {
        CtxBlock { doc, p }
    }
}

impl AttributeContainer for CtxBlock {
    fn get_attr(&self, k: &str) -> Option<&String> {
        self.p.get_attr(k)
    }
}

pub trait PipelineInput {
    fn get_doc_id(&self) -> DocId;
    fn get_blocks(self) -> Vec<CtxBlock>;
}

fn deref_single(p: DocRef, docs: &mut DocumentStore, visited_blocks: &mut HashSet<DocRef>) -> PP {
    if visited_blocks.contains(&p) {
        PP::Error("Reference cycle detected".to_owned())
    } else {
        match p {
            DocRef::Block(refdoc, ref rp) => match docs.load_blocklist(refdoc) {
                Ok(list) => match list.entries.get(rp) {
                    None => PP::Error("Paragraph not found".to_owned()),
                    Some(hash) => {
                        match DocBlock::from_path((&rp, &**hash).get_block_path(refdoc)) {
                            Ok(dp) => {
                                visited_blocks.insert(p);
                                preprocess_single(CtxBlock::new(refdoc, dp))
                            }
                            Err(_) => PP::Error("Paragraph not found".to_owned()),
                        }
                    }
                },
                Err(_) => PP::Error("Document not found".to_owned()),
            },
            DocRef::Area(refdoc, ref ra) => match docs.load_area(refdoc, ra) {
                Ok(blocks) => {
                    visited_blocks.insert(p);
                    PP::Area(preprocess_blocks(
                        blocks.into_iter().map(|p| CtxBlock::new(refdoc, p)),
                    ))
                }
                Err(_) => PP::Error("Failed to load area".to_owned()),
            },
        }
    }
}

fn dereference_blocks(
    blocks: Vec<PP>,
    docs: &mut DocumentStore,
    visited_blocks: &mut HashSet<DocRef>,
) -> Vec<PP> {
    blocks
        .into_iter()
        .map(|mut pp| loop {
            pp = match pp {
                PP::Norm(_) => return pp,
                PP::DerefBlock(box PP::Norm(_), _) => return pp,
                PP::DerefBlock(box PP::DerefBlock(_, _), _) => {
                    unreachable!("first slot of DerefPar cannot contain DerefPar")
                }
                PP::DerefBlock(box PP::Error(_), _) => return pp,
                PP::DerefBlock(box PP::Area(blocks), p2) => {
                    return PP::DerefBlock(
                        Box::from(PP::Area(dereference_blocks(blocks, docs, visited_blocks))),
                        p2,
                    )
                }
                PP::DerefBlock(box PP::Ref(ref _ctxblock, ref docref), _) => PP::DerefBlock(
                    Box::from(deref_single(docref.clone(), docs, visited_blocks)),
                    Box::from(pp),
                ),
                PP::DerefBlock(box PP::Setting(_), _) => return pp,
                PP::DerefBlock(box PP::Plugin(_), _) => return pp,
                PP::Error(_) => return pp,
                PP::Area(blocks) => {
                    return PP::Area(dereference_blocks(blocks, docs, visited_blocks))
                }
                PP::Ref(ref _ctxblock, ref docref) => PP::DerefBlock(
                    Box::from(deref_single(docref.clone(), docs, visited_blocks)),
                    Box::from(pp),
                ),
                PP::Setting(_) => return pp,
                PP::Plugin(_) => return pp,
            };
        })
        .collect()
}

fn preprocess_single(p: CtxBlock) -> PP {
    let attrs = &p.p.attrs.others;
    let plugin = attrs.get("plugin");
    let rd = attrs.get("rd").and_then(|rd| rd.parse().ok());
    let ra = attrs.get("ra").cloned();
    let rp = attrs.get("rp").cloned();
    let s = attrs.get("settings");
    match (plugin, rd, rp, ra, s) {
        (_, Some(rd), Some(rp), _, _) => PP::Ref(p, DocRef::Block(DocId(rd), BlockId(rp))),
        (_, Some(rd), _, Some(ra), _) => PP::Ref(p, DocRef::Area(DocId(rd), ra)),
        (Some(_name), _, _, _, _) => PP::Plugin(p),
        (_, _, _, _, Some(_name)) => PP::Setting(p),
        (_, _, _, _, _) => PP::Norm(p),
    }
}

fn preprocess_blocks(blocks: impl IntoIterator<Item = CtxBlock>) -> Vec<PP> {
    let mut result = Vec::new();
    let mut iter = blocks.into_iter();
    let mut curr_areas = Vec::new();
    while let Some(next) = iter.next() {
        if let Some(_area) = next.p.attrs.others.get("area") {
            curr_areas.push(vec![]);
        }
        let maybe_area_end = next.p.attrs.others.get("area_end").cloned();
        let pp = preprocess_single(next);
        if let Some(last) = curr_areas.last_mut() {
            last.push(pp);
        } else {
            result.push(pp);
        }
        if let Some(_area) = maybe_area_end {
            if let Some(blocks) = curr_areas.pop() {
                result.push(PP::Area(blocks))
            }
        }
    }
    result
}

pub fn run_html_pipeline(docs: &mut DocumentStore, x: impl PipelineInput) -> Vec<PP> {
    let blocks = x.get_blocks();
    let mut visited = HashSet::new();
    let blocks = dereference_blocks(preprocess_blocks(blocks), docs, &mut visited);
    blocks
}

impl PipelineInput for Document {
    fn get_doc_id(&self) -> DocId {
        self.id
    }

    fn get_blocks(self) -> Vec<CtxBlock> {
        let id = self.id;
        self.blocks
            .into_iter()
            .map(|p| CtxBlock::new(id, p))
            .collect()
    }
}
