use chrono::prelude::*;
use crate::document::docparagraph::AttributeContainer;
use crate::document::docparagraph::ParIdLike;
use crate::document::DocParagraph;
use crate::document::ParId;
use crate::timerror::TimErrorKind;
use failure::Error;
use failure::ResultExt;
use indexmap::IndexMap;
use rayon::iter::IntoParallelIterator;
use rayon::prelude::*;
use serde_derive::Deserialize;
use serde_json;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::path::PathBuf;

trait ParPath {
    fn get_par_path(&self, d: DocId) -> PathBuf;
}

impl<'a, T> ParPath for (&'a T, &'a str)
where
    T: ParIdLike,
{
    fn get_par_path(&self, d: DocId) -> PathBuf {
        d.get_pars_path().join(&(self.0).get_str()).join(self.1)
    }
}

//impl<'a, T> ParPath for (T, &'a str)
//where
//    T: ParIdLike,
//{
//    fn get_par_path(&self, d: DocId) -> PathBuf {
//        d.get_pars_path().join(&(self.0).get_str()).join(self.1)
//    }
//}

impl<'a, T> ParPath for (T, &'a String)
where
    T: ParIdLike,
{
    fn get_par_path(&self, d: DocId) -> PathBuf {
        let (id, hash) = self;
        (id, &***hash).get_par_path(d)
    }
}

pub struct BlockList {
    pub entries: IndexMap<ParId, String>,
}

impl BlockList {
    pub fn new(entries: IndexMap<ParId, String>) -> Self {
        BlockList { entries }
    }

    pub fn get_entry(&self, p: &ParId) -> Option<&str> {
        match self.entries.get(p) {
            None => None,
            Some(val) => Some(val),
        }
    }
}

pub struct DocumentStore {
    pub docs: HashMap<DocId, Document>,
    pub block_lists: HashMap<DocId, BlockList>,
}

impl DocumentStore {
    fn load_document(&mut self, id: DocId) -> Result<&Document, Error> {
        match self.docs.entry(id) {
            Entry::Occupied(o) => Ok(o.into_mut()),
            Entry::Vacant(v) => {
                let doc = Document::load_newest(id, id.get_docs_path(), id.get_pars_path())?;
                Ok(v.insert(doc))
            }
        }
    }

    fn load_blocklist(&mut self, id: DocId) -> Result<&BlockList, Error> {
        match self.block_lists.entry(id) {
            Entry::Occupied(o) => Ok(o.into_mut()),
            Entry::Vacant(v) => {
                let doc = Document::load_lines(id, id.get_docs_path())?;
                Ok(v.insert(doc))
            }
        }
    }

    fn load_area(&mut self, id: DocId, area_name: &str) -> Result<Vec<DocParagraph>, Error> {
        let doc = self.load_document(id)?;
        let mut pars = Vec::new();
        get_area(
            &mut pars,
            &mut doc
                .pars
                .iter()
                .skip_while(|p| {
                    p.attrs
                        .others
                        .get("area")
                        .map_or(true, |name| name != area_name)
                })
                .cloned(),
            area_name,
        );
        Ok(pars)
    }

    pub fn new() -> Self {
        DocumentStore {
            docs: HashMap::new(),
            block_lists: HashMap::new(),
        }
    }
}

fn get_area<T>(result: &mut Vec<T>, pars: &mut impl Iterator<Item = T>, area_name: &str)
where
    T: AttributeContainer,
{
    for p in pars {
        if !p
            .get_attr("area_end")
            .map_or(true, |name| name != area_name)
        {
            result.push(p);
            break;
        }
        result.push(p);
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
pub struct DocId(pub i32);

impl DocId {
    pub fn get_docs_path(&self) -> PathBuf {
        format!("../timApp/tim_files/docs/{}", self.0).into()
    }
    pub fn get_pars_path(&self) -> PathBuf {
        format!("../timApp/tim_files/pars/{}", self.0).into()
    }
}

#[derive(Debug)]
pub enum PP {
    Norm(CtxPar),
    DerefPar(Box<PP>, Box<PP>),

    // TODO maybe include CtxPar
    Error(String),

    Area(Vec<PP>),

    // TODO: CtxPar maybe not needed
    Ref(CtxPar, DocRef),

    Setting(CtxPar),
    Plugin(CtxPar),
}

impl From<CtxPar> for PP {
    fn from(c: CtxPar) -> Self {
        PP::Norm(c)
    }
}

#[derive(Debug)]
pub struct CtxPar {
    doc: DocId,
    p: DocParagraph,
}

impl CtxPar {
    pub fn new(doc: DocId, p: DocParagraph) -> Self {
        CtxPar { doc, p }
    }
}

impl AttributeContainer for CtxPar {
    fn get_attr(&self, k: &str) -> Option<&String> {
        self.p.get_attr(k)
    }
}

pub trait PipelineInput {
    fn get_doc_id(&self) -> DocId;
    fn get_blocks(self) -> Vec<CtxPar>;
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum DocRef {
    Par(DocId, ParId),
    Area(DocId, String),
}

fn deref_single(p: DocRef, docs: &mut DocumentStore, visited_pars: &mut HashSet<DocRef>) -> PP {
    if visited_pars.contains(&p) {
        PP::Error("Reference cycle detected".to_owned())
    } else {
        match p {
            DocRef::Par(refdoc, ref rp) => match docs.load_blocklist(refdoc) {
                Ok(list) => match list.entries.get(rp) {
                    None => PP::Error("Paragraph not found".to_owned()),
                    Some(hash) => {
                        match DocParagraph::from_path((&rp, &**hash).get_par_path(refdoc)) {
                            Ok(dp) => {
                                visited_pars.insert(p);
                                preprocess_single(CtxPar::new(refdoc, dp))
                            }
                            Err(_) => PP::Error("Paragraph not found".to_owned()),
                        }
                    }
                },
                Err(_) => PP::Error("Document not found".to_owned()),
            },
            DocRef::Area(refdoc, ref ra) => match docs.load_area(refdoc, ra) {
                Ok(pars) => {
                    visited_pars.insert(p);
                    PP::Area(preprocess_pars(
                        pars.into_iter().map(|p| CtxPar::new(refdoc, p)),
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
    visited_pars: &mut HashSet<DocRef>,
) -> Vec<PP> {
    blocks
        .into_iter()
        .map(|mut pp| loop {
            pp = match pp {
                PP::Norm(_) => return pp,
                PP::DerefPar(box PP::Norm(_), _) => return pp,
                PP::DerefPar(box PP::DerefPar(_, _), _) => {
                    unreachable!("first slot of DerefPar cannot contain DerefPar")
                }
                PP::DerefPar(box PP::Error(_), _) => return pp,
                PP::DerefPar(box PP::Area(pars), p2) => {
                    return PP::DerefPar(
                        Box::from(PP::Area(dereference_blocks(pars, docs, visited_pars))),
                        p2,
                    )
                }
                PP::DerefPar(box PP::Ref(ref _ctxpar, ref docref), _) => PP::DerefPar(
                    Box::from(deref_single(docref.clone(), docs, visited_pars)),
                    Box::from(pp),
                ),
                PP::DerefPar(box PP::Setting(_), _) => return pp,
                PP::DerefPar(box PP::Plugin(_), _) => return pp,
                PP::Error(_) => return pp,
                PP::Area(pars) => return PP::Area(dereference_blocks(pars, docs, visited_pars)),
                PP::Ref(ref _ctxpar, ref docref) => PP::DerefPar(
                    Box::from(deref_single(docref.clone(), docs, visited_pars)),
                    Box::from(pp),
                ),
                PP::Setting(_) => return pp,
                PP::Plugin(_) => return pp,
            };
        })
        .collect()
}

fn preprocess_single(p: CtxPar) -> PP {
    let attrs = &p.p.attrs.others;
    let plugin = attrs.get("plugin");
    let rd = attrs.get("rd").and_then(|rd| rd.parse().ok());
    let ra = attrs.get("ra").cloned();
    let rp = attrs.get("rp").cloned();
    let s = attrs.get("settings");
    match (plugin, rd, rp, ra, s) {
        (_, Some(rd), Some(rp), _, _) => PP::Ref(p, DocRef::Par(DocId(rd), ParId(rp))),
        (_, Some(rd), _, Some(ra), _) => PP::Ref(p, DocRef::Area(DocId(rd), ra)),
        (Some(_name), _, _, _, _) => PP::Plugin(p),
        (_, _, _, _, Some(_name)) => PP::Setting(p),
        (_, _, _, _, _) => PP::Norm(p),
    }
}

fn preprocess_pars(pars: impl IntoIterator<Item = CtxPar>) -> Vec<PP> {
    let mut result = Vec::new();
    let mut iter = pars.into_iter();
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
            if let Some(areapars) = curr_areas.pop() {
                result.push(PP::Area(areapars))
            }
        }
    }
    result
}

pub fn run_html_pipeline(docs: &mut DocumentStore, x: impl PipelineInput) -> Vec<PP> {
    let blocks = x.get_blocks();
    let mut visited = HashSet::new();
    let blocks = dereference_blocks(preprocess_pars(blocks), docs, &mut visited);
    blocks
}

#[allow(unused)]
pub enum Reference {
    Par {
        dest_id: ParId,
        dest_doc_id: DocId,
        par: DocParagraph,
    },
    Area {
        name: ParId,
        dest_doc_id: DocId,
        par: DocParagraph,
    },
}

#[allow(unused)]
pub struct Area(Vec<DocPart>);

#[allow(unused)]
pub enum DocPart {
    Par(DocParagraph),
    Reference(Reference),
    Area(Area),
}

#[derive(Debug)]
pub struct Document<T = DocParagraph> {
    id: DocId,
    pars: Vec<T>,
}

#[derive(Deserialize, Debug)]
pub struct OpParams {
    old_hash: Option<String>,
    new_hash: Option<String>,
    before_id: Option<String>,
}

#[derive(Deserialize, Debug)]
pub enum ChangeLogEntryKind {
    Added,
    Deleted,
    Inserted,
    Modified,
}

#[derive(Deserialize, Debug)]
pub struct ChangeLogEntry {
    group_id: i32,
    par_id: ParId,
    op: ChangeLogEntryKind,
    op_params: Option<OpParams>,
    pub ver: [i32; 2],
    #[serde(with = "changelog_date_format")]
    time: DateTime<Utc>,
}

mod changelog_date_format {
    use chrono::{DateTime, TimeZone, Utc};
    use serde::{self, Deserialize, Deserializer, Serializer};

    const FORMAT: &'static str = "%Y-%m-%d %H:%M:%S";

    #[allow(unused)]
    pub fn serialize<S>(date: &DateTime<Utc>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = format!("{}", date.format(FORMAT));
        serializer.serialize_str(&s)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<DateTime<Utc>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Utc.datetime_from_str(&s, FORMAT)
            .map_err(serde::de::Error::custom)
    }
}

fn split_block_line(mut l: String) -> Result<(String, String), Error> {
    let index = l.find('/').ok_or(TimErrorKind::DocumentLoad)?;
    let mut v = l.split_off(index);
    v.drain(..1);
    Ok((l, v))
}

impl Document {
    pub fn load_lines<T>(id: DocId, history_path: T) -> Result<BlockList, Error>
    where
        T: AsRef<Path>,
    {
        let changelog = File::open(history_path.as_ref().join("changelog"))
            .context(TimErrorKind::NonExistentOrEmptyDocument)?;
        let mut file = BufReader::new(&changelog);
        let mut s = String::new();
        file.read_line(&mut s)?;
        let p: ChangeLogEntry = serde_json::from_str(&s)?;
        let version_file_path = history_path
            .as_ref()
            .join(format!("{}/{}", p.ver[0], p.ver[1]));
        let cfile = File::open(version_file_path).context("Failed to open block list file")?;
        let cfiler = BufReader::new(&cfile);
        let lines = cfiler
            .lines()
            .map(|l| {
                l.map(|line| -> Result<(ParId, String), Error> {
                    let (id, h) = split_block_line(line)?;
                    Ok((ParId(id), h))
                })
            })
            .collect::<Result<Result<_, _>, _>>()??;
        Ok(BlockList::new(lines))
    }

    pub fn load_newest<T>(id: DocId, history_path: T, par_path: T) -> Result<Document, Error>
    where
        T: AsRef<Path> + Sync,
    {
        let lines = Self::load_lines(id, history_path)?;
        let pars = lines
            .entries
            .iter()
            .collect::<Vec<_>>() // TODO indexmap doesn't support rayon yet
            .into_par_iter()
            .map(|parhash| {
                let p = DocParagraph::from_path(parhash.get_par_path(id));
                p
            })
            .collect::<Result<_, _>>()?;
        Ok(Document { id, pars })
    }

    pub fn gen_block_list(&self) -> BlockList {
        BlockList::new(
            self.pars
                .iter()
                .map(|p| (p.id.clone(), p.t.clone()))
                .collect(),
        )
    }
}

impl PipelineInput for Document {
    fn get_doc_id(&self) -> DocId {
        self.id
    }

    fn get_blocks(self) -> Vec<CtxPar> {
        let id = self.id;
        self.pars.into_iter().map(|p| CtxPar::new(id, p)).collect()
    }
}
