use chrono::prelude::*;
use crate::document::DocParagraph;
use crate::document::ParId;
use crate::timerror::TimErrorKind;
use failure::Error;
use failure::ResultExt;
use rayon::prelude::*;
use serde_derive::Deserialize;
use serde_json;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::path::PathBuf;

pub struct BlockListEntry {
    pub par_id: ParId,
    pub hash: String,
}

impl BlockListEntry {
    pub fn new(par_id: ParId, hash: String) -> Self {
        BlockListEntry { par_id, hash }
    }

    pub fn get_par_path(&self, d: DocId) -> PathBuf {
        d.get_pars_path().join(&self.par_id.0).join(&self.hash)
    }
}

pub struct BlockList {
    pub entries: Vec<BlockListEntry>,
}

impl BlockList {
    pub fn new(entries: Vec<BlockListEntry>) -> Self {
        BlockList { entries }
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

struct CtxPar {
    doc: DocId,
    p: DocParagraph,
}

impl CtxPar {
    pub fn new(doc: DocId, p: DocParagraph) -> Self {
        CtxPar { doc, p }
    }
}

trait PipelineInput {
    fn get_doc_id(&self) -> DocId;
    fn get_blocks(self) -> Vec<CtxPar>;
}

fn dereference_blocks(blocks: Vec<CtxPar>) -> Vec<CtxPar> {
    blocks
        .into_iter()
        .flat_map(|CtxPar { p, doc }| {
            let attr_map = &p.attrs;
            match (
                attr_map.rd.as_ref().map(|rd| rd.parse()),
                attr_map.rp.as_ref(),
                attr_map.ra.as_ref(),
            ) {
                (Some(Ok(rd)), Some(rp), None) => {
                    let x = DocParagraph::from_path("").unwrap_or(p);
                    dereference_blocks(vec![CtxPar {
                        p: x,
                        doc: DocId(rd),
                    }])
                }
                (None, None, None)
                | (None, None, Some(_))
                | (None, Some(_), Some(_))
                | (None, Some(_), None)
                | (Some(Err(_)), _, _)
                | (Some(Ok(_)), None, None)
                | (Some(Ok(_)), Some(_), Some(_))
                | (Some(Ok(_)), None, Some(_)) => vec![CtxPar::new(doc, p)],
            }
        })
        .collect()
}

fn run_html_pipeline(x: impl PipelineInput) {
    let blocks = x.get_blocks();
    let blocks = dereference_blocks(blocks);
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
                l.map(|line| -> Result<BlockListEntry, Error> {
                    let (id, h) = split_block_line(line)?;
                    Ok(BlockListEntry::new(ParId(id), h))
                })
            })
            .collect::<Result<Result<Vec<BlockListEntry>, _>, _>>()??;
        Ok(BlockList::new(lines))
    }

    pub fn load_newest<T>(id: DocId, history_path: T, par_path: T) -> Result<Document, Error>
    where
        T: AsRef<Path> + Sync,
    {
        let lines = Self::load_lines(id, history_path)?;
        let pars = lines
            .entries
            .into_par_iter()
            .map(|be| {
                let p = DocParagraph::from_path(be.get_par_path(id));
                p
            })
            .collect::<Result<_, _>>()?;
        Ok(Document { id, pars })
    }

    pub fn gen_block_list(&self) -> BlockList {
        BlockList::new(
            self.pars
                .iter()
                .map(|p| BlockListEntry::new(p.id.clone(), p.t.clone()))
                .collect(),
        )
    }
}
