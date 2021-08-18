use crate::document::docblock::AttributeContainer;
use crate::document::docblock::BlockIdLike;
use crate::document::BlockId;
use crate::document::DocBlock;
use crate::timerror::TimError;
use anyhow::Context;
use chrono::prelude::*;
use indexmap::IndexMap;
use rayon::iter::IntoParallelIterator;
use rayon::prelude::*;
use serde_derive::Deserialize;
use serde_json;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::PathBuf;

pub trait BlockPath {
    fn get_block_path(&self, d: DocId) -> PathBuf;
}

impl<'a, T> BlockPath for (&'a T, &'a str)
where
    T: BlockIdLike,
{
    fn get_block_path(&self, d: DocId) -> PathBuf {
        get_block_path(self.0, &self.1, d)
    }
}

fn get_block_path<T: BlockIdLike>(b: &T, h: &str, d: DocId) -> PathBuf {
    d.get_blocks_path().join(b.get_str()).join(h)
}

impl<'a, T> BlockPath for (T, &'a String)
where
    T: BlockIdLike,
{
    fn get_block_path(&self, d: DocId) -> PathBuf {
        let (id, hash) = self;
        (id, &***hash).get_block_path(d)
    }
}

pub struct BlockList {
    pub entries: IndexMap<BlockId, String>,
}

impl BlockList {
    pub fn new(entries: IndexMap<BlockId, String>) -> Self {
        BlockList { entries }
    }

    #[allow(dead_code)]
    pub fn get_entry(&self, p: &BlockId) -> Option<&str> {
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

fn get_area<T>(result: &mut Vec<T>, blocks: &mut impl Iterator<Item = T>, area_name: &str)
where
    T: AttributeContainer,
{
    for p in blocks {
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

impl DocumentStore {
    fn load_document(&mut self, id: DocId) -> Result<&Document, anyhow::Error> {
        match self.docs.entry(id) {
            Entry::Occupied(o) => Ok(o.into_mut()),
            Entry::Vacant(v) => {
                let doc = Document::load_newest(id)?;
                Ok(v.insert(doc))
            }
        }
    }

    pub fn load_blocklist(&mut self, id: DocId) -> Result<&BlockList, anyhow::Error> {
        match self.block_lists.entry(id) {
            Entry::Occupied(o) => Ok(o.into_mut()),
            Entry::Vacant(v) => {
                let doc = Document::load_lines(id)?;
                Ok(v.insert(doc))
            }
        }
    }

    pub fn load_area(
        &mut self,
        id: DocId,
        area_name: &str,
    ) -> Result<Vec<DocBlock>, anyhow::Error> {
        let doc = self.load_document(id)?;
        let mut blocks = Vec::new();
        get_area(
            &mut blocks,
            &mut doc
                .blocks
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
        Ok(blocks)
    }

    pub fn new() -> Self {
        DocumentStore {
            docs: HashMap::new(),
            block_lists: HashMap::new(),
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
pub struct DocId(pub i32);

impl DocId {
    pub fn get_docs_path(&self) -> PathBuf {
        format!("../timApp/tim_files/docs/{}", self.0).into()
    }
    pub fn get_blocks_path(&self) -> PathBuf {
        format!("../timApp/tim_files/pars/{}", self.0).into()
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum DocRef {
    Block(DocId, BlockId),
    Area(DocId, String),
}

#[derive(Debug)]
pub struct Document<T = DocBlock> {
    pub id: DocId,
    pub blocks: Vec<T>,
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
    par_id: BlockId,
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

fn split_block_line(mut l: String) -> Result<(String, String), anyhow::Error> {
    let index = l.find('/').ok_or(TimError::DocumentLoad)?;
    let mut v = l.split_off(index);
    v.drain(..1);
    Ok((l, v))
}

impl Document {
    pub fn load_lines(id: DocId) -> Result<BlockList, anyhow::Error> {
        let history_path = id.get_docs_path();
        let changelog = File::open(history_path.join("changelog"))
            .context(TimError::NonExistentOrEmptyDocument)?;
        let mut file = BufReader::new(&changelog);
        let mut s = String::new();
        file.read_line(&mut s)?;
        let p: ChangeLogEntry = serde_json::from_str(&s)?;
        let version_file_path = history_path.join(format!("{}/{}", p.ver[0], p.ver[1]));
        let cfile = File::open(version_file_path).context("Failed to open block list file")?;
        let cfiler = BufReader::new(&cfile);
        let lines = cfiler
            .lines()
            .map(|l| {
                l.map(|line| -> Result<(BlockId, String), anyhow::Error> {
                    let (id, h) = split_block_line(line)?;
                    Ok((BlockId(id), h))
                })
            })
            .collect::<Result<Result<_, _>, _>>()??;
        Ok(BlockList::new(lines))
    }

    pub fn load_newest(id: DocId) -> Result<Document, anyhow::Error> {
        let lines = Self::load_lines(id)?;
        let blocks = lines
            .entries
            .into_par_iter()
            .map(|(blockid, hash)| {
                let p = DocBlock::from_path(get_block_path(&blockid, &hash, id));
                p
            })
            .collect::<Result<_, _>>()?;
        Ok(Document { id, blocks })
    }

    #[allow(dead_code)]
    pub fn gen_block_list(&self) -> BlockList {
        BlockList::new(
            self.blocks
                .iter()
                .map(|p| (p.id.clone(), p.t.clone()))
                .collect(),
        )
    }
}
