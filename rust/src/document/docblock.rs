use crate::timerror::TimError;
use crate::timerror::TimErrorKind;
use failure::Error;
use failure::ResultExt;
use serde_derive::Deserialize;
use serde_json;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use tera::{Context, Tera};
use yaml_rust::Yaml;
use yaml_rust::YamlLoader;

#[derive(Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub struct BlockId(pub String);

pub struct BlockIdRef<'a>(pub &'a str);

impl<'a> From<&'a str> for BlockIdRef<'a> {
    fn from(s: &'a str) -> Self {
        BlockIdRef(s)
    }
}

pub trait BlockIdLike {
    fn get_str(&self) -> &str;
}

impl Borrow<str> for BlockId {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl<'a> Borrow<str> for BlockIdRef<'a> {
    fn borrow(&self) -> &str {
        self.0
    }
}

impl BlockIdLike for BlockId {
    fn get_str(&self) -> &str {
        &self.0
    }
}

impl<'a> BlockIdLike for &'a BlockId {
    fn get_str(&self) -> &str {
        &self.0
    }
}

impl<'a> BlockIdLike for BlockIdRef<'a> {
    fn get_str(&self) -> &str {
        self.0
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct AttributeSet {
    classes: Option<Vec<String>>,
    #[serde(flatten)]
    pub others: HashMap<String, String>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct DocBlock {
    pub id: BlockId,
    md: String,
    pub t: String,
    pub attrs: AttributeSet,
}

impl DocBlock {
    pub fn from_path(path: impl AsRef<Path>) -> Result<DocBlock, Error> {
        let mut file = File::open(path).context("Failed to open DocParagraph file")?;
        let mut contents = String::new();
        //let mut contents = String::from(r#"{"id": "rehjt804ij043g", "md": "cat is brown", "t": "84u5945t954jt", "attrs": {}}"#);
        file.read_to_string(&mut contents)?;
        let p = serde_json::from_str(&contents)?;
        Ok(p)
    }

    pub fn plain_string(path: impl AsRef<Path>) -> Result<String, Error> {
        let mut file = File::open(path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(contents)
    }

    pub fn get_markdown(&self) -> &str {
        &self.md
    }

    pub fn get_expanded_markdown(self, macros: &Context) -> DocBlock {
        let md =
            Tera::one_off(&self.md, macros, false).unwrap_or("FAILED TO PROCESS MACROS".to_owned());
        DocBlock {
            id: self.id,
            md,
            t: self.t,
            attrs: self.attrs,
        }
    }
}

impl TryFrom<DocBlock> for Yaml {
    type Error = TimError;

    fn try_from(p: DocBlock) -> Result<Self, Self::Error> {
        let mut r =
            YamlLoader::load_from_str(p.get_markdown()).context(TimErrorKind::InvalidYaml)?;
        let mut drain = r.drain(..);
        drain.next().ok_or(TimErrorKind::InvalidYaml.into())
    }
}

pub trait AttributeContainer {
    fn get_attr(&self, k: &str) -> Option<&String>;
}

impl AttributeContainer for DocBlock {
    fn get_attr(&self, k: &str) -> Option<&String> {
        self.attrs.others.get(k)
    }
}
