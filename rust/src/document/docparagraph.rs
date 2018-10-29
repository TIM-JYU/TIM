use crate::timerror::TimError;
use crate::timerror::TimErrorKind;
use failure::Error;
use failure::ResultExt;
use serde_derive::Deserialize;
use serde_json;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use tera::{Context, Tera};
use yaml_rust::Yaml;
use yaml_rust::YamlLoader;

#[derive(Deserialize, Debug, Clone)]
pub struct ParId(pub String);

#[derive(Deserialize, Debug)]
pub struct AttributeSet {
    classes: Option<Vec<String>>,
    plugin: Option<String>,
    #[serde(rename = "taskId")]
    task_id: Option<String>,
    settings: Option<String>,
    pub ra: Option<String>,
    pub rd: Option<String>,
    pub rp: Option<String>,
    pub rt: Option<String>,
    #[serde(flatten)]
    pub others: HashMap<String, String>,
}

#[derive(Deserialize, Debug)]
pub struct DocParagraph {
    pub id: ParId,
    md: String,
    pub t: String,
    pub attrs: AttributeSet,
}

impl DocParagraph {
    pub fn from_path(path: impl AsRef<Path>) -> Result<DocParagraph, Error> {
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

    pub fn get_expanded_markdown(self, macros: &Context) -> DocParagraph {
        let md =
            Tera::one_off(&self.md, macros, false).unwrap_or("FAILED TO PROCESS MACROS".to_owned());
        DocParagraph {
            id: self.id,
            md,
            t: self.t,
            attrs: self.attrs,
        }
    }
}

impl TryFrom<DocParagraph> for Yaml {
    type Error = TimError;

    fn try_from(p: DocParagraph) -> Result<Self, Self::Error> {
        let mut r =
            YamlLoader::load_from_str(p.get_markdown()).context(TimErrorKind::InvalidYaml)?;
        let mut drain = r.drain(..);
        drain.next().ok_or(TimErrorKind::InvalidYaml.into())
    }
}
