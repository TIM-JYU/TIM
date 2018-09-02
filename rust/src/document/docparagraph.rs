use crate::timerror::TimError;
use crate::timerror::TimErrorKind;
use failure::Error;
use failure::ResultExt;
use serde_json;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::path::Path;
use yaml_rust::Yaml;
use yaml_rust::YamlLoader;

#[derive(Deserialize, Debug)]
pub struct AttributeSet {
    classes: Option<Vec<String>>,
    plugin: Option<String>,
    #[serde(rename = "taskId")]
    task_id: Option<String>,
    settings: Option<String>,
    #[serde(flatten)]
    others: HashMap<String, String>,
}

#[derive(Deserialize, Debug)]
pub struct DocParagraph {
    id: String,
    md: String,
    t: String,
    attrs: AttributeSet,
}

impl DocParagraph {
    pub fn from_path(path: impl AsRef<Path>) -> Result<DocParagraph, Error> {
        info!("{:#?}", path.as_ref());
        let file = File::open(path)?;
        let p = serde_json::from_reader(file)?;
        Ok(p)
    }

    pub fn get_markdown(&self) -> &str {
        &self.md
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
