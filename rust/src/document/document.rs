use crate::document::DocParagraph;
use crate::timerror::TimErrorKind;
use failure::Error;
use failure::ResultExt;
use serde_json;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use rayon::prelude::*;

pub enum Reference {
    Par {
        dest_id: String,
        dest_doc_id: i32,
        par: DocParagraph,
    },
    Area {
        name: String,
        dest_doc_id: i32,
        par: DocParagraph,
    },
}

pub struct Area(Vec<DocPart>);

pub enum DocPart {
    Par(DocParagraph),
    Reference(Reference),
    Area(Area),
}

#[derive(Debug)]
pub struct Document<T=DocParagraph> {
    id: i32,
    pars: Vec<T>,
}

#[derive(Deserialize, Debug)]
pub struct OpParams {
    old_hash: Option<String>,
    new_hash: Option<String>,
    before_id: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct ChangeLogEntry {
    group_id: i32,
    par_id: String,
    op: String, // TODO
    op_params: Option<OpParams>,
    pub ver: [i32; 2],
    time: String, // TODO
}

impl Document {
    pub fn load_newest<T>(
        id: i32,
        history_path: T,
        par_path: T,
    ) -> Result<Document<DocParagraph>, Error> where T: AsRef<Path> + Sync {
        let changelog = File::open(history_path.as_ref().join("changelog"))
            .context(TimErrorKind::NonExistentOrEmptyDocument)?;
        let mut file = BufReader::new(&changelog);
        let mut s = String::new();
        file.read_line(&mut s)?;
        let p: ChangeLogEntry = serde_json::from_str(&s)?;
        let version_file_path = history_path
            .as_ref()
            .join(format!("{}/{}", p.ver[0], p.ver[1]));
        let cfile = File::open(version_file_path)?;
        let cfiler = BufReader::new(&cfile);
        let pars = cfiler.lines().collect::<Vec<_>>().into_iter().map( |l| {
            let unwrapped = l.unwrap();
            let v: Vec<_> = unwrapped.splitn(2, '/').collect();
            let p = DocParagraph::from_path(par_path.as_ref().join(v[0]).join(v[1])).unwrap();
            p
        }).collect();
        Ok(Document { id, pars })
    }
}
