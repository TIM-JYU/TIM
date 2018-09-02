use askama::Template;
use crate::document::DocInfo;

pub trait Item {
    fn get_path(&self) -> String;
    fn get_title(&self) -> &str;
}

#[derive(Template)]
#[template(path = "itemlist.html")]
pub struct ItemList {
    pub items: Vec<TItem>,
}

#[derive(Template, Queryable, Serialize)]
#[template(path = "folder.html")]
pub struct Folder {
    pub id: i32,
    pub name: String,
    pub location: String,
    pub title: Option<String>,
}

impl Item for Folder {
    fn get_path(&self) -> String {
        if self.location.is_empty() {
            self.name.clone()
        } else {
            format!("{}/{}", self.location, self.name)
        }
    }

    fn get_title(&self) -> &str {
        match self.title {
            Some(ref t) => &t,
            None => "Untitled",
        }
    }
}

#[derive(Template)]
#[template(path = "item.html")]
pub struct TItem {
    pub item: ItemKind,
}

impl From<ItemKind> for TItem {
    fn from(i: ItemKind) -> Self {
        TItem { item: i }
    }
}

pub enum ItemKind {
    Folder(Folder),
    DocEntry(DocInfo),
}

impl From<Folder> for ItemKind {
    fn from(f: Folder) -> Self {
        ItemKind::Folder(f)
    }
}

impl From<DocInfo> for ItemKind {
    fn from(f: DocInfo) -> Self {
        ItemKind::DocEntry(f)
    }
}

#[derive(Debug, PartialEq, DbEnum)]
#[PgType = "questionactivitykind"]
#[DieselType = "QuestionactivitykindMapping"]
pub enum Questionactivitykind {
    Pointsclosed,
    Pointsshown,
    Useranswered,
    Userextended,
    Usershown,
}

#[derive(Debug, PartialEq, DbEnum)]
#[PgType = "readparagraphtype"]
#[DieselType = "ReadparagraphtypeMapping"]
pub enum Readparagraphtype {
    Pointsclosed,
    Pointsshown,
    Useranswered,
    Userextended,
    Usershown,
}
