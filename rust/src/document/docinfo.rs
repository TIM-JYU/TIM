use askama::Template;
use crate::models::Item;

#[derive(Template, Queryable, Serialize)]
#[template(path = "docentry.html")]
pub struct DocInfo {
    pub path: String,
    pub id: i32,
    pub public: bool,
    pub title: Option<String>,
}

impl Item for DocInfo {
    fn get_path(&self) -> String {
        self.path.clone()
    }

    fn get_title(&self) -> &str {
        match self.title {
            Some(ref t) => &t,
            None => "Untitled",
        }
    }
}
