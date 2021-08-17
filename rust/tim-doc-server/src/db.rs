use diesel::pg::PgConnection;
use diesel::prelude::*;

use crate::document::DocInfo;
use crate::models::Folder;
use crate::models::ItemKind;
use crate::schema;
use crate::timerror::TimError;
use anyhow::Context;

pub fn get_items(db: &PgConnection, path: &str) -> Result<Vec<ItemKind>, TimError> {
    use self::schema::block::dsl::{block, description};
    use self::schema::docentry::dsl::{docentry, id as docid, name, public};
    use self::schema::folder::dsl::{
        folder, id as f_id, location as f_location, name as f_name,
    };
    let conn = db;
    let docs = if path.is_empty() {
        docentry
            .inner_join(block)
            .filter(name.not_like("%/%"))
            .select((name, docid, public, description))
            .load::<DocInfo>(conn)
            .map_err(|_| TimError::Db)?
    } else {
        docentry
            .inner_join(block)
            .filter(
                name.like(format!("{}/%", path))
                    .and(name.not_like(format!("{}/%/%", path))),
            )
            .select((name, docid, public, description))
            .load::<DocInfo>(conn)
            .map_err(|_| TimError::Db)?
    };
    let folders = folder
        .inner_join(block)
        .filter(f_location.eq(path))
        .select((f_id, f_name, f_location, description))
        .load::<Folder>(conn)
        .map_err(|_| TimError::Db)?;
    let mut result: Vec<ItemKind> = vec![];
    result.extend(docs.into_iter().map(|d| d.into()));
    result.extend(folders.into_iter().map(|d| d.into()));
    Ok(result)
}

pub fn get_item(db: &PgConnection, path: &str) -> Result<ItemKind, anyhow::Error> {
    use self::schema::block::dsl::{block, description};
    use self::schema::docentry::dsl::{docentry, id as docid, name as docname, public};
    use self::schema::folder::dsl::{
        folder, id as f_id, location as f_location, name as f_name,
    };
    if path == "" {
        return Ok(Folder {
            id: -1,
            location: "".to_string(),
            name: "".to_string(),
            title: Some("All documents".to_string()),
        }
            .into());
    }
    let conn = db;
    let d = docentry
        .inner_join(block)
        .filter(docname.eq(&path))
        .select((docname, docid, public, description))
        .first::<DocInfo>(conn)
        .optional()
        .context(TimError::Db)?;
    match d {
        Some(e) => Ok(ItemKind::DocEntry(e)),
        None => {
            let path_parts: Vec<_> = path.rsplitn(2, "/").collect();
            let msg_name = path_parts[0];
            let msg_location = if path_parts.len() == 2 {
                path_parts[1]
            } else {
                ""
            };
            Ok(ItemKind::Folder(
                folder
                    .inner_join(block)
                    .filter(f_name.eq(msg_name).and(f_location.eq(msg_location)))
                    .select((f_id, f_name, f_location, description))
                    .first::<Folder>(conn)
                    .context(TimError::ItemNotFound)?,
            ))
        }
    }
}
