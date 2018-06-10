use actix::Actor;
use actix::Handler;
use actix::Message;
use actix::SyncContext;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use diesel::r2d2::{ConnectionManager, Pool};
use document::DocInfo;
use failure::ResultExt;
use models::Folder;
use models::ItemKind;
use schema;
use timerror::TimError;
use timerror::TimErrorKind;

pub struct DbExecutor(pub Pool<ConnectionManager<PgConnection>>);

impl Actor for DbExecutor {
    type Context = SyncContext<Self>;
}

pub struct GetItems {
    pub path: String,
}

impl Message for GetItems {
    type Result = Result<Vec<ItemKind>, TimError>;
}

impl Handler<GetItems> for DbExecutor {
    type Result = <GetItems as Message>::Result;

    fn handle(&mut self, msg: GetItems, _: &mut Self::Context) -> Self::Result {
        use self::schema::block::dsl::{block, description};
        use self::schema::docentry::dsl::{docentry, id as docid, name, public};
        use self::schema::folder::dsl::{
            folder, id as f_id, location as f_location, name as f_name,
        };
        let conn = &self.0.get().context(TimErrorKind::Db)?;
        let docs = if msg.path.is_empty() {
            docentry
                .inner_join(block)
                .filter(name.not_like("%/%"))
                .select((name, docid, public, description))
                .load::<DocInfo>(conn)
                .context(TimErrorKind::Db)?
        } else {
            docentry
                .inner_join(block)
                .filter(
                    name.like(format!("{}/%", msg.path))
                        .and(name.not_like(format!("{}/%/%", msg.path))),
                )
                .select((name, docid, public, description))
                .load::<DocInfo>(conn)
                .context(TimErrorKind::Db)?
        };
        let folders = folder
            .inner_join(block)
            .filter(f_location.eq(msg.path))
            .select((f_id, f_name, f_location, description))
            .load::<Folder>(conn)
            .context(TimErrorKind::Db)?;
        let mut result: Vec<ItemKind> = vec![];
        result.extend(docs.into_iter().map(|d| d.into()));
        result.extend(folders.into_iter().map(|d| d.into()));
        Ok(result)
    }
}

pub struct GetItem {
    pub path: String,
}

impl Message for GetItem {
    type Result = Result<ItemKind, TimError>;
}

impl Handler<GetItem> for DbExecutor {
    type Result = <GetItem as Message>::Result;

    fn handle(&mut self, msg: GetItem, _: &mut Self::Context) -> Self::Result {
        use self::schema::block::dsl::{block, description};
        use self::schema::docentry::dsl::{docentry, id as docid, name as docname, public};
        use self::schema::folder::dsl::{
            folder, id as f_id, location as f_location, name as f_name,
        };
        if msg.path == "" {
            return Ok(Folder {
                id: -1,
                location: "".to_string(),
                name: "".to_string(),
                title: Some("All documents".to_string()),
            }.into());
        }
        let conn = &self.0.get().context(TimErrorKind::Db)?;
        let d = docentry
            .inner_join(block)
            .filter(docname.eq(&msg.path))
            .select((docname, docid, public, description))
            .first::<DocInfo>(conn)
            .optional()
            .context(TimErrorKind::Db)?;
        match d {
            Some(e) => Ok(ItemKind::DocEntry(e)),
            None => {
                let path_parts: Vec<_> = msg.path.rsplitn(2, "/").collect();
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
                        .context(TimErrorKind::ItemNotFound)?,
                ))
            }
        }
    }
}
