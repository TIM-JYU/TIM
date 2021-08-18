#![recursion_limit = "128"]

#[macro_use]
extern crate diesel;

use std::error::Error;

use actix_web::{get, web, HttpServer, ResponseError};
use actix_web::{App, HttpResponse};
use askama::Template;
use diesel::pg::PgConnection;
use diesel::r2d2;
use diesel::r2d2::ConnectionManager;
use rayon;

use crate::db::{get_item, get_items};
use crate::models::Item;
use crate::models::ItemKind;
use crate::models::ItemList;
use crate::settings::Settings;
use thiserror::Error;
use tim_core::document::document::DocumentStore;
use tim_core::document::processing::run_html_pipeline;
use tim_core::document::DocBlock;
use tim_core::document::DocId;
use tim_core::document::Document;

mod db;
pub mod docinfo;
mod models;
mod schema;
mod settings;

type DbPool = r2d2::Pool<ConnectionManager<PgConnection>>;

#[derive(Debug)]
enum PathSpec<'a> {
    Id(u32),
    Path(&'a str),
}

impl<'a> From<&'a str> for PathSpec<'a> {
    fn from(s: &'a str) -> Self {
        let i: Result<u32, _> = s.parse();
        match i {
            Ok(n) => PathSpec::Id(n),
            Err(_) => PathSpec::Path(s),
        }
    }
}

#[derive(Error, Debug)]
#[error("{source}")]
struct AnyhowWrap {
    #[from]
    source: anyhow::Error,
}

impl ResponseError for AnyhowWrap {
    fn error_response(&self) -> HttpResponse {
        let mut err = format!("{}", self);
        let mut cause = self.source();
        while let Some(f) = cause {
            err += &format!("\nCaused by: {}", f);
            cause = f.source();
        }
        HttpResponse::InternalServerError()
            .content_type("text/plain; charset=utf-8")
            .body(err)
    }
}

#[get("/view/{path}")]
async fn view_item_impl(
    pool: web::Data<DbPool>,
    path: web::Path<String>,
) -> Result<HttpResponse, actix_web::Error> {
    let conn = pool.get().expect("couldn't get db connection from pool");
    let res = web::block(move || get_item(&conn, &path)).await?;
    let conn2 = pool.get().expect("couldn't get db connection from pool");
    match res {
        ItemKind::Folder(f) => {
            let items = get_items(&conn2, &f.get_path()).map_err(AnyhowWrap::from)?;
            Ok(HttpResponse::Ok()
                .content_type("text/html; charset=utf-8")
                .body(
                    ItemList {
                        items: items.into_iter().map(|i| i.into()).collect(),
                    }
                    .render()
                    .unwrap(),
                ))
        }
        ItemKind::DocEntry(d) => {
            let d_id = DocId(d.id);
            let doc: Document<DocBlock> = Document::load_newest(d_id).map_err(AnyhowWrap::from)?;
            let doc_str = format!("{:#?}", run_html_pipeline(&mut DocumentStore::new(), doc));
            Ok(HttpResponse::Ok()
                .content_type("text/plain; charset=utf-8")
                .body(doc_str))
        }
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    rayon::ThreadPoolBuilder::new()
        .num_threads(8)
        .build_global()
        .unwrap();
    // Create connection pool
    let settings = Settings::new().expect("failed to load settings");
    let manager = ConnectionManager::<PgConnection>::new(settings.psql_address);
    let pool = r2d2::Pool::builder()
        .build(manager)
        .expect("Failed to create pool.");

    // Start HTTP server
    HttpServer::new(move || App::new().data(pool.clone()).service(view_item_impl))
        .bind("127.0.0.1:8080")?
        .run()
        .await
}
