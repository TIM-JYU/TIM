#![feature(box_patterns)]
#![feature(async_await)]
#![recursion_limit = "128"]

#[macro_use]
extern crate diesel;

mod db;
mod document;
mod models;
mod schema;
mod settings;
mod timerror;

use actix::Addr;
use actix::SyncArbiter;
use actix_web::pred;
use actix_web::ResponseError;
use actix_web::{http, server, App, HttpRequest, HttpResponse, Path};
use askama::Template;
use crate::db::DbExecutor;
use crate::db::GetItem;
use crate::db::GetItems;
use crate::document::document::DocumentStore;
use crate::document::processing::run_html_pipeline;
use crate::document::DocBlock;
use crate::document::DocId;
use crate::document::Document;
use crate::models::Item;
use crate::models::ItemKind;
use crate::models::ItemList;
use crate::settings::Settings;
use crate::timerror::TimError;
use crate::timerror::TimErrorKind;
use diesel::pg::PgConnection;
use diesel::r2d2::{ConnectionManager, Pool};
use failure::Fail;
use failure::ResultExt;
use futures::executor::block_on;
use rayon;
use futures::compat::Future01CompatExt;

fn view_document(info: Path<(u32,)>) -> String {
    format!("Hello {}!", &info.0)
}

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

impl ResponseError for TimError {
    fn error_response(&self) -> HttpResponse {
        let mut err = format!("{}", self);
        let mut cause = self.cause();
        while let Some(ref f) = cause {
            err += &format!("\nCaused by: {}", f);
            cause = f.cause();
        }
        HttpResponse::InternalServerError()
            .content_type("text/plain; charset=utf-8")
            .body(err)
    }
}

fn view_item(req: &HttpRequest<AppState>) -> Result<HttpResponse, TimError> {
    block_on(view_item_impl(req))
}

async fn view_item_impl(req: &HttpRequest<AppState>) -> Result<HttpResponse, TimError> {
    let info = &req.match_info()["Path"];
    let state = req.state();
    let ps: PathSpec = (&info[..]).into();
    let path: String;
    match ps {
        PathSpec::Id(_) => return Ok(HttpResponse::Ok().body("not supported by id yet")),
        PathSpec::Path(p) => path = p.to_string(),
    }
    let res = (state
        .db
        .send(GetItem {
            path: path.to_string()
        })).compat().await
    .context(TimErrorKind::Db)??;
    match res {
        ItemKind::Folder(f) => {
            let items = (state.db.send(GetItems { path: f.get_path() })).compat().await
                .context(TimErrorKind::Db)??;
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
            let doc: Document<DocBlock> =
                Document::load_newest(d_id).context(TimErrorKind::DocumentLoad)?;
            let doc_str = format!("{:#?}", run_html_pipeline(&mut DocumentStore::new(), doc));
            Ok(HttpResponse::Ok()
                .content_type("text/plain; charset=utf-8")
                .body(doc_str))
        }
    }
}

fn not_found(_info: &HttpRequest<AppState>) -> &'static str {
    "not found!"
}

struct AppState {
    db: Addr<DbExecutor>,
}

fn main() -> Result<(), failure::Error> {
    // simple_logger::init().unwrap();
    rayon::ThreadPoolBuilder::new()
        .num_threads(8)
        .build_global()
        .unwrap();
    let sys = actix::System::new("tim");
    let settings = Settings::new()?;
    let manager = ConnectionManager::<PgConnection>::new(settings.psql_address);
    let pool = Pool::builder()
        .build(manager)
        .expect("Failed to create pool.");
    let addr = SyncArbiter::start(3, move || DbExecutor(pool.clone()));
    server::new(move || {
        App::with_state(AppState { db: addr.clone() })
            .resource("/view/{Path:.*}", |r| {
                r.method(http::Method::GET).f(view_item)
            })
            .route("/{id}", http::Method::GET, view_document)
            .default_resource(|r| {
                r.method(http::Method::GET).f(not_found);
                r.route()
                    .filter(pred::Not(pred::Get()))
                    .f(|_req| HttpResponse::MethodNotAllowed());
            })
    })
    .bind(settings.bind)
    .unwrap()
    .start();
    println!("server running");
    let _ = sys.run();
    Ok(())
}
