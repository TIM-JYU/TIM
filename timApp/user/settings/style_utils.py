from pathlib import Path

from flask import current_app

from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo

static_folder = Path("static")
stylesheets_folder = static_folder / "stylesheets"

OFFICIAL_STYLES_PATH = "styles/official"
USER_STYLES_PATH = "styles/user"


def get_default_scss_gen_dir() -> Path:
    """Returns the default path into which generated SCSS files should be put into

    :return: Default path for generated SCSS files
    """
    return static_folder / current_app.config["SASS_GEN_PATH"]


def resolve_themes(short_names: list[str]) -> list[DocEntry]:
    return get_documents(
        filter_folder=OFFICIAL_STYLES_PATH,
        search_recursively=False,
        custom_filter=DocEntry.name.in_(
            [f"{OFFICIAL_STYLES_PATH}/{n}" for n in short_names]
        ),
    )


def is_style_doc(doc: DocInfo) -> bool:
    return (
        doc.path.startswith(OFFICIAL_STYLES_PATH)
        or doc.path.startswith(USER_STYLES_PATH)
        and doc.document.get_settings().get("description", None) is not None
    )
