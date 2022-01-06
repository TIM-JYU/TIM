from pathlib import Path

from flask import current_app

from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo

static_folder = Path("static")
stylesheets_folder = static_folder / "stylesheets"

STYLES_FOLDER_PREFIX = "styles"
OFFICIAL_STYLES_PATH = f"{STYLES_FOLDER_PREFIX}/official"
USER_STYLES_PATH = f"{STYLES_FOLDER_PREFIX}/user"


def get_default_scss_gen_dir() -> Path:
    """Returns the default path into which generated SCSS files should be put into

    :return: Default path for generated SCSS files
    """
    return static_folder / current_app.config["SASS_GEN_PATH"]


def resolve_themes(short_names: list[str]) -> list[DocEntry]:
    """Resolves theme documents from short names or document paths.

    .. note:: Style documents are resolved even if the user has no permission to access it.
    This is done because SCSS generation only preserves SCSS blocks and not other possibly sensitive content.

    :param short_names: List of either official theme names or full paths to the style documents.
    :return: List of valid style documents found based on short names.
    """

    official_names = [short_name for short_name in short_names if "/" not in short_name]
    # NOTE: We also resolve full docs, but it is okay as style processing only preserves SCSS blocks
    full_names = [
        short_name[1:] if short_name.startswith("/") else short_name
        for short_name in short_names
        if "/" in short_name
    ]

    docs = get_documents(
        search_recursively=False,
        custom_filter=(
            DocEntry.name.in_([f"{OFFICIAL_STYLES_PATH}/{n}" for n in short_names])
            | DocEntry.name.in_(full_names)
        ),
    )

    # Only documents marked as styles
    return [d for d in docs if d.document.get_settings().is_style_document()]


def is_style_doc(doc: DocInfo) -> bool:
    """Checks whether the given document is an official or user-made style.

    :param doc: Document to check
    :return: True if document is a valid official or user style. Otherwise, False.
    """
    return (
        doc.path.startswith(OFFICIAL_STYLES_PATH)
        or doc.path.startswith(USER_STYLES_PATH)
        and doc.document.get_settings().is_style_document()
    )
