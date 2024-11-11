from dataclasses import dataclass
from pathlib import Path

from flask import current_app, has_request_context, request

from timApp.auth.get_user_rights_for_item import UserItemRights
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.document.viewcontext import ViewContext
from timApp.user.preferences import Preferences

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


@dataclass(slots=True, frozen=True)
class StyleForUserContext:
    """
    Context for generating the style for the user.
    Used only when generating a style for a specific document view since documents can override user themes.
    """

    view_ctx: ViewContext
    """View context for the document"""
    user_rights: UserItemRights
    """User rights for the document"""
    doc_info: DocInfo
    """Document information"""


def get_style_for_user(
    current_user_prefs: Preferences, context: StyleForUserContext | None = None
) -> str:
    """
    Generates the style path for the user based on their preferences and the context.

    :param current_user_prefs: User preferences
    :param context: Context for the style generation
    :return: URL path to the generated style
    """
    from timApp.item.routes import is_exam_mode
    from timApp.user.settings.styles import generate_style

    themes = current_user_prefs.theme_docs()

    if context:
        doc_settings = context.doc_info.document.get_settings()
        document_themes = doc_settings.themes()
        exam_mode = is_exam_mode(doc_settings, context.user_rights)
        if exam_mode:
            document_themes = list(
                dict.fromkeys(doc_settings.exam_mode_themes() + document_themes)
            )
        document_themes_final = []
        for theme in document_themes:
            parts = theme.split(":", 1)
            if len(parts) == 2:
                view_route, theme = parts
                if not theme:
                    continue
                if view_route and context.view_ctx.route.value != view_route:
                    continue
            document_themes_final.append(theme)

        if document_themes_final:
            document_theme_docs = resolve_themes(document_themes_final)
            # If the user themes are not overridden, they are merged with document themes
            if doc_settings.override_user_themes():
                themes = document_theme_docs
            else:
                themes = list(
                    (
                        {d.id: d for d in document_theme_docs}
                        | {d.id: d for d in themes}
                    ).values()
                )

    if has_request_context():
        request_themes = [
            ts for t in request.args.get("themes", "").split(",") if (ts := t.strip())
        ]
        if request_themes:
            resolved_request_themes = resolve_themes(request_themes)
            themes = list(
                (
                    {d.id: d for d in themes}
                    | {d.id: d for d in resolved_request_themes}
                ).values()
            )

    style_path, style_hash = generate_style(themes)
    return f"{style_path}?{style_hash}"
