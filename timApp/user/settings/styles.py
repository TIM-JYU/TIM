from dataclasses import field
from os.path import getmtime
from pathlib import Path
from typing import Optional

import sass
from flask import Response, current_app, render_template_string, flash

from timApp.auth.accesshelper import verify_logged_in, verify_view_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.caching import get_style_timestamp_hash, set_style_timestamp_hash
from timApp.document.docentry import get_documents, DocEntry
from timApp.document.randutils import hashfunc
from timApp.user.settings.style_utils import (
    stylesheets_folder,
    get_default_scss_gen_dir,
    OFFICIAL_STYLES_PATH,
    USER_STYLES_PATH,
)
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, text_response
from timApp.util.flask.typedblueprint import TypedBlueprint

styles = TypedBlueprint("styles", __name__, url_prefix="/styles")


def hash_theme_timestamps(theme_docs: Optional[list[DocEntry]] = None) -> str:
    """Computes a hash of theme documents based on the last time they were modified.

    The hash can be used to determine if the style containing the themes needs to be regenerated.

    :param theme_docs: List of theme documents with SCSS to generate style from.
    :return: A unique hash based on last modification time of the global styles and theme documents.
    """
    h = ""
    if theme_docs:
        for theme in theme_docs:
            h += str(theme.last_modified.timestamp())
    for p in stylesheets_folder.glob("*.scss"):
        h += str(getmtime(p))
    return hashfunc(h)


def get_style_name(theme_docs: list[DocEntry]) -> str:
    """Computes the name of the style to generate.

    The name is guaranteed to be unique for the given set of style documents.

    :param theme_docs: List of theme documents with SCSS to generate style from.
    :return: Name of the style or "default" if no documents are given.
    """
    # Order matters as user can set style priority
    if not theme_docs:
        return "default"
    m = ""
    for theme in theme_docs:
        m += f"{theme.id}"
    return f"compiled-style-{hashfunc(m)}"


def generate_style(
    theme_docs: list[DocEntry], gen_dir: Optional[Path] = None
) -> tuple[str, Path]:
    """Generates a CSS style based on the given theme documents.

    .. note::
       This function sorts the given theme list.

    The structure of the generated SCSS file is as follows:

    1. Charset declaration (always UTF-8)
    2. Import default values for variables (from variables.scss)
    3. For each theme:

      1. Declare an empty mixin whose name is the same as the theme
      2. Import the theme's SCSS file. This may contain a mixin of the same name which will override
         the empty one.

    4. Import all.scss that contains all generic SCSS files
    5. For each theme:

      1. Include the theme mixin in root scope. This trick allows the theme file to override any
         generic CSS.

    :param theme_docs: List of theme documents with SCSS to generate style from.
    :param gen_dir: Directory to put the generated theme in. If not specified, default SCSS cache dir is used.
    :return: Tuple of (style_name, style_path)
    """
    from timApp.printing.print import print_doc_scss

    if not gen_dir:
        gen_dir = get_default_scss_gen_dir()

    style_name = get_style_name(theme_docs)
    style_path = gen_dir / f"{style_name}.css"

    current_style_hash = hash_theme_timestamps(theme_docs)
    last_style_hash = get_style_timestamp_hash(style_name)
    style_exits = style_path.exists()

    if style_exits and last_style_hash == current_style_hash:
        return style_name, style_path
    gen_dir.mkdir(exist_ok=True)

    theme_paths = []
    theme_doc_map = {}
    for theme_doc in theme_docs:
        theme_path = print_doc_scss(theme_doc)
        theme_paths.append((theme_doc.id, theme_path))
        theme_doc_map[f"../..{theme_path}"] = theme_doc.path

    scss_src = render_template_string(
        """
@charset "UTF-8";
@import "stylesheets/varUtils";
@import "stylesheets/variables";
{% for theme_id, theme in themes %}
@mixin post-all-{{ theme_id }} {};
@import "{{ theme }}";
{% endfor %}
@include export-variables;
@import "stylesheets/all.scss";
{% for theme_id, _ in themes %}
@include post-all-{{ theme_id }};
{% endfor %}
""",
        themes=theme_paths,
    )

    try:
        compiled_sass = sass.compile(
            string=scss_src,
            output_style=current_app.config["LIBSASS_STYLE"],
            include_paths=current_app.config["LIBSASS_INCLUDES"],
        )
        with style_path.open(encoding="utf-8", mode="w") as f:
            f.write(compiled_sass)
    except sass.CompileError as e:
        error_message = str(e)
        for rel_path, doc_path in theme_doc_map.items():
            error_message = error_message.replace(rel_path, doc_path)
        flash(
            f"""
<p>Error in current style:</p>
<pre>{error_message}</pre>
<p>Please change style or fix the issue</p>
"""
        )
        return style_name if style_exits else "default", style_path

    set_style_timestamp_hash(style_name, current_style_hash)
    return style_name, style_path


@styles.get("")
def get_styles() -> Response:
    candidates = get_documents(
        filter_user=get_current_user_object(),
        custom_filter=(
            DocEntry.name.like(f"{OFFICIAL_STYLES_PATH}%")
            | DocEntry.name.like(f"{USER_STYLES_PATH}%")
        ),
    )

    result = []
    for doc in candidates:
        style_description = doc.document.get_settings().get("description", None)
        if not style_description:
            continue
        result.append(
            {
                "docId": doc.id,
                "name": doc.title,
                "path": doc.name,
                "description": style_description,
            }
        )

    return json_response(result)


@styles.get("/<path:doc_path>.scss")
def get_raw_scss(doc_path: str, force: bool = False) -> Response:
    from timApp.printing.print import print_doc_scss

    verify_logged_in()
    doc = DocEntry.find_by_path(f"styles/{doc_path}")
    if not doc:
        raise NotExist()

    verify_view_access(doc)
    scss_path = print_doc_scss(doc, force)

    with open(scss_path, "r", encoding="utf-8") as f:
        return Response(f.read(), content_type="text/plain")


@styles.get("/path")
def generate(docs: list[int] = field(default_factory=list)) -> Response:
    verify_logged_in()

    doc_entries: list[DocEntry] = DocEntry.query.filter(DocEntry.id.in_(docs)).all()

    for doc in doc_entries:
        verify_view_access(doc)

        if not doc.path.startswith(OFFICIAL_STYLES_PATH) and not doc.path.startswith(
            USER_STYLES_PATH
        ):
            raise RouteException(
                f"Document {doc.id} ({doc.path}) must be in {OFFICIAL_STYLES_PATH} or {USER_STYLES_PATH}"
            )

    _, style_path = generate_style(doc_entries)

    return text_response(str(style_path))
