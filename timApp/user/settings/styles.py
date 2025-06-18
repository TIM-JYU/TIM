from dataclasses import field, dataclass
from io import StringIO
from os.path import getmtime
from pathlib import Path
from typing import Any

import sass
from flask import Response, current_app, flash
from sqlalchemy import select

from timApp.auth.accesshelper import verify_logged_in, verify_view_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.caching import get_style_timestamp_hash, set_style_timestamp_hash
from timApp.document.docentry import get_documents, DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.document import dereference_pars
from timApp.document.post_process import process_areas
from timApp.document.preloadoption import PreloadOption
from timApp.document.randutils import hashfunc
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.item.partitioning import get_doc_version_hash
from timApp.timdb.sqa import run_sql
from timApp.user.settings.style_utils import (
    stylesheets_folder,
    get_default_scss_gen_dir,
    OFFICIAL_STYLES_PATH,
    USER_STYLES_PATH,
)
from timApp.user.special_group_names import ANONYMOUS_USERNAME
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, text_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import cache_folder_path
from tim_common.utils import render_raw_template_string
from tim_common.html_sanitize import sanitize_html

styles = TypedBlueprint("styles", __name__, url_prefix="/styles")
DEFAULT_STYLE_NAME = "default"

scss_cache_path = cache_folder_path / "generated_scss"


@dataclass
class StyleCompileException(Exception):
    compile_error: str


def generate_scss(doc: DocInfo, force: bool = False) -> str:
    """Generates SCSS from the given document.

    .. note:: Only SCSS code blocks are included in the final result.

    :param doc: Document to generate SCSS style from.
    :param force: If True, SCSS is always generated from the document. Otherwise, the latest cached SCSS is used if
                    the document hasn't changed.
    :return: Generated SCSS file
    """
    doc_hash = get_doc_version_hash(doc)
    cache_path = scss_cache_path / str(doc.id) / f"{doc_hash}.scss"

    if cache_path.exists() and not force:
        return cache_path.as_posix()

    cache_path.parent.mkdir(parents=True, exist_ok=True)

    view_ctx = default_view_ctx

    pars = doc.document.get_paragraphs(include_preamble=True)
    doc.document.preload_option = PreloadOption.all
    pars = dereference_pars(pars, context_doc=doc.document, view_ctx=view_ctx)

    anon_user = User.get_by_name(ANONYMOUS_USERNAME)
    assert anon_user is not None
    user_ctx = UserContext.from_one_user(anon_user)
    settings = doc.document.get_settings()
    macro_info = settings.get_macroinfo(view_ctx, user_ctx)
    macros = macro_info.get_macros()
    delimiter = macro_info.get_macro_delimiter()
    macro_env = macro_info.jinja_env

    # Process to resolve invisible paragraphs
    par_ids = {
        p.target_data.id
        for p in process_areas(
            settings,
            pars,
            macros,
            delimiter,
            macro_env,
            view_ctx,
            use_md=True,
            cache=False,
        )
    }

    with cache_path.open("w", encoding="utf-8") as f:
        for par in pars:
            if par.is_setting():
                continue

            if not par.is_theme_style():
                continue

            if par.id not in par_ids:
                continue

            # Style blocks are not plugins so we can skip pluginifying and just get expanded markdown
            md = par.get_expanded_markdown(macro_info, ignore_errors=True)

            # Remove the code marks
            if md.startswith("```"):
                style_start = md.find("\n")
                md = md[style_start + 1 : -3]
            # Replace hooks with unique ID for style generation
            md = md.replace("@mixin post-all", f"@mixin post-all-{doc.id}")
            md = md.replace("@mixin post-variables", f"@mixin post-variables-{doc.id}")

            f.write(f"{md}\n")

    return cache_path.as_posix()


def hash_theme_timestamps(theme_docs: list[DocEntry] | None = None) -> str:
    """Computes a hash of theme documents based on the last time they were modified.

    The hash can be used to determine if the style containing the themes needs to be regenerated.

    :param theme_docs: List of theme documents with SCSS to generate style from.
    :return: A unique hash based on last modification time of the global styles and theme documents.
    """
    h = ""
    if theme_docs:
        for theme in theme_docs:
            h += str(theme.id)
            h += str(theme.document.get_version())
            for preamble in theme.get_preamble_docs():
                h += str(preamble.document.get_version())
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
        return DEFAULT_STYLE_NAME
    m = ""
    for theme in theme_docs:
        m += f"{theme.id}"
    return f"compiled-style-{hashfunc(m)}"


def export_scss_variables() -> str:
    """Generates CSS that exports all SCSS variables in variables.scss as CSS properties.

    :return: A CSS string that exports all SCSS variables as CSS properties.
    """
    variables_path = stylesheets_folder / "variables.scss"
    if not variables_path.exists():
        return ""
    result = StringIO()
    result.write(":root {\n")
    with variables_path.open("r", encoding="utf-8") as f:
        for line in f:
            if line.startswith("$"):
                name = line.split(":")[0].strip()[1:]
                result.write(f"--{name}: #{{inspect(${name})}};\n")
    result.write("}\n")
    res = result.getvalue()
    return res


def generate_style(
    theme_docs: list[DocEntry],
    gen_dir: Path | None = None,
    throw_on_error: bool = False,
) -> tuple[str, str]:
    """Generates a CSS style based on the given theme documents.

    .. note::
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
    :param throw_on_error: If True, an error is thrown on SCSS compile error. Otherwise, the error is sent to Flask.
    :return: Tuple of path to the generated style file relative to project root (timApp folder) and last timestamp hash
    """
    if not gen_dir:
        gen_dir = get_default_scss_gen_dir()

    style_name = get_style_name(theme_docs)
    style_path = gen_dir / f"{style_name}.css"

    current_style_hash = hash_theme_timestamps(theme_docs)
    last_style_hash = get_style_timestamp_hash(style_name)
    style_exits = style_path.exists()

    if style_exits and last_style_hash == current_style_hash:
        return style_path.as_posix(), current_style_hash
    gen_dir.mkdir(exist_ok=True)

    theme_paths = []
    theme_doc_map = {}
    for theme_doc in theme_docs:
        theme_path = generate_scss(theme_doc)
        theme_paths.append((theme_doc.id, theme_path))
        theme_doc_map[f"../..{theme_path}"] = theme_doc.path

    scss_src = render_raw_template_string(
        """
@charset "UTF-8";
@import "stylesheets/varUtils";
{% for theme_id, theme in themes %}
@mixin post-all-{{ theme_id }} {};
@mixin post-variables-{{ theme_id }} {};
@import "{{ theme }}";
{% endfor %}
@import "stylesheets/variables";
{% for theme_id, _ in themes %}
@include post-variables-{{ theme_id }};
{% endfor %}
{{ css_props }}
@import "stylesheets/all.scss";
{% for theme_id, _ in themes %}
@include post-all-{{ theme_id }};
{% endfor %}
""",
        themes=theme_paths,
        css_props=export_scss_variables(),
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
        if throw_on_error:
            raise StyleCompileException(error_message)
        flash(
            sanitize_html(
                f"""
<p>Error in current style:</p>
<pre>{error_message}</pre>
<p>Please change style or fix the issue</p>
"""
            ),
            category="raw",
        )
        return (gen_dir / f"{DEFAULT_STYLE_NAME}.css").as_posix(), current_style_hash

    set_style_timestamp_hash(style_name, current_style_hash)
    return style_path.as_posix(), current_style_hash


@styles.get("")
def get_styles(
    docs: list[int]
    | None = field(
        default=None,
        metadata={"list_type": "delimited"},
    ),
    all: bool = False,
) -> Response:
    """Gets description of the available styles.

    :param docs: Style documents to get description from. If not given, all style documents are returned.
    :param all: If True, documents the user doesn't have access to aren't skipped.
                Otherwise, only documents user can viewed are returned.
    :return: List of JSON objects describing the style's name, path and type
    """
    cur_user = get_current_user_object()
    filter_user: User | None = cur_user
    filter: Any = DocEntry.name.like(f"{OFFICIAL_STYLES_PATH}%") | DocEntry.name.like(
        f"{USER_STYLES_PATH}%"
    )

    if docs:
        filter = DocEntry.id.in_(docs)
    if all:
        filter_user = None

    candidates = get_documents(filter_user=filter_user, custom_filter=filter)

    result = []
    for doc in candidates:
        has_access = cur_user.has_view_access(doc)
        style_description = doc.document.get_settings().get("description", None)
        if not style_description and not all:
            continue
        if not has_access:
            style_type = None
            style_description = None
        elif doc.path.startswith(OFFICIAL_STYLES_PATH):
            style_type = "official"
        else:
            style_type = "user"

        result.append(
            {
                "docId": doc.id,
                "name": doc.title if has_access else None,
                "path": doc.name,
                "description": style_description if has_access else None,
                "type": style_type,
            }
        )

    result = sorted(result, key=lambda r: r["type"])
    return json_response(result)


@styles.get("/<path:doc_path>.scss")
def get_raw_scss(doc_path: str, force: bool = False) -> Response:
    """Gets raw SCSS of the style document

    :param doc_path: Path to official or user document.
    :param force: If True, style is regenerated even if it was cached.
    :return: Generated SCSS or an error message.
    """
    verify_logged_in()
    doc = DocEntry.find_by_path(f"styles/{doc_path}")
    if not doc:
        raise NotExist()

    verify_view_access(doc)
    scss_path = generate_scss(doc, force)

    with open(scss_path, encoding="utf-8") as f:
        return text_response(f.read())


@styles.get("/path")
def generate(
    docs: list[int] = field(default_factory=list, metadata={"list_type": "delimited"})
) -> Response:
    """Gets path to the generated CSS style.

    :param docs: Style documents to generate the style from.
    :return: Path to the generated CSS file, or SCSS compile error.
    """
    verify_logged_in()

    doc_entries: list[DocEntry] = list(
        run_sql(select(DocEntry).filter(DocEntry.id.in_(docs))).scalars().all()
    )

    for doc in doc_entries:
        verify_view_access(doc)

        settings = doc.document.get_settings()
        if not settings.is_style_document():
            raise RouteException(
                f"Document {doc.id} ({doc.path}) must be marked as a style document (i.e. has `description` document setting)."
            )

    try:
        style_path, _ = generate_style(doc_entries)
        return text_response(style_path)
    except StyleCompileException as se:
        return text_response(
            se.compile_error,
            400,
        )
