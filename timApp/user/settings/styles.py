from dataclasses import field, dataclass
from os.path import getmtime
from pathlib import Path
from typing import Optional

import sass
from flask import Response, current_app, flash

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
from timApp.util.utils import cache_folder_path, render_raw_template_string

styles = TypedBlueprint("styles", __name__, url_prefix="/styles")
DEFAULT_STYLE_NAME = "default"

scss_cache_path = cache_folder_path / "generated_scss"


@dataclass
class StyleCompileException(Exception):
    compile_error: str


def generate_scss(doc: DocInfo, force: bool = False) -> str:
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
    par_ids = set(
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
    )

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
            # Replace post-all hook with unique ID for style generation
            md = md.replace("@mixin post-all", f"@mixin post-all-{doc.id}")

            f.write(f"{md}\n")

    return cache_path.as_posix()


def hash_theme_timestamps(theme_docs: Optional[list[DocEntry]] = None) -> str:
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


def generate_style(
    theme_docs: list[DocEntry],
    gen_dir: Optional[Path] = None,
    throw_on_error: bool = False,
) -> str:
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
    :return: Path to the generated style file, relative to project root (timApp folder)
    """
    if not gen_dir:
        gen_dir = get_default_scss_gen_dir()

    style_name = get_style_name(theme_docs)
    style_path = gen_dir / f"{style_name}.css"

    current_style_hash = hash_theme_timestamps(theme_docs)
    last_style_hash = get_style_timestamp_hash(style_name)
    style_exits = style_path.exists()

    if style_exits and last_style_hash == current_style_hash:
        return style_path.as_posix()
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
        if throw_on_error:
            raise StyleCompileException(error_message)
        flash(
            f"""
<p>Error in current style:</p>
<pre>{error_message}</pre>
<p>Please change style or fix the issue</p>
"""
        )
        return (gen_dir / f"{DEFAULT_STYLE_NAME}.css").as_posix()

    set_style_timestamp_hash(style_name, current_style_hash)
    return style_path.as_posix()


@styles.get("")
def get_styles(
    docs: Optional[list[int]] = field(
        default=None,
        metadata={"list_type": "delimited"},
    ),
    all: bool = False,
) -> Response:
    cur_user = get_current_user_object()
    filter_user: Optional[User] = cur_user
    filter = DocEntry.name.like(f"{OFFICIAL_STYLES_PATH}%") | DocEntry.name.like(
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
    verify_logged_in()
    doc = DocEntry.find_by_path(f"styles/{doc_path}")
    if not doc:
        raise NotExist()

    verify_view_access(doc)
    scss_path = generate_scss(doc, force)

    with open(scss_path, "r", encoding="utf-8") as f:
        return text_response(f.read())


@styles.get("/path")
def generate(
    docs: list[int] = field(default_factory=list, metadata={"list_type": "delimited"})
) -> Response:
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

    try:
        return text_response(generate_style(doc_entries, throw_on_error=True))
    except StyleCompileException as se:
        return text_response(
            se.compile_error,
            400,
        )
