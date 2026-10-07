"""Helpers for the embed mode (``?embed=true``) of the document view.

Embed mode renders a single task (or any paragraph range) so that it can be
shown inside an <iframe> on an external page, e.g. a course book hosted outside
TIM. See :class:`timApp.document.docviewparams.DocViewParams` for the related
URL parameters.
"""
from flask import Response, current_app, make_response, render_template, request
from flask_babel import gettext
from marshmallow import fields

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.timdb.exceptions import TimDbException
from timApp.util.flask.requesthelper import NotExist, RouteException
from timApp.util.flask.responsehelper import add_no_cache_headers

EMBED_ALLOWED_ORIGINS_KEY = "EMBED_ALLOWED_ORIGINS"


def parse_embed_allowed_origins(value: object) -> list[str]:
    """Parses the ``EMBED_ALLOWED_ORIGINS`` config value into a list of origins.

    The value may be a list of origins or a single string with origins separated
    by commas or whitespace (handy when the value comes from an environment
    variable). Empty entries are dropped and trailing slashes removed.
    """
    if value is None:
        return []
    if isinstance(value, str):
        raw = value.replace(",", " ").split()
    elif isinstance(value, (list, tuple, set)):
        raw = [str(v) for v in value]
    else:
        raise ValueError(
            f"{EMBED_ALLOWED_ORIGINS_KEY} must be a list of origins or a string"
        )
    result = []
    for origin in raw:
        origin = origin.strip().rstrip("/")
        if not origin:
            continue
        if "://" not in origin or any(c.isspace() for c in origin):
            raise ValueError(
                f"{EMBED_ALLOWED_ORIGINS_KEY}: invalid origin '{origin}' "
                f"(expected e.g. https://example.com)"
            )
        if origin not in result:
            result.append(origin)
    return result


def get_embed_allowed_origins() -> list[str]:
    """Returns the origins that are allowed to embed TIM documents in an <iframe>."""
    return parse_embed_allowed_origins(
        current_app.config.get(EMBED_ALLOWED_ORIGINS_KEY)
    )


def get_embed_frame_ancestors() -> str:
    """Returns the value of the Content-Security-Policy header for embed mode responses."""
    return " ".join(["frame-ancestors", "'self'", *get_embed_allowed_origins()])


def add_embed_headers(response: Response) -> Response:
    """Adds the security headers for an embed mode response.

    Only embed mode responses get the ``frame-ancestors`` directive; normal
    document views are left untouched.
    """
    response.headers["Content-Security-Policy"] = get_embed_frame_ancestors()
    return response


def is_embed_request() -> bool:
    """Returns whether the current request asks for embed mode (``?embed=true``)."""
    return request.args.get("embed", "") in fields.Boolean.truthy


def render_embed_notice(
    status: int,
    title: str,
    message: str,
    link_url: str,
    link_text: str,
    hint: str | None = None,
    user_name: str | None = None,
) -> Response:
    """Renders a compact notice that is shown in the frame instead of the task.

    The notice reports its height to the host page like the embedded document does,
    and its link opens TIM in a new top-level window. The frame reloads when the user
    returns to the host page after following the link (e.g. after logging in).
    """
    r = make_response(
        render_template(
            "embed_notice.jinja2",
            title=title,
            message=message,
            user_name=user_name,
            link_url=link_url,
            link_text=link_text,
            hint=hint,
            embed_allowed_origins=get_embed_allowed_origins(),
        ),
        status,
    )
    add_no_cache_headers(r)
    add_embed_headers(r)
    return r


def render_embed_login(doc_info: DocInfo, status: int) -> Response:
    """Renders the "log in to answer" notice for a user who is not logged in.

    The login link opens TIM in the top-level window because identity providers
    typically refuse to be framed.
    """
    return render_embed_notice(
        status,
        title=doc_info.title,
        message=gettext("Log in to TIM to answer this task."),
        link_url=f"/view/{doc_info.path}?login=true",
        link_text=gettext("Log in to TIM"),
        hint=gettext("Return to this tab after logging in."),
    )


def render_embed_no_access(doc_info: DocInfo, message: str | None) -> Response:
    """Renders the notice for a logged-in user who has no access to the document.

    :param message: The access denied message of the document, if it has one.
    """
    return render_embed_notice(
        403,
        title=doc_info.title,
        message=message or gettext("You do not have permission to view this task."),
        user_name=get_current_user_object().name,
        link_url=f"/view/{doc_info.path}",
        # In TIM the user can e.g. log out; the frame reloads on return.
        link_text=gettext("Open TIM"),
    )


def render_embed_error(message: str, status: int) -> Response:
    """Renders an error (e.g. an unknown task) as a compact notice for embed mode requests."""
    return render_embed_notice(
        status,
        title=gettext("Error"),
        message=message,
        link_url=request.path,
        link_text=gettext("Open in TIM"),
        hint=gettext(
            "The document opens in a new tab. "
            "This task reloads when you return to this page."
        ),
    )


def find_par_id_by_task(doc: Document, task: str) -> str:
    """Finds the id of the paragraph whose plugin has the given task id.

    :param doc: The document to search. Referenced paragraphs (e.g. in translations
     and citations) are searched too; in that case the id of the referencing
     paragraph in this document is returned.
    :param task: Task name (``t1``) or full task id (``123.t1``). If the document
     id is given, it must match the document that actually contains the task.
    :return: The id of the matching paragraph.
    :raises NotExist: If the task is not in the document.
    """
    try:
        wanted = TaskId.parse(
            task, require_doc_id=False, allow_block_hint=False, allow_type=False
        )
    except PluginException as e:
        raise RouteException(f"Invalid task id '{task}': {e}") from e

    def matches(p: DocParagraph) -> bool:
        if not p.get_attr("plugin"):
            return False
        attr = p.get_attr("taskId")
        if not attr:
            return False
        try:
            tid = TaskId.parse(attr, require_doc_id=False, allow_block_hint=False)
        except PluginException:
            return False
        if tid.task_name != wanted.task_name:
            return False
        if wanted.doc_id is None:
            return True
        if tid.doc_id is None:
            # Same resolution as find_task_ids: a referenced paragraph belongs to its source document.
            tid.update_doc_id_from_block(p)
        return wanted.doc_id == tid.doc_id

    for p in doc.get_paragraphs():
        if matches(p):
            return p.get_id()
        if p.is_reference():
            try:
                ref_pars = p.get_referenced_pars()
            except TimDbException:
                continue
            if any(matches(rp) for rp in ref_pars):
                return p.get_id()
    raise NotExist(
        f"Task '{task}' was not found in document {doc.doc_id}. "
        f"Check that the task id is correct and that the plugin paragraph is in this document."
    )
