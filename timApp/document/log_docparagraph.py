# Enable user personal logs with url param ?userlogs=TAG
import re
import flask
from typing import Callable

ENABLE_LOG_FOR_PERSON_LONG = 0
ENABLE_LOG_FOR_PERSON_SHORT = 0


def get_stack_str(limit: int = 6, remove_level: int = 0) -> str:
    import traceback

    stack = list(traceback.extract_stack())

    # Remove this function from the stack itself
    trimmed_stack = stack[: -1 - remove_level]

    # Take the last `limit` frames and reverse them (most recent first)
    last = trimmed_stack[-limit:]
    last.reverse()

    return "|".join(f"{s.name}, {s.filename}:{s.lineno}" for s in last)


def log_filename(file_name: str) -> None:
    from timApp.util.logger import log_info
    from timApp.auth.sessioninfo import get_username

    # log_info(f"W: {file_name}  {DocParagraph.get_stack_str(15, 1)}")
    log_info(f"{get_username()} W: {file_name} __write_")


def _log_for_person(msg_func: Callable[[], str], tag: str | None = None) -> None:
    """
    Logs msg_func if there is url parameter
       debug_writes=user_tag[&debug_reg=regular_expression][&debug_stack=true]
       debug_writes_long=user_tag,{log_tag}[&debug_reg=regular_expression][&debug_stack=true]
    Possible log tags are (find all calls for this function):
        dic - Creating par from dict
        unl - List of unloaded pars
        upd - Updating par cache HTML
        cha - list of changed pars
        pre - Pars to prealod
        che - Check cache for par
        wri - Writing par
        cle - Clear HTML cache for par
        stack - print also call stack
    To log for example just some par operations, one van use
        debug_reg = optional regular expression for printing
    and give the par id as regular expression

    :param msg_func: lambda text to log
    :param tag: condition to log this message,
                if log_tag is in debug_writes_long,
                then log this message, otherwise skip.
                If tag is missing, print anyway.
    :return: Nothing
    """
    if not flask.has_request_context():
        return
    # If url_param "debug_writes", log the filename and stack trace for debugging purposes
    tags = flask.request.args.get("debug_writes_long" if tag else "debug_writes")
    if tags is None:
        return

    parts = [p.strip() for p in tags.split(",") if p.strip()]

    if tag and tag not in parts:
        return

    user_tag: str = parts[0]
    text = msg_func()

    debug_reg = flask.request.args.get("debug_reg")
    if debug_reg:
        if re.search(debug_reg, text) is None:
            return
    stack = flask.request.args.get("debug_stack")
    from timApp.auth.sessioninfo import get_username

    username = get_username()

    from timApp.util.logger import log_info

    if stack:
        log_info(f"{username} PW ({user_tag}): {text}  {get_stack_str(15, 1)}")
    else:
        log_info(f"{username} PW ({user_tag}): {text}")


log_for_person = _log_for_person if ENABLE_LOG_FOR_PERSON_LONG else lambda *a, **k: None
log_for_person_short = (
    _log_for_person if ENABLE_LOG_FOR_PERSON_SHORT else lambda *a, **k: None
)
