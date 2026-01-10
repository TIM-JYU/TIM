"""Utility functions."""
import base64
import hashlib
import json
import os
import re
import shutil
import struct
import traceback
from concurrent.futures import Future
from dataclasses import fields, asdict
from datetime import datetime, timezone
from enum import Enum
from itertools import tee
from pathlib import Path, PurePosixPath
from types import TracebackType, FrameType
from typing import Any, Sequence, Callable, TypeVar, Iterable

import dateutil.parser
import pytz
import requests
from lxml.html import HtmlElement

from tim_common.html_sanitize import sanitize_html

NORM_NEWLINES = ["\r\n", "\r", "\n", "\u0085", "\u2028", "\u2029"]
NORM_NEWLINES_PATTERN = "|".join(NORM_NEWLINES)
NORM_NEWLINES_COMPILED = re.compile(NORM_NEWLINES_PATTERN)


def get_exception_code(
    ex: BaseException | None, tb: TracebackType | None = None
) -> str:
    """
    Creates a unique code for the exception and the optional traceback.
    The code can be used for short identification of errors.

    :param ex: Exception object
    :param tb: Optional traceback object
    :return: A string code of format ExceptionType[_HexCode] where _HexCode is generated from the traceback
    """
    t = type(ex)
    result = f"{t.__module__}.{t.__qualname__}"
    if tb:
        h = hashlib.shake_128()
        for f, _ in traceback.walk_tb(tb):  # type: FrameType, Any
            h.update(f.f_code.co_name.encode())
        result += f"_{h.hexdigest(2)}"
    return result


def datestr_to_relative(d: str | datetime) -> str:
    if isinstance(d, str):
        d = datetime.strptime(d, "%Y-%m-%d %H:%M:%S")
    return date_to_relative(d) if d else ""


def date_to_relative(d: datetime) -> str:
    """Converts the given datetime object to relative string representation, such as "2 days ago", etc.

    :param d: The datetime object to convert.
    :return: A string representing the given date relative to current time.

    """

    diff = get_current_time() - d
    s = diff.seconds
    if diff.days > 7 or diff.days < 0:
        return (datetime.now() - diff).strftime("%d %b %y")
    elif diff.days == 1:
        return "1 day ago"
    elif diff.days > 1:
        return f"{diff.days} days ago"
    elif s <= 1:
        return "just now"
    elif s < 60:
        return f"{s} seconds ago"
    elif s < 120:
        return "1 minute ago"
    elif s < 3600:
        return f"{s // 60} minutes ago"
    elif s < 7200:
        return "1 hour ago"
    else:
        return f"{s // 3600} hours ago"


def count_chars_from_beginning(md: str, char: str) -> int:
    num = 0
    for c in md:
        if c == char:
            num += 1
        else:
            break
    return num


def get_error_html(message: str | Exception, response: str | None = None) -> str:
    """Wraps an error message in an HTML element with class 'error'.

    :param response: The plugin response string.
    :param message: The message to be displayed in the error.
    :return: The sanitized error message HTML.
    """

    return sanitize_html(
        '<span class="error" ng-non-bindable>{}{}</span>'.format(
            str(message),
            f"<pre>---Full response string start---\n{response}\n---Full response string end---</pre>"
            if response is not None
            else "",
        )
    )


def get_error_html_block(
    title: str, message: str | Exception, response: str | None = None
) -> str:
    """Wraps an error message in a block-level HTML element with class 'error'.

    :param title: Title of the error.
    :param response: The plugin response string.
    :param message: The message to be displayed in the error.
    :return: The sanitized error message HTML.
    """

    return sanitize_html(
        '<div class="error" ng-non-bindable><strong>{}</strong>{}{}</div>'.format(
            title,
            f"<pre>{message}</pre>" if message else "",
            f"<pre>---Full response string start---\n{response}\n---Full response string end---</pre>"
            if response is not None
            else "",
        )
    )


def get_error_tex(title: str, message: str | Exception) -> str:
    """Wraps an error message in a TeX element 'timpluginerror'.

    :param title: The plugin response string.
    :param message: The message to be displayed in the error.
    :return: The sanitized error message HTML.
    """

    return f"\\timpluginerror{{ {title} }}{{ {str(message)} }}"


def del_content(
    directory: Path, onerror: Callable[[Any, str, Any], Any] | None = None
) -> None:
    for f in os.listdir(directory):
        f_path = os.path.join(directory, f)
        try:
            if os.path.isfile(f_path):
                os.unlink(f_path)
            elif os.path.isdir(f_path):
                shutil.rmtree(f_path, onerror=onerror)
        except Exception as e:
            print(e)


def split_location(path: str) -> tuple[str, str]:
    """Given a path 'a/b/c/d', returns a tuple ('a/b/c', 'd')."""
    rs = path.rfind("/")
    return ("", path) if rs < 0 else (path[:rs], path[rs + 1 :])


def join_location(location: str, name: str) -> str:
    return name if location == "" else location + "/" + name


def relative_location(location: str, base: str) -> str:
    """Returns the location of location relative to base."""
    return str(PurePosixPath(location).relative_to(base))


def get_sql_template(value_list: list) -> str:
    return ",".join(["%s"] * len(value_list))


def pycharm_running() -> bool:
    return os.environ.get("PYCHARM_HOSTED") == "1"


def remove_path_special_chars(item_path: str) -> str:
    return re.sub(
        "[^a-zA-Z0-9/_-]", "", item_path.translate(str.maketrans(" äöåÄÖÅ", "-aoaAOA"))
    )


accent_map = str.maketrans(
    "ãàáäâåẽèéëêìíïîõòóöôùúüûñç·/,:;", "aaaaaaeeeeeiiiiooooouuuunc-----"
)
accent_map_underscored = str.maketrans(
    "ãàáäâåẽèéëêìíïîõòóöôùúüûñç·/,:;", "aaaaaaeeeeeiiiiooooouuuunc_____"
)


def slugify(s: str, underscored: bool = False) -> str:
    """
    Slugify a string into a URL-friendly string.
    :param s: The string to slugify.
    :param underscored: If True, replace spaces with underscores instead of dashes.
    :return: Slugified string.
    """
    s = s.strip().lower()
    s = s.translate(accent_map_underscored if underscored else accent_map)
    s = re.sub(r"[^a-z0-9 _.-]", "", s)
    s = re.sub(r"\s+", "_" if underscored else "-", s)
    s = re.sub(r"-+", "_" if underscored else "-", s)
    s = re.sub(r"_+", "_" if underscored else "-", s)
    return s


def title_to_id(s: str) -> str:
    """Converts an HTML heading to id attribute. Tries to be equivalent to what Pandoc does."""
    if s is None:
        return "section"
    if not any(c.isalpha() for c in s):
        return "section"
    s = re.sub(r"[^\w-]", " ", s.lower())
    s = "-".join(s.split())
    return s


def getdatetime(s: str, default_val: datetime | None = None) -> datetime | None:
    try:
        dt = dateutil.parser.parse(s, dayfirst=not "Z" in s)
        return dt if dt.tzinfo is not None else dt.replace(tzinfo=timezone.utc)
    except (ValueError, TypeError):
        return default_val


def trim_markdown(text: str) -> str:
    """Trims the specified text. Don't trim spaces from left side because they may indicate a code block.

    :param text: The text to be trimmed.
    :return: The trimmed text.

    """
    return text.rstrip().strip("\r\n")


def remove_prefix(text: str, prefix: str) -> str:
    if text.startswith(prefix):
        return text[len(prefix) :]
    return text


def include_keys(obj: dict[str, Any], *keys: str) -> dict[str, Any]:
    return {k: v for k, v in obj.items() if k in keys}


def exclude_keys(obj: dict[str, Any], *keys: str) -> dict[str, Any]:
    return {k: v for k, v in obj.items() if k not in keys}


# noinspection PyPep8Naming
class cached_property:
    """
    A property that is only computed once per instance and then replaces itself
    with an ordinary attribute. Deleting the attribute resets the property.
    Source: https://github.com/bottlepy/bottle/commit/fa7733e075da0d790d809aa3d2f53071897e6f76
    """

    def __init__(self, func: Callable):
        self.__doc__ = getattr(func, "__doc__")
        self.func = func

    def __get__(self, obj: Any, cls: Any) -> Any:
        if obj is None:
            return self
        value = obj.__dict__[self.func.__name__] = self.func(obj)
        return value


def try_load_json(json_str: str | None) -> None | str | Any:
    """"""
    try:
        if json_str is not None:
            return json.loads(json_str)
        return None
    except ValueError:
        return json_str


def get_boolean(s: bool | int | str, default: bool) -> bool:
    if s is None:
        return default
    if isinstance(s, bool):
        return s
    if isinstance(s, int):
        return s != 0
    result = s
    lresult = result.lower().strip()
    if len(lresult) == 0:
        return default
    if "f0n".find(lresult[0]) >= 0:
        return False
    if "t1y".find(lresult[0]) >= 0:
        return True
    return default


def get_static_tim_doc_path() -> Path:
    return Path("static/tim_docs")


def static_tim_doc(path: str) -> str:
    return f"static/tim_docs/{path}"


def decode_csplugin(text: HtmlElement) -> dict[str, Any]:
    return json.loads(base64.b64decode(text.get("json")))["markup"]


def get_current_time() -> datetime:
    return datetime.now(tz=timezone.utc)


def seq_to_str(lst: Sequence[str]) -> str:
    if not lst:
        return ""
    if len(lst) == 1:
        return lst[0]
    else:
        return f", ".join(lst[:-1]) + " and " + lst[-1]


def split_by_semicolon(p: str) -> list[str]:
    return [s.strip() for s in p.split(";")]


def get_error_message(e: Exception) -> str:
    """
    Gives error message with error class.
    :param e: Exception.
    :return: String 'ErrorClass: reason'.
    """
    return f"{str(e.__class__.__name__)}: {str(e)}"


Range = tuple[int, int]

TASK_PROG = re.compile(
    r"([\w\.]*)(:\w*)?\( *(\d*) *, *(\d*) *\)(.*)"
)  # see https://regex101.com/r/ZZuizF/4
TASK_NAME_PROG = re.compile(
    r"(\d+\.)?([\w\d]+)[.\[]?.*"
)  # see https://regex101.com/r/CFOLgd/1


def widen_fields(fields: list[str] | str) -> list[str]:
    """
    if there is syntax d(1,3) in fields, it is made d1,d2
    from d(1,3)=t  would come d1=t1, d2=t2

    :param fields: list of fields
    :return: array fields widened
    """
    fields1 = []
    if not isinstance(fields, list):
        fields = fields.split(";")
    for field in fields:
        parts = field.split(";")
        fields1.extend(parts)

    rfields = []
    for field in fields1:
        field = field.strip()
        if not field:
            continue
        parts = field.split("=")
        t = parts[0].strip()
        a = None
        if len(parts) > 1:
            a = parts[1].strip()
        match = re.search(TASK_PROG, t)
        if not match:
            rfields.append(field)
            continue

        tb = match.group(1)
        ft = match.group(2) or ""
        n1 = int(match.group(3))
        n2 = int(match.group(4))
        te = match.group(5)

        for i in range(n1, n2 + 1):
            tn = tb + str(i) + te
            if not tb:
                tn = ""
            tn += ft
            if a is not None:  # a is allowed to be empty
                tn += "=" + a + str(i)
            rfields.append(tn)

    return rfields


def get_alias(name: str) -> str:
    """
    Get name part form string like 534.d1.points

    :param name: full name of field
    :return: just name part of field, like d1
    """
    t = name.strip()
    match = re.search(TASK_NAME_PROG, t)
    if not match:
        return name
    return match.group(2)


fin_timezone = pytz.timezone("Europe/Helsinki")
local_timezone = fin_timezone  # TODO: find real local timezone somewhere
temp_folder_path = Path("/tmp")
cache_folder_path = Path("/cache")


# TODO: Use an email validation library.
def is_valid_email(email: str) -> bool:
    parts = email.split("@")
    if len(parts) != 2:
        return False
    username, domain = parts
    if username.startswith(".") or username.endswith(".") or ".." in username:
        return False
    return (
        re.match(r"^[\w.+-]+$", username) is not None
        and re.match(r"^([\w-]+\.)+[\w-]{2,}$", domain) is not None
    )


def convert_email_to_lower(email_or_username: str) -> str:
    email_or_username = email_or_username.strip()
    if is_valid_email(email_or_username):
        return email_or_username.lower()
    return email_or_username


def approximate_real_name(email: str) -> str:
    nameparts = email.split("@")[0].split(".")
    approx_name = f"{nameparts[-1].title()} {nameparts[0].title()}"
    return approx_name


def get_dataclass_field_names(d: Any) -> set[str]:
    return {f.name for f in fields(d)}


def dataclass_to_bytearray(x: Any) -> bytearray:
    b = bytearray()
    for k, v in asdict(x).items():
        b.extend(k.encode())
        b.append(0)
        append_to_bytearray(b, v)
        b.append(0)
    return b


def append_to_bytearray(b: bytearray, v: Any) -> None:
    if v is None:
        pass
    elif isinstance(v, str):
        b.extend(v.encode())
    elif isinstance(v, bool):
        b.append(v)
    elif isinstance(v, int):
        b.extend(v.to_bytes(4, "little", signed=True))
    elif isinstance(v, float):
        b.extend(struct.pack("f", v))
    elif isinstance(v, tuple) or isinstance(v, list):
        for m in v:
            append_to_bytearray(b, m)
    elif isinstance(v, Enum):
        append_to_bytearray(b, v.value)
    else:
        raise Exception(f"Unhandled type: {type(v)}")


def read_json_lines(file_to_read: Path) -> list[dict]:
    with file_to_read.open() as f:
        content = f.read()
    json_str = f'[{",".join(content.splitlines())}]'
    loaded_json = json.loads(json_str)
    return loaded_json


def wait_response_and_collect_error(f: Future, h: str, errors: list[str]) -> None:
    try:
        resp: requests.Response = f.result()
    except Exception as e:
        errors.append(f"Connection to {h} failed: {e}")
    else:
        if resp.status_code != 200:
            errors.append(
                f"{resp.request.url} returned status {resp.status_code} and text {resp.text}"
            )


def collect_errors_from_hosts(futures: list[Future], hosts: list[str]) -> list[str]:
    errors: list[str] = []
    for f, h in zip(futures, hosts):
        wait_response_and_collect_error(f, h, errors)
    return errors


TItem = TypeVar("TItem")


def partition(
    pred: Callable[[TItem], bool], iterable: Iterable[TItem]
) -> tuple[Iterable[TItem], Iterable[TItem]]:
    """
    Use a predicate to partition entries into false entries and true entries.

    :param pred: Predicate to determine in which tuple entries will be placed.
    :param iterable: Iterable that is divided into a tuple based on the predicate.
    :return: True entries go to the left, and false entries to the right.
    """

    # Create two buffered iterators: values of i1 are cached for i2 to use
    i1, i2 = tee((pred(item), item) for item in iterable)
    return [item for t, item in i1 if t], [item for t, item in i2 if not t]


def normalize_newlines(text: str) -> str:
    """
    Find and replace non-canonical separator characters with whitespace ' ' in the specified text.

    :param text: Original text.
    :return: Modified source text.
    """
    return re.sub(NORM_NEWLINES_COMPILED, " ", text)


TIM_IDENTS = "a-zA-Z0-9_-"

TIM_IDENTS_RE = re.compile(rf"^[{TIM_IDENTS}]*$")
NOT_TIM_IDENTS_RE = re.compile(rf"[^{TIM_IDENTS}]")


def is_valid_tim_indetifier(s: str) -> bool:
    """
    Check whether the given string is a valid TIM identifier.

    :param s: The string to check.
    :return: True if the string is a valid TIM identifier, False otherwise.
    """
    return bool(TIM_IDENTS_RE.match(s))


def strip_not_allowed(s: str, allow_reg: str | None = None) -> str:
    """
    Strips characters that are not allowed in TIM identifiers.

    :param s: The string to strip.
    :param allow_reg: Optional regex pattern to specify allowed characters.
    :return: The stripped string.
    """
    if s is None or s.strip() == "":
        return ""
    if allow_reg is not None:
        return re.sub(rf"[^{allow_reg}]", "", s)
    return NOT_TIM_IDENTS_RE.sub("", s)
