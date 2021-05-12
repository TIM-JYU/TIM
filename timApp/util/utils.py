"""Utility functions."""
import base64
import json
import os
import re
import shutil
import struct
from dataclasses import fields, asdict
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path, PurePosixPath
from typing import List, Optional, Tuple, Union, Dict, Any, Sequence, Callable, Set

import dateutil.parser
import pytz
from lxml.html import HtmlElement

from timApp.markdown.htmlSanitize import sanitize_html


def datestr_to_relative(d: Union[str, datetime]) -> str:
    if isinstance(d, str):
        d = datetime.strptime(d, '%Y-%m-%d %H:%M:%S')
    return date_to_relative(d) if d else ''


def date_to_relative(d: datetime) -> str:
    """Converts the given datetime object to relative string representation, such as "2 days ago", etc.

    :param d: The datetime object to convert.
    :return: A string representing the given date relative to current time.

    """

    diff = get_current_time() - d
    s = diff.seconds
    if diff.days > 7 or diff.days < 0:
        return (datetime.now() - diff).strftime('%d %b %y')
    elif diff.days == 1:
        return '1 day ago'
    elif diff.days > 1:
        return f'{diff.days} days ago'
    elif s <= 1:
        return 'just now'
    elif s < 60:
        return f'{s} seconds ago'
    elif s < 120:
        return '1 minute ago'
    elif s < 3600:
        return f'{s // 60} minutes ago'
    elif s < 7200:
        return '1 hour ago'
    else:
        return f'{s // 3600} hours ago'


def count_chars_from_beginning(md: str, char: str) -> int:
    num = 0
    for c in md:
        if c == char:
            num += 1
        else:
            break
    return num


def get_error_html(message: Union[str, Exception], response: Optional[str]=None) -> str:
    """Wraps an error message in an HTML element with class 'error'.

    :param response: The plugin response string.
    :param message: The message to be displayed in the error.
    :return: The sanitized error message HTML.
    """

    return sanitize_html(
        '<span class="error">{}{}</span>'.format(str(message),
                                                 f'<pre>---Full response string start---\n{response}\n---Full response string end---</pre>' if response is not None else ''))


def get_error_tex(title: str, message: Union[str, Exception]) -> str:
    """Wraps an error message in a TeX element 'timpluginerror'.

    :param title: The plugin response string.
    :param message: The message to be displayed in the error.
    :return: The sanitized error message HTML.
    """

    return f'\\timpluginerror{{ {title} }}{{ {str(message)} }}'


def del_content(directory: Path, onerror: Optional[Callable[[Any, str, Any], Any]]=None) -> None:
    for f in os.listdir(directory):
        f_path = os.path.join(directory, f)
        try:
            if os.path.isfile(f_path):
                os.unlink(f_path)
            elif os.path.isdir(f_path):
                shutil.rmtree(f_path, onerror=onerror)
        except Exception as e:
            print(e)


def split_location(path: str) -> Tuple[str, str]:
    """Given a path 'a/b/c/d', returns a tuple ('a/b/c', 'd')."""
    rs = path.rfind('/')
    return ('', path) if rs < 0 else (path[:rs], path[rs + 1:])


def join_location(location: str, name: str) -> str:
    return name if location == '' else location + '/' + name

def relative_location(location: str, base: str) -> str:
    """Returns the location of location relative to base."""
    return str(PurePosixPath(location).relative_to(base))

def get_sql_template(value_list: List) -> str:
    return ','.join(['%s'] * len(value_list))


def pycharm_running() -> bool:
    return os.environ.get('PYCHARM_HOSTED') == '1'


def remove_path_special_chars(item_path: str) -> str:
    return re.sub('[^a-zA-Z0-9/_-]', '', item_path.translate(str.maketrans(' äöåÄÖÅ', '-aoaAOA')))


def title_to_id(s: str) -> str:
    """Converts a HTML heading to id attribute. Tries to be equivalent to what Pandoc does."""
    if s is None:
        return 'section'
    if not any(c.isalpha() for c in s):
        return 'section'
    s = re.sub(r'[^\w-]', ' ', s.lower())
    s = '-'.join(s.split())
    return s


def getdatetime(s: str, default_val: Optional[datetime] = None) -> Optional[datetime]:
    try:
        dt = dateutil.parser.parse(s, dayfirst=not 'Z' in s)
        return dt if dt.tzinfo is not None else dt.replace(tzinfo=timezone.utc)
    except (ValueError, TypeError):
        return default_val


def trim_markdown(text: str) -> str:
    """Trims the specified text. Don't trim spaces from left side because they may indicate a code block.

    :param text: The text to be trimmed.
    :return: The trimmed text.

    """
    return text.rstrip().strip('\r\n')


def remove_prefix(text: str, prefix: str) -> str:
    if text.startswith(prefix):
        return text[len(prefix):]
    return text


def include_keys(obj: Dict[str, Any], *keys: str) -> Dict[str, Any]:
    return {k: v for k, v in obj.items() if k in keys}


def exclude_keys(obj: Dict[str, Any], *keys: str) -> Dict[str, Any]:
    return {k: v for k, v in obj.items() if k not in keys}


# noinspection PyPep8Naming
class cached_property:
    """
    A property that is only computed once per instance and then replaces itself
    with an ordinary attribute. Deleting the attribute resets the property.
    Source: https://github.com/bottlepy/bottle/commit/fa7733e075da0d790d809aa3d2f53071897e6f76
    """

    def __init__(self, func: Callable):
        self.__doc__ = getattr(func, '__doc__')
        self.func = func

    def __get__(self, obj: Any, cls: Any) -> Any:
        if obj is None:
            return self
        value = obj.__dict__[self.func.__name__] = self.func(obj)
        return value


def try_load_json(json_str: Optional[str]) -> Union[None, str, Any]:
    """"""
    try:
        if json_str is not None:
            return json.loads(json_str)
        return None
    except ValueError:
        return json_str


def get_boolean(s: Union[bool, int, str], default: bool) -> bool:
    if s is None:
        return default
    if isinstance(s, bool):
        return s
    if isinstance(s, int):
        return s != 0
    result = s
    lresult = result.lower()
    if len(lresult) == 0:
        return default
    if "f0y".find(lresult[0]) >= 0:
        return False
    if "t1n".find(lresult[0]) >= 0:
        return True
    return True


def static_tim_doc(path: str) -> str:
    return f'static/tim_docs/{path}'


def decode_csplugin(text: HtmlElement) -> Dict[str, Any]:
    return json.loads(base64.b64decode(text.get('json')))['markup']


def get_current_time() -> datetime:
    return datetime.now(tz=timezone.utc)


def seq_to_str(lst: Sequence[str]) -> str:
    if len(lst) == 1:
        return lst[0]
    else:
        return f', '.join(lst[:-1]) + ' and ' + lst[-1]


def split_by_semicolon(p: str) -> List[str]:
    return [s.strip() for s in p.split(';')]


def get_error_message(e: Exception) -> str:
    """
    Gives error message with error class.
    :param e: Exception.
    :return: String 'ErrorClass: reason'.
    """
    return f"{str(e.__class__.__name__)}: {str(e)}"


Range = Tuple[int, int]

TASK_PROG = re.compile('([\w\.]*)(:\w*)?\( *(\d*) *, *(\d*) *\)(.*)') # see https://regex101.com/r/ZZuizF/4
TASK_NAME_PROG = re.compile("(\d+.)?([\w\d]+)[.\[]?.*")  # see https://regex101.com/r/OjnTAn/4


def widen_fields(fields: Union[List[str], str]) -> List[str]:
    """
    if there is syntax d(1,3) in fileds, it is made d1,d2
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


fin_timezone = pytz.timezone('Europe/Helsinki')
local_timezone = fin_timezone  #  TODO: find real local timezone somewhere
temp_folder_path = Path('/tmp')
cache_folder_path = Path('/cache')


# TODO: Use an email validation library.
def is_valid_email(email: str) -> bool:
    parts = email.split('@')
    if len(parts) != 2:
        return False
    username, domain = parts
    if username.startswith('.') or username.endswith('.') or '..' in username:
        return False
    return (
            re.match('^[\w.+-]+$', username) is not None
            and
            re.match('^([\w-]+\.)+[\w-]{2,}$', domain) is not None
    )


def approximate_real_name(email: str) -> str:
    nameparts = email.split('@')[0].split('.')
    approx_name = f'{nameparts[-1].title()} {nameparts[0].title()}'
    return approx_name


def get_dataclass_field_names(d: Any) -> Set[str]:
    return set(f.name for f in fields(d))


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
        b.extend(v.to_bytes(4, 'little', signed=True))
    elif isinstance(v, float):
        b.extend(struct.pack("f", v))
    elif isinstance(v, tuple):
        for m in v:
            append_to_bytearray(b, m)
    elif isinstance(v, Enum):
        append_to_bytearray(b, v.value)
    else:
        raise Exception(f'Unhandled type: {type(v)}')
