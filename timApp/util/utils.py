"""Utility functions."""
import binascii
import json
import os
import re
import shutil
from datetime import datetime, timezone
from typing import List, Optional, Tuple, Union, Dict, Any, Sequence

import dateutil.parser

from timApp.markdown.htmlSanitize import sanitize_html


def datestr_to_relative(dstr):
    if isinstance(dstr, str):
        dstr = datetime.strptime(dstr, '%Y-%m-%d %H:%M:%S')
    return date_to_relative(dstr) if dstr else ''


def date_to_relative(d: Optional[datetime]):
    """Converts the given datetime object to relative string representation, such as "2 days ago", etc.

    :param d: The datetime object to convert.
    :return: A string representing the given date relative to current time.

    """

    if d is None:
        return None
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


def count_chars_from_beginning(md: str, char: str):
    num = 0
    for c in md:
        if c == char:
            num += 1
        else:
            break
    return num


def get_error_html(message: Union[str, Exception], response: Optional[str]=None):
    """Wraps an error message in an HTML element with class 'error'.

    :param response: The plugin response string.
    :param message: The message to be displayed in the error.
    :return: The sanitized error message HTML.
    """

    return sanitize_html('<div class="error">{}{}</div>'.format(str(message),
                                                                f'<pre>---Full response string start---\n{response}\n---Full response string end---</pre>' if response is not None else ''))


def get_error_tex(title, message: Union[str, Exception], response: Optional[str]=None):
    """Wraps an error message in a TeX element 'timpluginerror'.

    :param response: The plugin response string.
    :param message: The message to be displayed in the error.
    :return: The sanitized error message HTML.
    """

    return f'\\timpluginerror{{ {title} }}{{ {str(message)} }}'


def del_content(directory, onerror=None):
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


def get_sql_template(value_list: List) -> str:
    return ','.join(['%s'] * len(value_list))


def pycharm_running():
    return os.environ.get('PYCHARM_HOSTED') == '1'


def remove_path_special_chars(item_path):
    return re.sub('[^a-zA-Z0-9/_-]', '', item_path.translate(str.maketrans(' äöåÄÖÅ', '-aoaAOA')))


def title_to_id(s: str):
    """Converts a HTML heading to id attribute. Tries to be equivalent to what Pandoc does."""
    if s is None:
        return 'section'
    if not any(c.isalpha() for c in s):
        return 'section'
    s = re.sub(r'[^\w-]', ' ', s.lower())
    s = '-'.join(s.split())
    return s


def getdatetime(s: str, default_val=None):
    try:
        dt = dateutil.parser.parse(s, dayfirst=not 'Z' in s)
        return dt if dt.tzinfo is not None else dt.replace(tzinfo=timezone.utc)
    except (ValueError, TypeError):
        return default_val


def trim_markdown(text: str):
    """Trims the specified text. Don't trim spaces from left side because they may indicate a code block.

    :param text: The text to be trimmed.
    :return: The trimmed text.

    """
    return text.rstrip().strip('\r\n')


def remove_prefix(text: str, prefix: str):
    if text.startswith(prefix):
        return text[len(prefix):]
    return text


def include_keys(obj: Dict[str, Any], *keys: str):
    return {k: v for k, v in obj.items() if k in keys}


def exclude_keys(obj: Dict[str, Any], *keys: str):
    return {k: v for k, v in obj.items() if k not in keys}


class cached_property:
    """
    A property that is only computed once per instance and then replaces itself
    with an ordinary attribute. Deleting the attribute resets the property.
    Source: https://github.com/bottlepy/bottle/commit/fa7733e075da0d790d809aa3d2f53071897e6f76
    """

    def __init__(self, func):
        self.__doc__ = getattr(func, '__doc__')
        self.func = func

    def __get__(self, obj, cls):
        if obj is None:
            return self
        value = obj.__dict__[self.func.__name__] = self.func(obj)
        return value


def try_load_json(json_str: str):
    """"""
    try:
        if json_str is not None:
            return json.loads(json_str)
        return None
    except ValueError:
        return json_str


def get_boolean(s, default, cast=None):
    if s is None:
        return default
    if isinstance(s, bool):
        return s
    if isinstance(s, int):
        return s != 0
    result = s
    lresult = result.lower()
    if isinstance(default, bool):
        if len(lresult) == 0:
            return default
        if "f0y".find(lresult[0]) >= 0:
            return False
        if "t1n".find(lresult[0]) >= 0:
            return True
        return True
    if isinstance(default, int):
        try:
            return int(lresult)
        except ValueError:
            return default
    if cast is not None:
        try:
            result = cast(result)
        except ValueError:
            pass
    return result


EXAMPLE_DOCS_PATH = 'static/example_docs'


def decode_csplugin(text: str):
    return json.loads(binascii.unhexlify(remove_prefix(text, 'xxxHEXJSONxxx')).decode())


def get_current_time():
    return datetime.now(tz=timezone.utc)


def seq_to_str(lst: Sequence[str]):
    if len(lst) == 1:
        return lst[0]
    else:
        return f', '.join(lst[:-1]) + ' and ' + lst[-1]


def split_by_semicolon(p: str):
    return [s.strip() for s in p.split(';')]
