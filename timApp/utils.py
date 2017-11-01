"""Utility functions."""
import os
import re
import shutil
from datetime import datetime, timezone
from typing import List, Optional, Tuple, Union

import dateutil.parser

from timApp.htmlSanitize import sanitize_html
from timApp.theme import Theme


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
    diff = datetime.now(timezone.utc) - d
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


def count_chars(md, char):
    num_ticks = 0
    for i, c in enumerate(md):
        if c == char:
            num_ticks = i + 1
        else:
            break
    return num_ticks


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


class ThemeNotFoundException(Exception):
    pass


def generate_theme_scss(themes: List[Theme], gen_dir: str) -> None:
    """Generates an SCSS file based on the given theme names.

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

    :param themes: The list of themes.
    :param gen_dir: The directory where the SCSS file should be generated.

    """
    for t in themes:
        if not t.exists():
            raise ThemeNotFoundException(t.filename)
    combined = get_combined_css_filename(themes)
    if os.path.exists(os.path.join(gen_dir, combined + '.scss')):
        return
    if not os.path.exists(gen_dir):
        os.mkdir(gen_dir)
    with open(os.path.join(gen_dir, combined + '.scss'), encoding='utf-8', mode='w') as f:
        f.write('@charset "UTF-8";\n')
        f.write('@import "variables";\n')
        for t in themes:
            f.write(f'@mixin {t.filename} {{}}\n')
            f.write(f'@import "css/{t.filename}";\n')
        f.write('@import "all.scss";\n')  # "all" conflicts with a jQuery CSS file, so we must add the .scss extension
        for t in themes:
            f.write(f'@include {t.filename};\n')


def get_combined_css_filename(themes: List[Theme]):
    """Returns the combined file name based on the given list of theme names.

    :param themes: The list of themes.
    :return: The combined file name based on the themes. If the list is empty, 'default' is returned.

    """
    return '-'.join(t.filename for t in themes) or 'default'


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
