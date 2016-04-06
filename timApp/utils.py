"""Utility functions."""
import os
import re
import shutil
from datetime import datetime

import yaml
from typing import List
from yaml import CLoader

from htmlSanitize import sanitize_html


def datestr_to_relative(dstr):
    return date_to_relative(datetime.strptime(dstr, '%Y-%m-%d %H:%M:%S'))


def date_to_relative(d):
    """Converts the given datetime object to relative string representation, such as "2 days ago", etc.

    :param d: The datetime object to convert.
    :return: A string representing the given date relative to current time.
    """
    diff = datetime.now() - d
    s = diff.seconds
    if diff.days > 7 or diff.days < 0:
        return d.strftime('%d %b %y')
    elif diff.days == 1:
        return '1 day ago'
    elif diff.days > 1:
        return '{} days ago'.format(diff.days)
    elif s <= 1:
        return 'just now'
    elif s < 60:
        return '{} seconds ago'.format(s)
    elif s < 120:
        return '1 minute ago'
    elif s < 3600:
        return '{} minutes ago'.format(s//60)
    elif s < 7200:
        return '1 hour ago'
    else:
        return '{} hours ago'.format(s//3600)


def merge(a, b):
    """Merges two dictionaries recursively. Stores the result in the first dictionary.
    :param a: The first dictionary.
    :param b: The second dictionary.
    """
    for key in b:
        if key in a:
            if isinstance(a[key], dict) and isinstance(b[key], dict):
                merge(a[key], b[key])
            elif a[key] == b[key]:
                pass
            else:
                a[key] = b[key]
        else:
            a[key] = b[key]


def correct_yaml(text):
    """
      Inserts missing spaces after : Like  width:20 => width: 20
      Also gives an other way to write multiline attributes, by starting
      the multiline like: program: |!!  (!! could be any number and any non a-z,A-Z chars
      and ending it by !! in first column

    :param text: text to convert proper yaml
    :return: text that is proper yaml
    :type text: str
    """
    lines = text.splitlines()
    s = ""
    p = re.compile("^[^ :]*:[^ ]")  # kissa:istuu
    pm = re.compile("^[^ :]+:[ ]*\|[ ]*[^ ]+[ ]*$")  # program: ||| or  program: |!!!
    multiline = False
    end_str = ''
    for line in lines:
        line = line.rstrip()
        if p.match(line) and not multiline:
            line = line.replace(':', ': ', 1)
        if pm.match(line):
            multiline = True
            line, end_str = line.split("|", 1)
            end_str = end_str.rstrip()
            s = s + line + "|\n"
            continue
        if multiline:
            if line == end_str:
                multiline = False
                continue
            line = " " + line
        s = s + line + "\n"
    return s


def parse_yaml(text):
    """

    :type text: str
    :return:
    """

    if len(text) == 0:
        return False
    try:
        text = correct_yaml(text)
        values = yaml.load(text, Loader=CLoader)
    except yaml.parser.ParserError as e:
        return str(e)
    except yaml.scanner.ScannerError as e:
        return str(e)
    try:
        if type(values) is str:
            return values
        else:
            return values
    except KeyError:
        return "Missing identifier"


def count_chars(md, char):
    num_ticks = 0
    for i, c in enumerate(md):
        if c == char:
            num_ticks = i + 1
        else:
            break
    return num_ticks


def get_error_html(message):
    """
    Wraps an error message in an HTML element with class 'error'.

    :param message: The message to be displayed in the error.

    """

    return sanitize_html('<div class="error">{}</div>'.format(str(message)))


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


def generate_theme_scss(themes: List[str], css_dir: str, gen_dir: str) -> None:
    """
    Generates an SCSS file based on the given theme names.

    NOTE: This function sorts the given theme list.

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

    :param themes: The list of theme names without .scss extension.
    :param css_dir: The directory where the theme files are located.
    :param gen_dir: The directory where the SCSS file should be generated.
    """
    themes.sort()
    for t in themes:
        if not os.path.exists(os.path.join(css_dir, t + '.scss')):
            raise Exception("Theme file not found: {}".format(t))
    combined = get_combined_css_filename(themes)
    if os.path.exists(os.path.join(gen_dir, combined + '.scss')):
        return
    if not os.path.exists(gen_dir):
        os.mkdir(gen_dir)
    with open(os.path.join(gen_dir, combined + '.scss'), encoding='utf-8', mode='w') as f:
        f.write('@charset "UTF-8";\n')
        f.write('@import "../variables";\n')
        for t in themes:
            f.write('@mixin {} {{}}\n'.format(t))
            f.write('@import "../css/{}";\n'.format(t))
        f.write('@import "../all";\n')
        for t in themes:
            f.write('@include {};\n'.format(t))


def get_combined_css_filename(themes: List[str]):
    """
    Returns the combined file name based on the given list of theme names.
    :param themes: The list of theme names without .scss extension.
    :return: The combined file name based on the themes. If the list is empty, 'default' is returned.
    """
    return '-'.join(themes) or 'default'
