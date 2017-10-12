import re
from enum import Enum
from typing import Dict, Optional, Tuple

import yaml
from copy import deepcopy
from yaml import CLoader, YAMLError

from timApp.utils import count_chars


class BlockEndMissingError(YAMLError):
    def __init__(self, end_str: str) -> None:
        super().__init__(f'Missing multiline terminator: {end_str}')
        self.end_str = end_str


class MergeStyle(Enum):
    Replace = 'r'
    Append = 'a'
    ReplaceIfNotExist = 'r?'


YamlMergeInfo = Dict[str, MergeStyle]

yaml_loader = CLoader


class YamlBlock:
    def __init__(self, values: dict = None, merge_hints: Optional[YamlMergeInfo] = None):
        self.values = values if values is not None else {}
        self.merge_hints = merge_hints

    def __eq__(self, o: object) -> bool:
        if isinstance(o, self.__class__):
            return self.__dict__ == o.__dict__
        elif isinstance(o, dict):
            return self.values == o
        return NotImplemented

    def __ne__(self, o: object) -> bool:
        if isinstance(o, self.__class__):
            return not self == o
        return NotImplemented

    def __repr__(self):
        return f'{self.__class__.__name__}({self.__dict__})'

    def __setitem__(self, key: str, value):
        self.values.__setitem__(key, value)

    def __getitem__(self, item: str):
        return self.values.__getitem__(item)

    def get(self, key: str, default=None):
        return self.values.get(key, default)

    @staticmethod
    def from_markdown(md: str):
        md = strip_code_block(md)
        values, hints = parse_yaml(md)
        return YamlBlock(values=values, merge_hints=hints)

    def merge_with(self, other: 'YamlBlock'):
        new_vals = deepcopy(self.values)
        merge(new_vals, other.values, other.merge_hints)
        return YamlBlock(values=new_vals, merge_hints=other.merge_hints)

    def to_markdown(self):
        return yaml.dump(self.values, default_flow_style=False)


missing_space_after_colon = re.compile("^[ \t]*[^ :]*:[^ ]")  # kissa:istuu
multiline_unindented_string = re.compile(
    """^( *)([^ :"']+): *\| *([^ 0-9+-]+[^ ]+)( (a|r|r\?))? *$""")  # program: ||| or  program: |!!!


def strip_code_block(md: str) -> str:
    code_block_marker = get_code_block_str(md)
    if len(code_block_marker) < 3:
        return md
    return md.split('\n', 1)[1].rstrip(f'\n{code_block_marker}')


def get_code_block_str(md):
    code_block_marker = '`' * count_chars(md, '`')
    return code_block_marker


def correct_yaml(text: str) -> Tuple[str, YamlMergeInfo]:
    """Inserts missing spaces after `:` Like  `width:20` => `width: 20`
    Also gives an other way to write multiline attributes, by starting
    the multiline like: `program: |!!`  (`!!` could be any number and any non a-z,A-Z chars
    and ending it by `!!` in first column.

    :param text: Text to convert to proper yaml.
    :return: Text that is proper yaml.
    """
    # don't use splitlines here - it loses the possible last trailing newline character, and we don't want that.
    lines = text.split('\n')
    s = ""
    multiline = False
    end_str = ''
    indent = None
    merge_hints = {}
    for line in lines:
        line = line.rstrip()
        if missing_space_after_colon.match(line) and not multiline:
            line = line.replace(':', ': ', 1)
        r = multiline_unindented_string.match(line)
        if r and not multiline:
            end_str = r.group(3)
            indent = ' ' + r.group(1)
            multiline = True
            line, _ = line.split('|', 1)
            key = r.group(2)
            hint = r.group(5)
            if hint in ('a', 'r', 'r?'):
                merge_hints[key] = MergeStyle(hint)
            s = s + '\n' + line + '|'
            continue
        if multiline:
            if line == end_str:
                multiline = False
                continue
            line = indent + line
        s = s + '\n' + line
    if multiline:
        raise BlockEndMissingError(end_str)
    return s, merge_hints


def parse_yaml(text: str) -> Tuple[dict, YamlMergeInfo]:
    """Parses the specified text as (customized) YAML.

    :param text: The text to parse.
    :return: The parsed YAML as a dict.
    """

    text, hints = correct_yaml(text)
    values = yaml.load(text, yaml_loader)
    if isinstance(values, str):
        raise YAMLError('Markup must not be a mere string.')
    # empty YAML is equal to null, so we avoid that by returning {} in that case
    return (values or {}), hints


def merge(a: dict, b: dict, merge_info: Optional[YamlMergeInfo] = None):
    """Merges two dictionaries recursively. Stores the result in the first dictionary.

    :param merge_info: The merge hints to use while merging.
    :param a: The first dictionary.
    :param b: The second dictionary.

    """
    return __merge_helper(a, b, 0, merge_info=merge_info)


def __merge_helper(a: dict, b: dict, depth: int = 0, merge_info: Optional[YamlMergeInfo] = None):
    for key in b:
        if key in a:
            if isinstance(a[key], dict) and isinstance(b[key], dict):
                __merge_helper(a[key], b[key], depth + 1)
            elif a[key] == b[key]:
                pass
            elif type(a[key]) != type(b[key]):
                a[key] = b[key]
            else:
                if depth == 0 and merge_info:
                    m = merge_info.get(key, MergeStyle.Replace)
                    if m == MergeStyle.Replace:
                        a[key] = b[key]
                    elif m == MergeStyle.Append:
                        a[key] += b[key]
                else:
                    a[key] = b[key]
        else:
            a[key] = b[key]
