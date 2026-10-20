import re
from copy import deepcopy
from enum import Enum
from textwrap import shorten
from typing import Generator

# noinspection PyPackageRequirements
import yaml

# noinspection PyPackageRequirements
from yaml import YAMLError, CSafeLoader, Event, AliasEvent, NodeEvent

# noinspection PyPackageRequirements
from yaml.scanner import ScannerError

from timApp.util.utils import count_chars_from_beginning


class BlockEndMissingError(YAMLError):
    def __init__(self, end_str: str) -> None:
        super().__init__(f"Missing multiline terminator: {end_str}")
        self.end_str = end_str


class DuplicateKeyMergeHintError(YAMLError):
    def __init__(self, key: str):
        super().__init__(
            f'Using merge hints in a key ("{key}") having same name in different levels is not currently supported'
        )


class InvalidIndentError(YAMLError):
    def __init__(self, line: str):
        super().__init__(
            f'The line "{shorten(line, width=30, placeholder="...")}" '
            f"must be indented at least as much as the first line."
        )


class MergeStyle(Enum):
    Replace = "r"
    Append = "a"
    ReplaceIfNotExist = "r?"
    ReplaceOnlyIfExists = "r!"


MERGE_HINT_VALUES = {m.value for m in MergeStyle}

MERGE_HINT_PATTERN = "|".join(
    re.escape(m.value) for m in sorted(MergeStyle, key=lambda m: -len(m.value))
)

YamlMergeInfo = dict[str, MergeStyle]

yaml_loader = CSafeLoader


class YamlBlock:
    def __init__(self, values: dict = None, merge_hints: YamlMergeInfo | None = None):
        self.values = values if values is not None else {}
        self.merge_hints = merge_hints

    def __eq__(self, o: object) -> bool:
        if isinstance(o, self.__class__):
            return self.__dict__ == o.__dict__
        elif isinstance(o, dict):
            return self.values == o
        return NotImplemented

    def __repr__(self):
        return f"{self.__class__.__name__}({self.__dict__})"

    def __setitem__(self, key: str, value):
        self.values.__setitem__(key, value)

    def __getitem__(self, item: str):
        return self.values.__getitem__(item)

    def get(self, key: str, default=None):
        return self.values.get(key, default)

    def pop(self, key: str, default=None):
        return self.values.pop(key, default)

    @staticmethod
    def from_markdown(md: str):
        md = strip_code_block(md)
        values, hints = parse_yaml(md)
        return YamlBlock(values=values, merge_hints=hints)

    def merge_with(
        self, other: "YamlBlock", do_merge_hints: bool = False
    ) -> "YamlBlock":
        new_vals = deepcopy(self.values)
        merge(new_vals, other.values, other.merge_hints)
        merge_hints = other.merge_hints
        if do_merge_hints:
            merge_hints = (self.merge_hints or {}) | (other.merge_hints or {})
        return YamlBlock(values=new_vals, merge_hints=merge_hints)

    def to_markdown(self):
        return yaml.dump(self.values, default_flow_style=False)


missing_space_after_colon = re.compile(
    "^[ \t]*[^ :[\\]()'\"]*:[^ /=:]"
)  # kissa:istuu mutta ei http://koti tai a:=5
multiline_unindented_string = re.compile(
    rf"""^( *)([^ :"']+): *([|>][+-]?)([0-9]*) *([^ 0-9+-]+[^ ]*)( +#?({MERGE_HINT_PATTERN})(?= |$).*| +#.*)? *$"""
)
normal_multiline_indented_string = re.compile(
    """^( *)([^ :"']+): *([|>][+-]?)([0-9]*) *$"""
)  # program: | or  program: |+2
multiline_unindented_obj_string = re.compile(
    """^( *)([^ :"']+): *@ *([^ 0-9+-]+[^ ]*)$"""
)  # object: @|| or  object: @!!!
merge_hint_comment = re.compile(
    rf"(?:^|\s)#({MERGE_HINT_PATTERN})(?:\s|$)"
)  # a: 1 #r jotain


def strip_code_block(md: str) -> str:
    code_block_marker = get_code_block_str(md)
    if len(code_block_marker) < 3:
        return md
    # Strip only after verifying we have a code block, otherwise we might break YAML spacing
    md = md.strip()
    return md.split("\n", 1)[1].rstrip(f"\n{code_block_marker}")


def get_code_block_str(md: str) -> str:
    md = md.lstrip()  # Strip to ensure correct count
    code_block_marker = "`" * count_chars_from_beginning(md, "`")
    return code_block_marker


def compare_same(s1: str, s2: str, n: int) -> bool:
    """
    :param s1: string than can contain max n spaces at the beginning
    :param s2: string to oompare, no spaces in the beginning
    :param n: how many spaces allowed to caintain still to be same
    :return: True is same False other
    """
    if not n:
        n = 0
    i = s1.find(s2)
    if i < 0 or i > n:  # No match or too far
        return False
    if i + len(s2) != len(s1):  # The end part is not exactly same
        return False
    return count_chars_from_beginning(s1, " ") <= n


def correct_obj(text: str) -> str:
    """
    Also gives another way to write unindented object attributes, by starting
    the attribute like: `object: @!!`  (`!!` could any combinations of chars except space)
    and ending it by `!!` in first column.

    :param text: Text to convert to proper YAML.
    :return: Text that is proper YAML obj
    """
    """
     Problem analyze:
        il = attribute line indent len,   fi = first line indent
        indent = string that must be inserted to every line, len = it's length

        |a1: @!            il = 0, fi = 0     il-fi+1  = 1 
        |a            => indent=" "   len 1 

        |a1: @!            il = 0, fi = 4     il-fi+1  = -3 
        |    a        => indent=""    len 0

        |a1: @!            il = 0, fi = 1     il-fi+1  = 2
        | a           => indent=""    len 0

        | a1: @!           il = 1  fi = 1     il-fi+1  = 2
        | a           => indent=" "   len 1

        |  a1: @!          il = 2  fi = 0     il-fi+1  = 2
        |a            => indent="   " len 3

        |  a1: @!          il = 2  fi = 1     il-fi+1  = 2  
        | a           => indent="  "  len 2

    Note: This is almost the same as correct_yaml-body but no hint handling.
    It may be possible to put them just one code but then it is even more difficult to
    understand what is happening.  One small difference is that in this code we look for @
    and do not put it back as we do with | in correct_yaml.

    And the idea is to run this as many times as there is no changes any more in object level.
    The objects could be nested, string can not be.  There is still a problem that if string
    includes object start, this function will destroy it, f.ex:

        |a1: |!!
        |cat
        |  o1: @!
        |a:1
        |!
        |!!

    even should be    

        |a1: |
        | cat
        |   o1: @!
        | a:1

    There are testcases for all these things in timApp/tests/unit/test_correct_yaml.py

    """
    # don't use splitlines here - it loses the possible last trailing newline character, and we don't want that.
    while True:  # repeat until n == 0
        lines = text.split("\n")
        n = 0  # count how many unindented object block found and how many handled
        s = ""
        multiline = False
        multiline_string = False
        end_str = ""
        indent = ""
        multiline_first_indent = None
        original_indent_len = 0
        max_allowed_spaces = 0
        lf = ""
        for line in lines:
            line = line.rstrip()
            r2 = multiline_unindented_string.match(line)
            if (
                r2 and not multiline
            ):  # we have multiline string, and we do nothing until it ends
                end_str = r2.group(5)
                indent = r2.group(1)
                max_allowed_spaces = original_indent_len = len(indent)
                indent = ""  # no changes while in multilinestring
                multiline_string = multiline = True
                multiline_first_indent = None
                s = s + lf + line
                lf = "\n"
                continue
            else:
                r = multiline_unindented_obj_string.match(line)
                if r and not multiline_string:
                    n += 1
                if r and not multiline:
                    end_str = r.group(3)
                    indent = r.group(1)
                    max_allowed_spaces = original_indent_len = len(indent)
                    multiline = True
                    multiline_first_indent = None
                    line, _ = line.split("@", 1)
                    s = s + lf + line.rstrip()
                    lf = "\n"
                    continue
            if multiline:
                if compare_same(line, end_str, max_allowed_spaces):
                    multiline = False
                    if multiline_string:
                        s = s + lf + line
                        multiline_string = False
                    else:  # one more unindented object handled
                        n -= 1
                    continue
                if multiline_first_indent is None:
                    multiline_first_indent = count_chars_from_beginning(line, " ")
                    needed_indent_length = max(
                        original_indent_len - multiline_first_indent + 1, 0
                    )
                    if (
                        not multiline_string
                    ):  # we do not touch multiline strings in this function
                        indent = " " * needed_indent_length
                    max_allowed_spaces = original_indent_len + multiline_first_indent
                else:
                    if line and multiline_first_indent > count_chars_from_beginning(
                        line, " "
                    ):
                        raise InvalidIndentError(line)
                line = indent + line
            s = s + lf + line
            lf = "\n"
        if multiline:
            raise BlockEndMissingError(end_str)
        if n == 0:
            return s
        text = s
    # until n = 0


def correct_yaml(text: str) -> tuple[str, YamlMergeInfo]:
    """Inserts missing spaces after `:` Like  `width:20` => `width: 20`
    Also gives another way to write multiline attributes, by starting
    the multiline like: `program: |!!`  (`!!` could any combinations of chars except space)
    and ending it by `!!` in first column.

    :param text: Text to convert to proper YAML.
    :return: Text that is proper YAML.
    """
    # don't use splitlines here - it loses the possible last trailing newline character, and we don't want that.
    if (
        text.find(":@") >= 0
    ):  # we suppose that using this is so rare that it is cheaper to avoid
        text = correct_obj(text)  # this call as much as possible
    lines = text.split("\n")
    s = ""
    multiline = False
    end_str = ""
    indent = ""
    merge_hints = {}
    encountered_keys = set()
    multiline_first_indent = None
    original_indent_len = 0
    max_allowed_spaces = 0
    lf = ""
    end_match = None
    multiline_ended_last = False
    for line in lines:
        multiline_ended_last = False
        line = line.rstrip()
        if end_match:  # to protect : space insertion while in normal | or >
            if not end_match.match(line):
                s = s + lf + line
                lf = "\n"
                continue
            end_match = None
        if missing_space_after_colon.match(line) and not multiline:
            line = line.replace(":", ": ", 1)
        r = normal_multiline_indented_string.match(line)
        if r and not multiline:
            indent = r.group(1)
            spacereg = ""
            spaces = len(indent)
            if spaces != 0:
                spacereg = " {0," + str(spaces) + "}"
            em = "^" + spacereg + "[^ ]+.*$"
            end_match = re.compile(em)
            s = s + lf + line
            lf = "\n"
            continue
        r = multiline_unindented_string.match(line)
        if r and not multiline:
            block_style_indicator = str(r.group(3)) or ""
            end_str = r.group(5)
            indent = r.group(1)
            key_indent_len = len(indent)
            multiline_first_indent = None
            indent_indicator = 0
            indent_indicator_string = ""
            if r.group(4):
                indent_indicator_string = r.group(4)
                indent_indicator = int(indent_indicator_string)
                if indent_indicator <= 0:
                    indent_indicator_string = "1"
                multiline_first_indent = 0
                needed_indent_length = max(key_indent_len - indent_indicator + 1, 0)
                # needed_indent_length = original_indent_len + indent_indicator
                indent = " " * needed_indent_length
            original_indent_len = len(indent)
            max_allowed_spaces = original_indent_len + indent_indicator
            multiline = True
            line = line[: r.start(3)]
            key = r.group(2)
            hint = r.group(7)
            if hint in MERGE_HINT_VALUES:
                if key in encountered_keys:
                    raise DuplicateKeyMergeHintError(key)
                merge_hints[key] = MergeStyle(hint)
            s = s + lf + line + block_style_indicator + indent_indicator_string
            lf = "\n"
            encountered_keys.add(key)
            continue
        if multiline:
            if compare_same(line, end_str, max_allowed_spaces):
                multiline = False
                multiline_ended_last = True
                continue
            if multiline_first_indent is None:
                multiline_first_indent = count_chars_from_beginning(line, " ")
                needed_indent_length = max(
                    original_indent_len - multiline_first_indent + 1, 0
                )
                indent = " " * needed_indent_length
                max_allowed_spaces = original_indent_len + multiline_first_indent
            else:
                if line and multiline_first_indent > count_chars_from_beginning(
                    line, " "
                ):
                    raise InvalidIndentError(line)
            line = indent + line
        else:
            key, _, rest = (s.strip() for s in line.partition(":"))
            if key and not key[0] == "#":  # not a comment
                if key in encountered_keys and key in merge_hints:
                    raise DuplicateKeyMergeHintError(key)
                if hm := merge_hint_comment.search(rest):
                    merge_hints[key] = MergeStyle(hm.group(1))
                encountered_keys.add(key)
        s = s + lf + line
        lf = "\n"
    if multiline:
        raise BlockEndMissingError(end_str)
    if multiline_ended_last:
        s += "\n"
    return s, merge_hints


def verify_anchor_depth(text: str, max_depth=3) -> None:
    """
    Verifies that the given YAML file does not include too deep anchor references.

    Anchor references can be used for quadratic growth DoS attacks when YAML is being iterated through.
    The method verifies that the maximum reference depth is within the provided value. Default max depth is 3.

    If YAML includes deep references, YAMLError is thrown.

    :param text: YAML to check
    :param max_depth: Maximum anchor reference depth
    """
    parser: Generator[Event] = yaml.parse(text, yaml_loader)
    context_depths = {}
    current_context = None
    for p in parser:
        if isinstance(p, AliasEvent) and current_context:
            depths = context_depths[current_context]
            if p.anchor not in depths:
                depth = max([*context_depths[p.anchor].values(), 0]) + 1
                if depth > max_depth:
                    raise YAMLError("Markup includes too deep anchor references")
                depths[p.anchor] = depth
            continue
        if isinstance(p, NodeEvent) and p.anchor is not None:
            context_depths[p.anchor] = {}
            current_context = p.anchor


def parse_yaml(text: str) -> tuple[dict, YamlMergeInfo]:
    """Parses the specified text as (customized) YAML.

    :param text: The text to parse.
    :return: The parsed YAML as a dict.
    """

    text, hints = correct_yaml(text)

    try:
        verify_anchor_depth(text)
        values = yaml.load(text, yaml_loader)
    except ValueError as e:
        raise YAMLError("Invalid YAML: " + str(e)) from e
    except ScannerError as e:
        cm = e.context_mark
        pm = e.problem_mark

        context_line = cm.line if cm else None
        # context_col = cm.column if cm else None
        problem_line = pm.line if pm else None
        # problem_col = pm.column if pm else None
        rows = text.split("\n")
        row = ""
        if context_line is not None and context_line < len(rows):
            row += f"<br>\n{context_line + 1}: {rows[context_line]}"

        if problem_line is not None and problem_line < len(rows):
            row += f"<br>\n{problem_line + 1}: {rows[problem_line]}"
        raise YAMLError("Invalid YAML: " + str(e) + str(row)) from e

    if isinstance(values, str):
        raise YAMLError("Markup must not be a mere string.")
    # empty YAML is equal to null, so we avoid that by returning {} in that case
    return (values or {}), hints


def merge(a: dict, b: dict, merge_info: YamlMergeInfo | None = None):
    """Merges two dictionaries recursively. Stores the result in the first dictionary.

    :param merge_info: The merge hints to use while merging.
    :param a: The first dictionary.
    :param b: The second dictionary.

    """
    __merge_helper(a, b, 0, merge_info=merge_info)


default_append_keys = {"css", "themes", "globalmacros", "ADDFOREVERY"}


def __merge_helper(
    a: dict, b: dict, depth: int = 0, merge_info: YamlMergeInfo | None = None
):
    for key in b:
        hint = merge_info.get(key) if merge_info else None
        if key not in a:
            if hint != MergeStyle.ReplaceOnlyIfExists:
                a[key] = b[key]
            continue
        if hint == MergeStyle.ReplaceIfNotExist:
            continue  # avain on jo olemassa, joten a:n arvo säilyy
        if hint == MergeStyle.Replace or hint == MergeStyle.ReplaceOnlyIfExists:
            a[key] = b[key]
            continue
        if isinstance(a[key], dict) and isinstance(b[key], dict):
            __merge_helper(a[key], b[key], depth + 1, merge_info)
            continue
        if a[key] == b[key]:
            continue
        if type(a[key]) is not type(b[key]):
            a[key] = b[key]
            continue
        if hint == MergeStyle.Append or key in default_append_keys:
            if isinstance(a[key], set):
                a[key] |= b[key]
            else:
                a[key] += b[key]
            continue
        a[key] = b[key]
