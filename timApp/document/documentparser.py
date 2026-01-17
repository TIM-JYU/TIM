import re

from timApp.document.attributeparser import AttributeParser
from timApp.document.documentparseroptions import DocumentParserOptions
from timApp.document.randutils import hashfunc, random_id, is_valid_id
from timApp.document.validationresult import (
    ValidationResult,
    AttributesAtEndOfCodeBlock,
    DuplicateParagraphId,
    InvalidParagraphId,
    DuplicateTaskId,
    MultipleAreasWithSameName,
    ZeroLengthArea,
    OverlappingClassedArea,
    AreaEndWithoutStart,
    DuplicateAreaEnd,
    AreaWithoutEnd,
    IllegalId,
)
from timApp.util.utils import count_chars_from_beginning, is_valid_tim_indetifier


class DocReader:
    """
    :type lines: list[str]
    :type i: int
    :type current_line: int
    :param lines:
    :param i:
    """

    def __init__(self, lines, i=0):
        self.lines = lines
        self.current_line = i

    def peek_line(self):
        """

        :rtype: str
        :return:
        """
        return self.lines[self.current_line]

    def get_line_and_advance(self):
        result = self.peek_line()
        self.current_line += 1
        return result

    def has_more_lines(self):
        return self.current_line < len(self.lines)


# python
def add_to_comma_map_str(m: dict, key, curr_id, sep: str = ", "):
    """
    Ensure m[key] is a comma-separated string.
    - If missing/None/"" -> set to curr_id
    - Accepts existing str or list values
    - Avoids duplicate entries
    - Always stores a str
    :type m: dict map with str values
    :type key: key in the map
    :type curr_id: value to add
    :type sep: optional separator, default ", "
    """
    if curr_id is None:
        curr_id = "None"

    curr = str(curr_id).strip()

    val = m.get(key)
    if val is None or (isinstance(val, str) and val.strip() == ""):
        m[key] = curr
        return

    m[key] = f"{val}{sep}{curr}"


class ErrorSet:
    def __init__(self):
        self._set = set()
        self._map = {}

    _MISSING = "__MISSING__"

    def add(self, err_id: str, par_id=_MISSING):
        if par_id == self._MISSING:
            self._set.add(err_id)
            val = self._map.get(err_id)
            if val is None:
                self._map[err_id] = "-"
        else:
            add_to_comma_map_str(self._map, err_id, par_id)

    @property
    def set(self):
        return self._set

    @property
    def map(self):
        return self._map


ATOM_PATTERN: re.Pattern[str] = re.compile(r'\batom\s*=\s*["\']')


class DocumentParser:
    """Splits documents into paragraphs.

    :type _blocks: list[dict]
    :type _doc_text: str
    :type _last_setting: tuple

    """

    def __init__(self, doc_text="", options: DocumentParserOptions | None = None):
        """

        :type doc_text: str
        """
        self._doc_text: str = doc_text
        self._blocks: [dict] = []
        self._break_on_empty_line = False
        self._last_setting: DocumentParserOptions | None = None
        self.options: DocumentParserOptions = (
            options if options is not None else DocumentParserOptions()
        )

    def get_blocks(self) -> [dict]:
        self._parse_document()
        return self._blocks

    def add_missing_attributes(
        self, hash_func=hashfunc, id_func=random_id, force_new_ids=False
    ):
        self._parse_document()
        for r in self._blocks:
            r["t"] = hash_func(r["md"], r["attrs"])
            if force_new_ids or not r.get("id"):
                r["id"] = id_func()
        return self

    def validate_structure(self) -> ValidationResult:
        self._parse_document()
        return self.do_validate_structure(self._blocks)

    @staticmethod
    def validate_end_of_code_block(md) -> bool:
        last_line = md[md.rindex("\n") + 1 :]
        num_ticks = count_chars_from_beginning(md, "`")
        if last_line.startswith("`" * num_ticks):
            attrs, start_index = AttributeParser(last_line).get_attributes()
            if start_index is not None:
                return False
        return True

    @staticmethod
    def do_validate_structure(blocks: [dict]) -> ValidationResult:
        found_ids = set()
        found_tasks = ErrorSet()
        found_areas = ErrorSet()
        classed_areas = []
        found_area_ends = ErrorSet()
        result = ValidationResult()
        for r in blocks:
            rtype = r.get("type")
            if rtype is None:
                rtype = DocumentParser.check_if_code(r["md"])
            if rtype == "code":
                md = r["md"]
                try:
                    if not DocumentParser.validate_end_of_code_block(md):
                        result.add_issue(AttributesAtEndOfCodeBlock(r.get("id")))
                except ValueError:
                    pass
            curr_id = r.get("id")
            if curr_id is not None:
                if curr_id in found_ids:
                    result.add_issue(DuplicateParagraphId(curr_id))
                found_ids.add(curr_id)
                if not is_valid_id(curr_id):
                    result.add_issue(InvalidParagraphId(curr_id))
            attrs = r.get("attrs", {})
            task_id = attrs.get("taskId")
            if task_id:
                if not is_valid_tim_indetifier(task_id):
                    result.add_issue(IllegalId(f"taskId '{task_id}'"))
                found_tasks.add(task_id, curr_id)
                if task_id in found_tasks.set:
                    result.add_issue(DuplicateTaskId(found_tasks.map[task_id], task_id))
                found_tasks.add(task_id)
            area = attrs.get("area")
            if area:
                if not is_valid_tim_indetifier(area):
                    result.add_issue(IllegalId(f"area name '{area}'"))
                found_areas.add(area, curr_id)
                if area in found_areas.set:
                    result.add_issue(
                        MultipleAreasWithSameName(found_areas.map[area], area)
                    )
                has_classes = len(attrs.get("classes", [])) > 0
                if has_classes:
                    classed_areas.append(area)
                found_areas.add(area)
            area_end = attrs.get("area_end")
            if area_end:
                found_area_ends.add(area_end, curr_id)
                if area_end == area:
                    result.add_issue(ZeroLengthArea(curr_id, area))
                if area_end in classed_areas:
                    if area_end != classed_areas[-1]:
                        result.add_issue(
                            OverlappingClassedArea(curr_id, classed_areas[-1], area_end)
                        )
                    classed_areas.pop()
                if area_end not in found_areas.set:
                    result.add_issue(AreaEndWithoutStart(curr_id, area_end))
                if area_end in found_area_ends.set:
                    result.add_issue(
                        DuplicateAreaEnd(found_area_ends.map[area_end], area_end)
                    )
                found_area_ends.add(area_end)
        unended_areas = found_areas.set - found_area_ends.set
        for a in unended_areas:
            result.add_issue(
                AreaWithoutEnd(found_areas.map[a], a)
            )  # TODO get the par id of the start
        return result

    def _parse_document(self):
        if self._last_setting == self.options:
            return
        self._blocks = []
        options = self.options
        self._break_on_empty_line = options.break_on_empty_line
        self._last_setting = options
        lines = self._doc_text.splitlines()
        doc = DocReader(lines)
        funcs = [
            self.try_parse_code_block,
            self.try_parse_header_block,
            self.parse_normal_block,
        ]
        while True:
            self.eat_whitespace(doc)
            if not doc.has_more_lines():
                break
            for func in funcs:
                result = func(doc)
                if result:
                    result["md"] = result["md"].rstrip().strip("\r\n")
                    if (
                        (
                            (
                                result["type"] == "code"
                                and not options.break_on_code_block
                            )
                            or (
                                result["type"] == "header"
                                and not options.break_on_header
                            )
                            or (
                                result["type"] == "autonormal"
                                and not options.break_on_normal
                            )
                        )
                        and (
                            not result.get("attrs")
                            or "code_lang" in result.get("attrs")
                        )
                        and len(self._blocks) > 0
                        and not self._blocks[-1].get("attrs", {}).get("plugin")
                        and self._blocks[-1]["type"] != "atom"
                    ):
                        self._blocks[-1]["md"] += "\n\n" + result["md"]
                    else:
                        if not result.get("attrs"):
                            result["attrs"] = {}
                        self._blocks.append(result)
                    if result["type"] == "code":
                        if not self.validate_end_of_code_block(result["md"]):
                            result["error"] = "Attributes at end of code block"
                    break

    @staticmethod
    def is_line_beginning_of_code_block(line: str):
        """
        is string a begining of code line
        :type line: str
        """
        if line.startswith("```"):
            code_start_char = "`"
        elif line.startswith("~~~"):
            code_start_char = "~"
        else:
            return False, None
        match = re.match("^" + code_start_char + "+", line).group(0)
        return True, match

    @staticmethod
    def is_beginning_of_code_block(doc):
        """

        :type doc: DocReader
        """
        return DocumentParser.is_line_beginning_of_code_block(doc.peek_line())

    @staticmethod
    def is_beginning_of_header_block(doc):
        return doc.peek_line().startswith("#")

    @staticmethod
    def is_empty_line(doc):
        """

        :type doc: DocReader
        """
        return doc.peek_line().isspace() or doc.peek_line() == ""

    @staticmethod
    def check_if_code(md) -> str:
        first_line_end = md.find("\n")
        if first_line_end == -1:
            first_line_end = len(md)
        first_line = md[:first_line_end]
        is_code, _ = DocumentParser.is_line_beginning_of_code_block(first_line)
        if not is_code:
            return ""
        if ATOM_PATTERN.search(first_line):
            return "atom"
        return "code"

    def try_parse_code_block(self, doc):
        """

        :type doc: DocReader
        :rtype: dict
        """
        is_code_block, code_block_marker = self.is_beginning_of_code_block(doc)
        if not is_code_block:
            return None
        start_line = doc.get_line_and_advance()
        block_lines = []
        tokens, start = AttributeParser(start_line).get_attributes()
        is_atom = tokens.get("atom", False)
        if is_atom:
            tokens.pop("atom")
        else:
            first_line = start_line[:start].strip()
            block_lines.append(first_line)
        line = None
        while True:
            if not doc.has_more_lines():
                break
            line = doc.get_line_and_advance()
            if line.startswith(code_block_marker):
                break
            block_lines.append(line)
        if not is_atom and line is not None and line.startswith(code_block_marker):
            block_lines.append(line)
        elif line is None and not is_atom:
            # If the document ended abruptly, we insert the code block end marker automatically
            block_lines.append(code_block_marker)
        elif not is_atom:
            # Fill an incomplete code block end marker if needed.
            # For example, the paragraph
            #
            # ```
            # a
            # `
            #
            # becomes
            #
            # ```
            # a
            # ```
            #
            single_mark = code_block_marker[0]
            last_line_code_chars = count_chars_from_beginning(
                block_lines[-1], single_mark
            )
            if (last_line_code_chars > 0 or len(line) == 0) and len(
                line.strip()
            ) == last_line_code_chars:
                block_lines[-1] = (
                    single_mark * (len(code_block_marker) - last_line_code_chars)
                    + block_lines[-1]
                )
            else:
                block_lines.append(code_block_marker)

        result = {"md": "\n".join(block_lines), "type": "atom" if is_atom else "code"}
        self.extract_attrs(result, tokens)
        return result

    def try_parse_header_block(self, doc):
        """

        :rtype: dict
        :type doc: DocReader
        :param doc:
        :return:
        """
        if not self.is_beginning_of_header_block(doc):
            return None
        header_line = doc.get_line_and_advance()
        block_lines = []
        tokens, start = AttributeParser(header_line).get_attributes()
        block_type = "normal"
        if not header_line.startswith("#-"):
            block_type = "header"
            block_lines.append(header_line[:start].strip())
        block_lines.append(self.parse_normal_block(doc)["md"])
        result = {"md": "\n".join(block_lines), "type": block_type}
        self.extract_attrs(result, tokens)
        return result

    def parse_normal_block(self, doc):
        """

        :type doc: DocReader
        """
        block_lines = []
        while doc.has_more_lines():
            if (
                self.is_beginning_of_header_block(doc)
                or self.is_beginning_of_code_block(doc)[0]
                or (self._break_on_empty_line and self.is_empty_line(doc))
            ):
                break
            block_lines.append(doc.get_line_and_advance())
        return {"md": "\n".join(block_lines), "type": "autonormal"}

    @staticmethod
    def extract_attrs(result, tokens):
        for builtin in ("id", "t"):
            if builtin in tokens:
                result[builtin] = tokens.pop(builtin)
        if len(tokens) > 0:
            result["attrs"] = tokens

    def eat_whitespace(self, doc):
        """

        :rtype: NoneType
        :type doc: DocReader
        """
        while doc.has_more_lines() and self.is_empty_line(doc):
            doc.get_line_and_advance()
        return None
