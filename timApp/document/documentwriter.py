"""Defines the DocumentWriter class."""
from typing import Optional

from timApp.document.documentparser import DocumentParser
from timApp.document.documentparseroptions import DocumentParserOptions
from timApp.util.utils import count_chars_from_beginning


class DocumentWriter:
    """Converts a sequence of document blocks to text."""

    def __init__(self, pars, export_hashes=False, export_ids=True):
        """Initializes a DocumentWriter object.

        :param export_hashes: Whether to include hash attributes in the exported markdown.
        :param export_ids: Whether to include id attributes in the exported markdown.
        :param pars: A sequence of paragraphs representing the document.

        """
        self.pars = pars
        self.ignored_attrs = [
            "md",
            "type",
            "html",
            "links",
            "doc_id",
            "props",
            "h",
            "code_lang",
        ]
        if not export_hashes:
            self.ignored_attrs.append("t")
        if not export_ids:
            self.ignored_attrs.append("id")

    def get_text(self, options: Optional[DocumentParserOptions] = None):
        """Gets the full text for the document.

        :return: The full text of the document.

        """
        if options is None:
            options = DocumentParserOptions()
        text = ""
        for p in self.pars:
            blocks = DocumentParser(p["md"], options=options).get_blocks()
            text += "\n"
            if len(blocks) > 1:
                atomized = p.copy()
                atomized["atom"] = "true"
                num_ticks = 3
                for b in blocks:
                    if b["type"] == "code":
                        num_ticks = count_chars_from_beginning(b["md"], "`") + 1
                text += (
                    "`" * num_ticks
                    + " {"
                    + self.attrs_to_str(atomized)
                    + "}\n"
                    + p["md"]
                    + "\n"
                    + "`" * num_ticks
                )
            else:
                attrs_str = self.attrs_to_str(p)
                if not attrs_str:
                    text += p["md"]
                else:
                    if (
                        len(blocks) == 0
                        or blocks[0]["type"] == "normal"
                        or blocks[0]["type"] == "autonormal"
                    ):
                        text += "#-" + " {" + attrs_str + "}\n" + p["md"]
                    else:
                        parts = blocks[0]["md"].split("\n", 1)
                        first_line, rest = parts[0], parts[1] if len(parts) > 1 else ""
                        text += first_line + " {" + attrs_str + "}\n" + rest
            if text[-1] != "\n":
                text += "\n"
        return text[1:]

    def attrs_to_str(self, attrs):
        """

        :type attrs: dict
        """
        attr_str = ""
        for k, v in attrs.items():
            if k in self.ignored_attrs:
                continue
            elif k == "taskId":
                attr_str += "#" + v
            elif k == "classes":
                attr_str += " ".join(["." + cl for cl in v])
            elif isinstance(v, dict):
                attr_str += self.attrs_to_str(v)
            else:
                attr_str += k + '="'
                for char in str(v):
                    if char in ('"', "\\"):
                        attr_str += "\\"
                    attr_str += char
                attr_str += '"'
            attr_str += " "
        return attr_str.strip()
