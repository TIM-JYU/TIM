from difflib import HtmlDiff
from typing import Optional

from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.document.document import Document
from timApp.document.preloadoption import PreloadOption
from timApp.document.version import Version


class DocumentVersion(Document):
    def __init__(
        self,
        doc_id: int,
        doc_ver: Version,
        modifier_group_id: int | None = 0,
        preload_option=PreloadOption.none,
    ):
        super().__init__(doc_id, modifier_group_id, preload_option)
        self.does_exist = None
        self.settings = None
        self.version = doc_ver
        self.index = None
        self.pars = None
        self.dict = None
        self.indexlen = 0

    def cache_index(self):
        if self.index is None:
            self.index = {}
            with open(self.get_version_path(self.version)) as f:
                while True:
                    line = f.readline()
                    if line == "":
                        self.indexlen = len(self.index)
                        return
                    entry = line.rstrip("\n").split("/")
                    if len(entry) > 1:
                        self.index[entry[0]] = entry[1]

    def __len__(self) -> int:
        self.cache_index()
        return self.indexlen

    def exists(self) -> bool:
        if self.does_exist is None:
            self.does_exist = Document.version_exists(self.doc_id, self.version)
        return self.does_exist

    def create(self, ignore_exists: bool = False):
        assert False, "Called DocumentVersion.create"

    @classmethod
    def remove(cls, doc_id: int, ignore_exists=False):
        assert False, "Called DocumentVersion.remove"

    def get_version(self) -> Version:
        return self.version

    def has_paragraph(self, par_id: str) -> bool:
        """Checks if the document has the given paragraph.

        :param par_id: The paragraph id.
        :return: Boolean.

        """
        self.cache_index()
        return par_id in self.index

    def get_paragraph(self, par_id: str) -> DocParagraph:
        self.cache_index()
        par_hash = self.index.get(par_id, None)
        if par_hash is not None:
            return DocParagraph.get(self, par_id, par_hash)
        else:
            return DocParagraph.get_latest(self.doc_id, par_id)

    def get_settings(self) -> DocSettings:
        if self.settings is None:
            self.settings = super().get_settings()
        return self.settings

    def add_paragraph_obj(self, p: DocParagraph) -> DocParagraph:
        raise Exception("Called DocumentVersion.add_paragraph_obj")

    def add_paragraph(
        self, text: str, par_id: str | None = None, attrs: dict | None = None
    ) -> DocParagraph:
        raise Exception("Called DocumentVersion.add_paragraph")

    def add_ref_paragraph(
        self, src_par: DocParagraph, text: str | None = None
    ) -> DocParagraph:
        raise Exception("Called DocumentVersion.add_ref_paragraph")

    def delete_paragraph(self, par_id: str):
        assert False, "Called DocumentVersion.delete_paragraph"

    def insert_paragraph(
        self,
        text: str,
        insert_before_id: str | None = None,
        insert_after_id: str | None = None,
        attrs: dict | None = None,
        par_id: str | None = None,
    ) -> DocParagraph:
        raise Exception("Called DocumentVersion.delete_paragraph")

    def modify_paragraph(
        self, par_id: str, new_text: str, new_attrs: dict | None = None
    ) -> DocParagraph:
        raise Exception("Called DocumentVersion.modify_paragraph")

    def update_section(
        self, text: str, par_id_first: str, par_id_last: str
    ) -> tuple[str, str]:
        raise Exception("Called DocumentVersion.update_section")

    def update(self, text: str, original: str, strict_validation=True):
        raise Exception("Called DocumentVersion.update")

    def modify_paragraph_obj(self, par_id: str, p: DocParagraph) -> DocParagraph:
        raise Exception("Called DocumentVersion.modify_paragraph_obj")

    def insert_paragraph_obj(
        self,
        p: DocParagraph,
        insert_before_id: str | None = None,
        insert_after_id: str | None = None,
    ) -> DocParagraph:
        raise Exception("Called DocumentVersion.insert_paragraph_obj")

    def delete_section(self, area_start: str, area_end: str):
        raise Exception("Called DocumentVersion.delete_section")

    @staticmethod
    def get_diff(d1, d2):
        differ = HtmlDiff()
        diffhtml = differ.make_file(
            d1.export_markdown().splitlines(),
            d2.export_markdown().splitlines(),
            context=True,
            numlines=5,
        )
        # Fix for https://github.com/python/cpython/issues/131204
        # TODO: remove this if we update Python to version 3.14+
        old_font_style = "table.diff {font-family:Courier; border:medium;}"
        fixed_font_style = "table.diff {font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace; border:medium;}"
        diffhtml = diffhtml.replace(old_font_style, fixed_font_style)
        return diffhtml
