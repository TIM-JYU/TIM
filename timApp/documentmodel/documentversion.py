from difflib import HtmlDiff
from typing import Optional, Tuple

from documentmodel.docsettings import DocSettings
from documentmodel.document import Document, DocParagraphIter
from documentmodel.docparagraph import DocParagraph


class DocumentVersion(Document):
    def __init__(self, doc_id: int, doc_ver: 'tuple(int, int)',
                 files_root=None, modifier_group_id: Optional[int] = 0):
        super(DocumentVersion, self).__init__(doc_id, files_root, modifier_group_id)
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
            with open(self.get_version_path(self.version), 'r') as f:
                while True:
                    line = f.readline()
                    if line == '':
                        self.indexlen = len(self.index)
                        return
                    entry = line.rstrip('\n').split('/')
                    if len(entry) > 1:
                        self.index[entry[0]] = entry[1]

    def __len__(self) -> int:
        self.cache_index()
        return self.indexlen

    def exists(self) -> bool:
        if self.does_exist is None:
            self.does_exist = Document.version_exists(self.doc_id, self.version, self.files_root)
        return self.does_exist

    def create(self, ignore_exists : bool = False):
        assert False, "Called DocumentVersion.create"

    @classmethod
    def remove(cls, doc_id: int, files_root: Optional[str] = None, ignore_exists=False):
        assert False, "Called DocumentVersion.remove"

    def get_version(self) -> Tuple[int, int]:
        return self.version

    def has_paragraph(self, par_id: str) -> bool:
        """
        Checks if the document has the given paragraph.
        :param par_id: The paragraph id.
        :return: Boolean.
        """
        self.cache_index()
        return par_id in self.index

    def get_paragraph(self, par_id: str) -> DocParagraph:
        self.cache_index()
        par_hash = self.index.get(par_id, None)
        if par_hash is not None:
            return DocParagraph.get(self, par_id, par_hash, self.files_root)
        else:
            return DocParagraph.get_latest(self.doc_id, par_id, self.files_root)

    def get_settings(self) -> DocSettings:
        if self.settings is None:
            self.settings = super(DocumentVersion, self).get_settings()
        return self.settings

    def add_paragraph_obj(self, p: DocParagraph) -> DocParagraph:
        assert False, "Called DocumentVersion.add_paragraph_obj"

    def add_paragraph(
            self,
            text: str,
            par_id: Optional[str]=None,
            attrs: Optional[dict]=None,
            properties: Optional[dict]=None
            ) -> DocParagraph:
        assert False, "Called DocumentVersion.add_paragraph"

    def add_ref_paragraph(self, src_par: DocParagraph, text: Optional[str] = None) -> DocParagraph:
        assert False, "Called DocumentVersion.add_ref_paragraph"

    def delete_paragraph(self, par_id: str):
        assert False, "Called DocumentVersion.delete_paragraph"

    def insert_paragraph(self, text: str,
                         insert_before_id: Optional[str],
                         attrs: Optional[dict]=None, properties: Optional[dict]=None,
                         par_id: Optional[str]=None) -> DocParagraph:
        assert False, "Called DocumentVersion.delete_paragraph"

    def modify_paragraph(self, par_id: str, new_text: str, new_attrs: Optional[dict]=None, new_properties: Optional[dict]=None) -> DocParagraph:
        assert False, "Called DocumentVersion.modify_paragraph"

    def update_section(self, text: str, par_id_first: str, par_id_last: str) -> Tuple[str, str]:
        assert False, "Called DocumentVersion.update_section"

    def update(self, text: str, original: str, strict_validation=True):
        assert False, "Called DocumentVersion.update"

    def modify_paragraph_obj(self, par_id: str, p: DocParagraph) -> DocParagraph:
        assert False, "Called DocumentVersion.modify_paragraph_obj"

    def insert_paragraph_obj(self, p: DocParagraph, insert_before_id: Optional[str]) -> DocParagraph:
        assert False, "Called DocumentVersion.insert_paragraph_obj"

    def delete_section(self, area_start: str, area_end: str):
        assert False, "Called DocumentVersion.delete_section"

    @staticmethod
    def get_diff(d1, d2):
        differ = HtmlDiff()
        return differ.make_file(d1.export_markdown().splitlines(),
                                d2.export_markdown().splitlines(),
                                context=True,
                                numlines=5)
