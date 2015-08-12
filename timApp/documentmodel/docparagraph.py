import os

from contracts import contract, new_contract
from documentmodel.documentwriter import DocumentWriter
from markdownconverter import md_to_html
from .randutils import *
from timdb.timdbbase import TimDbException


class DocParagraphBase:
    pass


# This is so the DocParagraph contract can be used in the class itself
new_contract('DocParagraph', DocParagraphBase)


class DocParagraph(DocParagraphBase):
    @contract
    def __init__(
            self,
            md: 'str' = '',
            doc_id: 'int|None'=None,
            par_id: 'str|None'=None,
            t: 'str|None'=None,
            attrs: 'dict|None'=None,
            src_dict: 'dict|None'=None,
            files_root: 'str|None'=None):
        self.original = None
        if not attrs:
            attrs = {}
        self.files_root = self.get_default_files_root() if files_root is None else files_root
        self.doc_id = doc_id
        assert doc_id is not None
        if src_dict:
            # Create from JSON
            self.__data = src_dict
            return

        assert doc_id is not None
        self.__data = {
            'id': par_id if par_id is not None else random_id(),
            'md': md,
            'html': None,
            't': hashfunc(md) if md is not None else None,
            'links': [],
            'attrs': attrs}

        if par_id:
            # Try to read from file if we know the paragraph id
            self.__read()
            for attr in attrs or []:
                self.__data['attrs'][attr] = attrs[attr]

    def __iter__(self):
        return self.__data.__iter__()

    @classmethod
    def get_default_files_root(cls):
        return 'tim_files'

    @classmethod
    @contract
    def from_dict(cls, doc_id: 'int', d: 'dict', files_root: 'str|None' = None):
        return DocParagraph(doc_id=doc_id, src_dict=d, files_root=files_root)

    @classmethod
    @contract
    def _get_path(cls, doc_id: 'int', par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'str':
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', str(doc_id), par_id, t)

    @classmethod
    @contract
    def _get_base_path(cls, doc_id: 'int', par_id: 'str', files_root: 'str|None' = None) -> 'str':
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', str(doc_id), par_id)

    @classmethod
    @contract
    def get_latest(cls, doc_id: 'int', par_id: 'str', files_root: 'str|None' = None) -> 'DocParagraph':
        froot = cls.get_default_files_root() if files_root is None else files_root
        t = os.readlink(cls._get_path(doc_id, par_id, 'current', froot))
        return cls.get(doc_id, par_id, t, files_root=froot)

    @classmethod
    @contract
    def get(cls, doc_id: 'int', par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'DocParagraph':
        with open(cls._get_path(doc_id, par_id, t, files_root), 'r') as f:
            return cls.from_dict(doc_id, json.loads(f.read()), files_root=files_root)

    @contract
    def dict(self) -> 'dict':
        return self.__data

    @contract
    def get_doc_id(self) -> 'int':
        return self.doc_id

    @contract
    def get_id(self) -> 'str':
        return self.__data['id']

    @contract
    def is_different_from(self, par) -> 'bool':
        return self.get_hash() != par.get_hash() or self.get_attrs() != par.get_attrs()

    @contract
    def get_hash(self) -> 'str':
        return self.__data['t']

    @contract
    def get_markdown(self) -> 'str':
        return self.__data['md']

    @contract
    def get_exported_markdown(self) -> 'str':
        return DocumentWriter([self.__data], export_hashes=False, export_ids=False).get_text()

    @contract
    def get_html(self) -> 'str':
        if self.__data['html']:
            return self.__data['html']
        self.set_html(md_to_html(self.get_markdown()))
        return self.__data['html']

    @contract
    def get_ref_html(self, classname="parref", write_link=False) -> 'str':
        linkhtml = ''
        if write_link:
            from documentmodel.document import Document
            doc = Document(self.get_doc_id())
            linkhtml = '<a class="parlink" href="/view/{0}">{1}</a>'.format(self.get_doc_id(), doc.get_name())
        return """
            <div class="{0}">
                {1}
                {2}
            </div>
        """.format(classname, linkhtml, self.get_html())

    @contract
    def __get_html(self) -> 'str':
        if self.get_original() is None:
            return self.__get_html()

        # Referenced paragraph
        return """
            <div class="parref">
                <a class="parlink" href="{1}">Dokumentti {1}</a>
                {0}
            </div>
        """.format(self.__get_html(), self.get_doc_id())

    @contract
    def set_html(self, new_html: 'str'):
        self.__data['html'] = new_html

    @contract
    def get_links(self) -> 'list(str)':
        return self.__data['links']

    @contract
    def get_attrs(self) -> 'dict':
        return self.__data['attrs']

    @contract
    def get_attrs_str(self) -> 'str':
        return json.dumps(self.get_attrs())

    @contract
    def get_class_str(self) -> 'str':
        return ' '.join(self.get_attrs().get('classes', []))

    @contract
    def get_base_path(self) -> 'str':
        return self._get_base_path(self.get_doc_id(), self.get_id(), files_root=self.files_root)

    @contract
    def get_path(self) -> 'str':
        return self._get_path(self.get_doc_id(), self.get_id(), self.get_hash(), files_root=self.files_root)

    def __read(self):
        if not os.path.isfile(self.get_path()):
            return
        with open(self.get_path(), 'r') as f:
            self.__data = json.loads(f.read())

    def __write(self):
        file_name = self.get_path()
        should_exist = len(self.get_links()) > 0
        does_exist = os.path.isfile(file_name)

        if does_exist and not should_exist:
            os.unlink(file_name)
            base_path = self.get_base_path()
            if os.listdir(base_path) == ['current']:
                os.unlink(os.path.join(base_path, 'current'))
                os.rmdir(base_path)

        if not should_exist:
            return

        if not does_exist and should_exist:
            base_path = self.get_base_path()
            if not os.path.exists(base_path):
                os.makedirs(base_path)
            self.__set_latest()

        with open(file_name, 'w') as f:
            f.write(json.dumps(self.__data))

    def __set_latest(self):
        linkpath = self._get_path(self.get_doc_id(), self.get_id(), 'current', files_root=self.files_root)
        if linkpath == self.get_hash():
            return
        if os.path.isfile(linkpath):
            os.unlink(linkpath)
        os.symlink(self.get_hash(), linkpath)

    @contract
    def add_link(self, doc_id: 'int'):
        self.__read()
        self.__data['links'].append(str(doc_id))
        self.__write()

    @contract
    def remove_link(self, doc_id: 'int'):
        self.__read()
        self.__data['links'].remove(str(doc_id))
        self.__write()

    def update_links(self):
        self.__read()
        self.__write()

    def is_reference(self):
        return self.is_par_reference() or self.is_area_reference()

    def is_par_reference(self):
        attrs = self.get_attrs()
        return 'rd' in attrs and 'rp' in attrs

    def is_area_reference(self):
        attrs = self.get_attrs()
        return 'rd' in attrs and 'ra' in attrs

    def get_referenced_pars(self):
        attrs = self.get_attrs()
        from documentmodel.document import Document  # Document import needs to be here to avoid circular import
        try:
            ref_doc = Document(int(attrs['rd']))
        except ValueError as e:
            raise TimDbException('Invalid reference document id: "{}"'.format(attrs['rd']))
        if not ref_doc.exists():
            raise TimDbException('The referenced document does not exist.')
        if self.is_par_reference():
            if not ref_doc.has_paragraph(attrs['rp']):
                raise TimDbException('The referenced paragraph does not exist.')
            ref_par = ref_doc.get_paragraph(attrs['rp'])
            ref_par.set_original(self)
            ref_par.set_html(ref_par.get_ref_html(write_link=True))
            return [ref_par]
        elif self.is_area_reference():
            ref_pars = ref_doc.get_named_section(attrs['ra'])
            n = len(ref_pars)
            for p in ref_pars:
                p.set_html(p.get_ref_html(classname="parref-mid"))
                p.set_original(self)
            ref_pars[0].set_html(ref_pars[0].get_ref_html(classname="parref-begin", write_link=True))
            ref_pars[n-1].set_html(ref_pars[n-1].get_ref_html(classname="parref-end"))
            return ref_pars
        else:
            assert False

    def set_original(self, orig):
        self.original = orig

    def get_original(self):
        return self.original

    def is_plugin(self):
        return 'taskId' in self.get_attrs()
