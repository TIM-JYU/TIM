import os

from contracts import contract, new_contract
from markdownconverter import md_to_html
from .randutils import *

class DocParagraph:
    @contract
    def __init__(
            self,
            md: 'str' = '',
                 par_id: 'str|None'=None,
                 t: 'str|None'=None,
                 attrs: 'dict|None'=None,
                 src_dict: 'dict|None'=None,
                 files_root: 'str|None'=None):

        if not attrs:
            attrs = {}
        self.files_root = self.get_default_files_root() if files_root is None else files_root
        if src_dict:
            # Create from JSON
            self.__data = src_dict
            return

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
    def from_dict(cls, d: 'dict', files_root: 'str|None' = None):
        return DocParagraph(src_dict=d, files_root=files_root)

    @classmethod
    @contract
    def _get_path(cls, par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'str':
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', par_id, t)

    @classmethod
    @contract
    def _get_base_path(cls, par_id: 'str', files_root: 'str|None' = None) -> 'str':
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', par_id)

    @classmethod
    @contract
    def get_latest(cls, par_id: 'str', files_root: 'str|None' = None):
        froot = cls.get_default_files_root() if files_root is None else files_root
        t = os.readlink(cls._get_path(par_id, 'current', froot))
        return cls.get(par_id, t, files_root=froot)

    @classmethod
    @contract
    def get(cls, par_id: 'str', t: 'str', files_root: 'str|None' = None):
        with open(cls._get_path(par_id, t, files_root), 'r') as f:
            return cls.from_dict(json.loads(f.read()), files_root=files_root)

    @contract
    def dict(self) -> 'dict':
        return self.__data

    @contract
    def get_id(self) -> 'str':
        return self.__data['id']

    @contract
    def get_hash(self) -> 'str':
        return self.__data['t']

    @contract
    def get_markdown(self) -> 'str':
        return self.__data['md']

    @contract
    def get_html(self) -> 'str':
        if self.__data['html']:
            return self.__data['html']
        self.set_html(md_to_html(self.get_markdown()))
        return self.__data['html']

    @contract
    def set_html(self, new_html: 'str'):
        self.__data['html'] = new_html

    @contract
    def get_links(self) -> 'list(int)':
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
        return self._get_base_path(self.get_id(), files_root=self.files_root)

    @contract
    def get_path(self) -> 'str':
        return self._get_path(self.get_id(), self.get_hash(), files_root=self.files_root)

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
        linkpath = self._get_path(self.get_id(), 'current', files_root=self.files_root)
        if linkpath == self.get_hash():
            return
        if os.path.isfile(linkpath):
            os.unlink(linkpath)
        os.symlink(self.get_hash(), linkpath)

    @contract
    def add_link(self, doc_id: 'int'):
        self.__read()
        self.__data['links'].append(doc_id)
        self.__write()

    @contract
    def remove_link(self, doc_id: 'int'):
        self.__read()
        self.__data['links'].remove(doc_id)
        self.__write()

    def update_links(self):
        self.__read()
        self.__write()

new_contract('DocParagraph', DocParagraph)
