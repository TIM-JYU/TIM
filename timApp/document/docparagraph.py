import os

from contracts import contract, new_contract
from .randutils import *

class DocParagraph:
    @contract
    def __init__(
            self,
            md: 'str' = '',
            html: 'str|None' = None,
            par_id: 'str|None' = None,
            t: 'str|None' = None,
            links: 'list|None' = None,
            attrs: 'dict|None' = None,
            src_dict: 'dict|None' = None,
            files_root: 'str|None' = None):
        self.__data = {
            'md': md,
            'html': html,
            'id': random_id() if par_id is None else par_id,
            't': hashfunc(md) if t is None else hashfunc(md),
            'links': [] if links is None else links,
            'attrs': {} if attrs is None else attrs
        } if src_dict is None else src_dict
        self.files_root = self.get_default_files_root() if files_root is None else files_root

    def __iter__(self):
        return self.__data.__iter__()

    @classmethod
    def get_default_files_root(cls):
        return 'tim_files'

    @classmethod
    @contract
    def fromDict(cls, d: 'dict'):
        return DocParagraph(src_dict=d)

    @classmethod
    @contract
    def getPath(cls, par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'str':
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', par_id, t)

    @classmethod
    @contract
    def getBasePath(cls, par_id: 'str', files_root: 'str|None' = None) -> 'str':
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', par_id)

    @classmethod
    @contract
    def getLatest(cls, par_id: 'str', files_root: 'str|None' = None):
        froot = cls.get_default_files_root() if files_root is None else files_root
        t = os.readlink(cls.getPath(par_id, 'current', froot))
        return cls.get(par_id, t, files_root=froot)

    @classmethod
    @contract
    def get(cls, par_id: 'str', t: 'str', files_root: 'str|None' = None):
        with open(cls.getPath(par_id, t, files_root), 'r') as f:
            return cls.fromDict(json.loads(f.read()))

    @contract
    def dict(self) -> 'dict':
        return self.__data

    @contract
    def getId(self) -> 'str':
        return self.__data['id']

    @contract
    def getHash(self) -> 'str':
        return self.__data['t']

    @contract
    def getMarkdown(self) -> 'str':
        return self.__data['md']

    @contract
    def getHtml(self) -> 'str':
        return self.__data['html']

    @contract
    def setHtml(self, new_html: 'str'):
        self.__data['html'] = new_html

    @contract
    def getLinks(self) -> 'list(int)':
        return self.__data['links']

    @contract
    def getAttrs(self) -> 'dict':
        return self.__data['attrs']

    @contract
    def get_base_path(self) -> 'str':
        return self.getBasePath(self.getId(), files_root=self.files_root)

    @contract
    def get_path(self) -> 'str':
        return self.getPath(self.getId(), self.getHash(), files_root=self.files_root)

    def __read(self):
        if not os.path.isfile(self.get_path()):
            return
        with open(self.get_path(), 'r') as f:
            self.__data = json.loads(f.read())

    def __write(self):
        file_name = self.get_path()
        should_exist = len(self.getLinks()) > 0
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
            self.__setLatest()

        with open(file_name, 'w') as f:
            f.write(json.dumps(self.__data))

    def __setLatest(self):
        linkpath = self.getPath(self.getId(), 'current', files_root=self.files_root)
        if linkpath == self.getHash():
            return
        if os.path.isfile(linkpath):
            os.unlink(linkpath)
        os.symlink(self.getHash(), linkpath)

    @contract
    def addLink(self, doc_id: 'int'):
        self.__read()
        self.__data['links'].append(doc_id)
        self.__write()

    @contract
    def removeLink(self, doc_id: 'int'):
        self.__read()
        self.__data['links'].remove(doc_id)
        self.__write()

    def updateLinks(self):
        self.__read()
        self.__write()

new_contract('DocParagraph', DocParagraph)
