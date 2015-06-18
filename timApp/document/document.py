import os
import shutil

from contracts import contract, new_contract
from .docparagraph import DocParagraph


class Document:
    @contract()
    def __init__(self, doc_id: 'int|None', files_root = None):
        self.doc_id = doc_id if doc_id is not None else Document.getNextFreeId()
        self.files_root = self.get_files_root() if files_root is None else files_root
        self.__check_paths()

    @classmethod
    def get_default_files_root(cls):
        return 'tim_files'

    def __check_paths(self):
        path = os.path.join(self.files_root, 'docs', str(self.doc_id))
        if not os.path.exists(path):
            os.makedirs(path)

    def __len__(self):
        count = 0
        for _ in self:
            count += 1
        return count

    def __iter__(self):
        return DocParagraphIter(self)

    @classmethod
    @contract
    def __get_largest_file_number(cls, path: 'str', default=None) -> 'int':
        if not os.path.exists(path):
            return default

        largest = -1
        for name in os.listdir(path):
            try:
                largest = max(largest, int(name))
            except ValueError:
                pass
        return largest if largest > -1 else default

    @classmethod
    @contract
    def exists(cls, doc_id: 'int', files_root: 'str|None' = None) -> 'bool':
        """
        Checks if a document id exists.
        :param doc_id: Document id.
        :return: Boolean.
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.exists(os.path.join(froot, 'docs', str(doc_id)))

    @classmethod
    @contract
    def remove(cls, doc_id: 'int', files_root: 'str|None' = None):
        """
        Removes the whole document.
        :param doc_id: Document id to remove.
        :return:
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        shutil.rmtree(os.path.join(froot, 'docs', str(doc_id)))
        # todo: remove all paragraph links

    @classmethod
    @contract
    def getNextFreeId(cls, files_root: 'str|None' = None) -> 'int':
        """
        Gets the next free document id.
        :return:
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return cls.__get_largest_file_number(os.path.join(froot, 'docs'), default=-1) + 1

    @contract
    def getVersion(self) -> 'tuple(int, int)':
        """
        Gets the latest version of the document as a major-minor tuple.
        :return: Latest version, or (-1, 0) if there isn't yet one.
        """
        basedir = os.path.join(self.files_root, 'docs', str(self.doc_id))
        major = self.__get_largest_file_number(basedir, default=0)
        minor = 0 if major < 1 else self.__get_largest_file_number(os.path.join(basedir, str(major)), default=0)
        return major, minor

    @contract
    def getVersionPath(self, ver: 'tuple(int, int)') -> 'str':
        return os.path.join(self.files_root, 'docs', str(self.doc_id), str(ver[0]), str(ver[1]))

    @contract
    def __incrementVersion(self, increment_major: 'bool') -> 'tuple(int, int)':
        ver_exists = True
        while ver_exists:
            old_ver = self.getVersion()
            ver = (old_ver[0] + 1, 0) if increment_major else (old_ver[0], old_ver[1] + 1)
            ver_exists = os.path.isfile(self.getVersionPath(ver))
        if increment_major:
            os.mkdir(os.path.join(self.files_root, 'docs', str(self.doc_id), str(ver[0])))
        if old_ver[0] > 0:
            shutil.copyfile(self.getVersionPath(old_ver), self.getVersionPath(ver))
        else:
            with open(self.getVersionPath(ver), 'w'):
                pass
        return ver

    @contract
    def hasParagraph(self, par_id: 'str') -> 'bool':
        """
        Checks if the document has the given paragraph.
        :param par_id: The paragraph id.
        :return: Boolean.
        """
        par_line = par_id + '\n'
        with open(self.getVersionPath(self.getVersion()), 'r') as f:
            if f.readline() == par_line:
                return True
        return False

    @contract
    def addParagraph(self, text: 'str') -> 'DocParagraph':
        """
        Appends a new paragraph into the document.
        :param text: New paragraph text.
        :return: The new paragraph object.
        """
        p = DocParagraph(text, files_root=self.files_root)
        p.addLink(self.doc_id)
        old_ver = self.getVersion()
        new_ver = self.__incrementVersion(increment_major=True)
        old_path = self.getVersionPath(old_ver)
        new_path = self.getVersionPath(new_ver)
        if os.path.exists(old_path):
            shutil.copyfile(old_path, new_path)

        with open(new_path, 'a') as f:
            f.write(p.getId())
            f.write('\n')
        return p

    @contract
    def deleteParagraph(self, par_id: 'str', prev_par_id: 'str|None'):
        """
        Removes a paragraph from the document.
        :param par_id: Paragraph id to remove.
        :param prev_par_id: Previous paragraph id. Used in case there are multiple
        instances of the same paragraph id. None if it's the first paragraph.
        """
        old_ver = self.getVersion()
        new_ver = self.__incrementVersion(increment_major=True)
        prev_line = None
        id_line = par_id + '\n'
        prev_id_line = None if prev_par_id is None else prev_par_id + '\n'
        with open(self.getVersionPath(old_ver), 'r') as f_src:
            with open(self.getVersionPath(new_ver), 'w') as f:
                while True:
                    line = f_src.readline()
                    if not line:
                        return
                    if line == id_line and prev_line == prev_id_line:
                        p = DocParagraph.getLatest(par_id, files_root=self.files_root)
                        p.removeLink(self.doc_id)
                    else:
                        f.write(line)
                        prev_line = line
        # todo: don't make a new version if the paragraph was not found

    @contract
    def insertParagraph(self, text: 'str', insert_before_id: 'str') -> 'DocParagraph':
        """
        Inserts a paragraph before a given paragraph id.
        :param text: New paragraph text.
        :param insert_before_id: Id of the paragraph to insert before.
        :return: The inserted paragraph object.
        """
        p = DocParagraph(text, files_root=self.files_root)
        p.addLink(self.doc_id)
        old_ver = self.getVersion()
        new_ver = self.__incrementVersion(increment_major=True)
        id_line = insert_before_id + '\n'
        with open(self.getVersionPath(old_ver), 'r') as f_src:
            with open(self.getVersionPath(new_ver), 'w') as f:
                while True:
                    line = f_src.readline()
                    if not line:
                        return p
                    if line == id_line:
                        f.write(p.getId())
                        f.write('\n')
                    f.write(line)

    @contract
    def modifyParagraph(self, par_id: 'str', new_text: 'str') -> 'DocParagraph':
        """
        Modifies the text of the given paragraph.
        :param par_id: Paragraph id.
        :param new_text: New text.
        :return: The new paragraph object.
        """
        if not self.hasParagraph(par_id):
            raise KeyError('No paragraph {} in document {} version {}'.format(par_id, self.doc_id, self.getVersion()))
        p_src = DocParagraph.getLatest(par_id, files_root=self.files_root)
        p = DocParagraph(new_text, par_id=par_id, links=p_src.getLinks(), attrs=p_src.getAttrs(), files_root=self.files_root)
        p.updateLinks()
        # todo: file to record paragraph hashes
        self.__incrementVersion(increment_major=False)
        return p


new_contract('Document', Document)


class DocParagraphIter:
    def __init__(self, doc: 'Document'):
        self.doc = doc
        self.next_index = 0
        name = doc.getVersionPath(doc.getVersion())
        self.f = open(name, 'r') if os.path.isfile(name) else None

    def __next__(self) -> 'DocParagraph':
        if not self.f:
            raise StopIteration
        while True:
            line = self.f.readline()
            if not line:
                self.__close()
                raise StopIteration
            if line != '\n':
                return DocParagraph.getLatest(line.replace('\n', ''), self.doc.files_root)

    def __close(self):
        if self.f:
            self.f.close()
            self.f = None
