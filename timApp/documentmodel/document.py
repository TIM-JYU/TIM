import os
import shutil

from datetime import datetime
from time import time
from tempfile import mkstemp

from contracts import contract, new_contract
from documentmodel.docparagraph import DocParagraph
from documentmodel.exceptions import DocExistsError

class Document:
    @contract()
    def __init__(self, doc_id: 'int|None', files_root = None):
        self.doc_id = doc_id if doc_id is not None else Document.get_next_free_id()
        self.files_root = self.get_default_files_root() if not files_root else files_root

    @classmethod
    def get_default_files_root(cls):
        return 'tim_files'


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

    def create(self, ignore_exists=False):
        path = os.path.join(self.files_root, 'docs', str(self.doc_id))
        if not os.path.exists(path):
            os.makedirs(path)
        elif not ignore_exists:
            raise DocExistsError(self.doc_id)

    @classmethod
    @contract
    def remove(cls, doc_id: 'int', files_root: 'str|None' = None, ignore_exists=False):
        """
        Removes the whole document.
        :param doc_id: Document id to remove.
        :return:
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        if cls.exists(doc_id, files_root=froot):
            shutil.rmtree(os.path.join(froot, 'docs', str(doc_id)))
            # todo: remove all paragraph links
        elif not ignore_exists:
            raise DocExistsError(doc_id)

    @classmethod
    @contract
    def get_next_free_id(cls, files_root: 'str|None' = None) -> 'int':
        """
        Gets the next free document id.
        :return:
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return 1 + cls.__get_largest_file_number(os.path.join(froot, 'docs'), default=0)

    @contract
    def get_version(self) -> 'tuple(int, int)':
        """
        Gets the latest version of the document as a major-minor tuple.
        :return: Latest version, or (-1, 0) if there isn't yet one.
        """
        basedir = os.path.join(self.files_root, 'docs', str(self.doc_id))
        major = self.__get_largest_file_number(basedir, default=0)
        minor = 0 if major < 1 else self.__get_largest_file_number(os.path.join(basedir, str(major)), default=0)
        return major, minor

    @contract
    def get_document_path(self) -> 'str':
        return os.path.join(self.files_root, 'docs', str(self.doc_id))

    @contract
    def get_version_path(self, ver: 'tuple(int, int)') -> 'str':
        return os.path.join(self.files_root, 'docs', str(self.doc_id), str(ver[0]), str(ver[1]))

    @contract
    def getlogfilename(self) -> 'str':
        return os.path.join(self.get_document_path(), 'changelog')

    @contract
    def __write_changelog(self, ver: 'tuple(int, int)', msg: 'str'):
        logname = self.getlogfilename()
        src = open(logname, 'r') if os.path.exists(logname) else None
        destfd, tmpname = mkstemp()
        dest = os.fdopen(destfd, 'w')

        ts = time()
        timestamp = datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')
        dest.write('{} {}.{} {}\n'.format(timestamp, ver[0], ver[1], msg))

        while src:
            line = src.readline()
            if line:
                dest.write(line)
            else:
                src.close()
                src = None

        dest.close()
        shutil.copyfile(tmpname, logname)
        os.unlink(tmpname)

    @contract
    def __increment_version(self, msg: 'str', increment_major: 'bool') -> 'tuple(int, int)':
        ver_exists = True
        while ver_exists:
            old_ver = self.get_version()
            ver = (old_ver[0] + 1, 0) if increment_major else (old_ver[0], old_ver[1] + 1)
            ver_exists = os.path.isfile(self.get_version_path(ver))
        if increment_major:
            os.mkdir(os.path.join(self.files_root, 'docs', str(self.doc_id), str(ver[0])))
        if old_ver[0] > 0:
            shutil.copyfile(self.get_version_path(old_ver), self.get_version_path(ver))
        else:
            with open(self.get_version_path(ver), 'w'):
                pass
        self.__write_changelog(ver, msg)
        return ver

    @contract
    def has_paragraph(self, par_id: 'str') -> 'bool':
        """
        Checks if the document has the given paragraph.
        :param par_id: The paragraph id.
        :return: Boolean.
        """
        par_line = par_id + '\n'
        with open(self.get_version_path(self.get_version()), 'r') as f:
            while True:
                line = f.readline()
                if line == '':
                    return False
                if line == par_line:
                    return True

    @contract
    def add_paragraph(self, text: 'str', attrs: 'dict|None'=None) -> 'DocParagraph':
        """
        Appends a new paragraph into the document.
        :param attrs: The attributes for the paragraph.
        :param text: New paragraph text.
        :return: The new paragraph object.
        """
        p = DocParagraph(md=text, files_root=self.files_root, attrs=attrs)
        p.get_html()
        p.add_link(self.doc_id)
        old_ver = self.get_version()
        new_ver = self.__increment_version('Added paragraph id {}'.format(p.get_id()), increment_major=True)
        old_path = self.get_version_path(old_ver)
        new_path = self.get_version_path(new_ver)
        if os.path.exists(old_path):
            shutil.copyfile(old_path, new_path)

        with open(new_path, 'a') as f:
            f.write(p.get_id())
            f.write('\n')
        return p

    @contract
    def delete_paragraph(self, par_id: 'str'):
        """
        Removes a paragraph from the document.
        :param par_id: Paragraph id to remove.
        """
        if not self.has_paragraph(par_id):
            return

        old_ver = self.get_version()
        new_ver = self.__increment_version('Deleted paragraph id {}'.format(par_id), increment_major=True)
        id_line = par_id + '\n'
        with open(self.get_version_path(old_ver), 'r') as f_src:
            with open(self.get_version_path(new_ver), 'w') as f:
                while True:
                    line = f_src.readline()
                    if not line:
                        return
                    if line == id_line:
                        p = DocParagraph.get_latest(par_id, files_root=self.files_root)
                        p.remove_link(self.doc_id)
                    else:
                        f.write(line)

    @contract
    def insert_paragraph(self, text: 'str', insert_before_id: 'str|None') -> 'DocParagraph':
        """
        Inserts a paragraph before a given paragraph id.
        :param text: New paragraph text.
        :param insert_before_id: Id of the paragraph to insert before, or None if last.
        :return: The inserted paragraph object.
        """
        if not insert_before_id:
            return self.add_paragraph(text)

        p = DocParagraph(text, files_root=self.files_root)
        p.add_link(self.doc_id)
        old_ver = self.get_version()
        new_ver = self.__increment_version('Inserted paragraph {}'.format(p.get_id()), increment_major=True)
        id_line = insert_before_id + '\n'
        with open(self.get_version_path(old_ver), 'r') as f_src:
            with open(self.get_version_path(new_ver), 'w') as f:
                while True:
                    line = f_src.readline()
                    if not line:
                        return p
                    if line == id_line:
                        f.write(p.get_id())
                        f.write('\n')
                        inserted = True
                    f.write(line)

    @contract
    def modify_paragraph(self, par_id: 'str', new_text: 'str') -> 'DocParagraph':
        """
        Modifies the text of the given paragraph.
        :param par_id: Paragraph id.
        :param new_text: New text.
        :return: The new paragraph object.
        """
        if not self.has_paragraph(par_id):
            raise KeyError('No paragraph {} in document {} version {}'.format(par_id, self.doc_id, self.get_version()))
        p_src = DocParagraph.get_latest(par_id, files_root=self.files_root)
        old_hash = p_src.get_hash()
        p = DocParagraph(new_text, par_id=par_id, links=p_src.get_links(), attrs=p_src.get_attrs(), files_root=self.files_root)
        new_hash = p.get_hash()
        p.update_links()
        self.__increment_version('Modified paragraph {} from hash {} to {}'.format(par_id, old_hash, new_hash), increment_major=False)
        return p

    @contract
    def get_index(self) -> 'list(str)':
        # todo: optimization?
        return [par.get_markdown() for par in self if par.get_markdown().startswith('#')]

    @contract
    def get_changelog(self, max_entries : 'int' = -1) -> 'list(str)':
        log = []
        logname = self.getlogfilename()
        if not os.path.isfile(logname):
            return []

        lc = max_entries
        with open(logname, 'r') as f:
            while lc != 0:
                line = f.readline()
                if not line:
                    break
                log.append(line)
                lc -= 1
        
        return log

new_contract('Document', Document)


class DocParagraphIter:
    def __init__(self, doc: 'Document'):
        self.doc = doc
        self.next_index = 0
        name = doc.get_version_path(doc.get_version())
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
                return DocParagraph.get_latest(line.replace('\n', ''), self.doc.files_root)

    def __close(self):
        if self.f:
            self.f.close()
            self.f = None
