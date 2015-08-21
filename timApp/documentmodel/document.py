from difflib import SequenceMatcher
import json
import os
import shutil

from datetime import datetime
from time import time
from tempfile import mkstemp
from lxml import etree
from io import StringIO, BytesIO

from contracts import contract, new_contract
from documentmodel.docparagraph import DocParagraph
from documentmodel.documentparser import DocumentParser
from documentmodel.documentwriter import DocumentWriter
from documentmodel.exceptions import DocExistsError
from timdb.timdbbase import TimDbException


class Document:
    @contract()
    def __init__(self, doc_id: 'int|None'=None, files_root = None, modifier_group_id: 'int|None' = 0):
        self.doc_id = doc_id if doc_id is not None else Document.get_next_free_id(files_root)
        self.files_root = self.get_default_files_root() if not files_root else files_root
        self.modifier_group_id = modifier_group_id

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
    def doc_exists(cls, doc_id: 'int', files_root: 'str|None' = None) -> 'bool':
        """
        Checks if a document id exists.
        :param doc_id: Document id.
        :return: Boolean.
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.exists(os.path.join(froot, 'docs', str(doc_id)))

    @contract
    def create(self, ignore_exists : 'bool' = False):
        path = os.path.join(self.files_root, 'docs', str(self.doc_id))
        if not os.path.exists(path):
            os.makedirs(path)
        elif not ignore_exists:
            raise DocExistsError(self.doc_id)

    @contract
    def exists(self) -> 'bool':
        return Document.doc_exists(self.doc_id, self.files_root)

    @contract
    def export_markdown(self, export_hashes : 'bool' = False) -> 'str':
        return DocumentWriter([par.dict() for par in self], export_hashes=export_hashes).get_text()

    @contract
    def export_section(self, par_id_start: 'str', par_id_end: 'str', export_hashes=False) -> 'str':
        return DocumentWriter([par.dict() for par in self.get_section(par_id_start, par_id_end)],
                              export_hashes=export_hashes).get_text()

    @contract
    def get_section(self, par_id_start: 'str', par_id_end: 'str') -> 'list(DocParagraph)':
        all_pars = [par for par in self]
        all_par_ids = [par.get_id() for par in all_pars]
        start_index, end_index = all_par_ids.index(par_id_start), all_par_ids.index(par_id_end)
        return all_pars[start_index:end_index + 1]

    @classmethod
    @contract
    def remove(cls, doc_id: 'int', files_root: 'str|None' = None, ignore_exists=False):
        """
        Removes the whole document.
        :param doc_id: Document id to remove.
        :return:
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        if cls.doc_exists(doc_id, files_root=froot):
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
        res = 1 + cls.__get_largest_file_number(os.path.join(froot, 'docs'), default=0)
        print("get_next_free_id() = {}".format(res))
        return res

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
    def __write_changelog(self, ver: 'tuple(int, int)', operation: 'str', par_id: 'str', op_params: 'dict|None' = None):
        logname = self.getlogfilename()
        src = open(logname, 'r') if os.path.exists(logname) else None
        destfd, tmpname = mkstemp()
        dest = os.fdopen(destfd, 'w')

        ts = time()
        timestamp = datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')
        entry = {
            'group_id': self.modifier_group_id,
            'par_id': par_id,
            'op': operation,
            'op_params': op_params,
            'ver': ver,
            'time': timestamp
        }
        dest.write(json.dumps(entry))
        dest.write('\n')
        #dest.write('{} {}.{} {}\n'.format(timestamp, ver[0], ver[1], msg))

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
    def __increment_version(self, op: 'str', par_id: 'str', increment_major: 'bool', op_params: 'dict|None' = None) -> 'tuple(int, int)':
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
        self.__write_changelog(ver, op, par_id, op_params)
        return ver

    @contract
    def get_name(self) -> 'str':
        """
        Gets the document name from its main heading.
        :return: Document name or "Document n" if not found.
        """
        for par in self:
            md = par.get_markdown().lstrip()
            if md.startswith('#'):
                return md.lstrip('# ')
        return "Document {}".format(self.doc_id)

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
    def get_paragraph(self, par_id: 'str') -> 'DocParagraph':
        return DocParagraph.get_latest(self.doc_id, par_id, self.files_root)

    @contract
    def add_paragraph(self, text: 'str', attrs: 'dict|None'=None, par_id: 'str|None'=None) -> 'DocParagraph':
        """
        Appends a new paragraph into the document.
        :param par_id: The id of the paragraph or None if it should be autogenerated.
        :param attrs: The attributes for the paragraph.
        :param text: New paragraph text.
        :return: The new paragraph object.
        """
        p = DocParagraph(md=text, files_root=self.files_root, attrs=attrs, par_id=par_id, doc_id=self.doc_id)
        p.get_html()
        p.add_link(self.doc_id)
        old_ver = self.get_version()
        new_ver = self.__increment_version('Added', p.get_id(), increment_major=True)
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
        new_ver = self.__increment_version('Deleted', par_id, increment_major=True)
        id_line = par_id + '\n'
        with open(self.get_version_path(old_ver), 'r') as f_src:
            with open(self.get_version_path(new_ver), 'w') as f:
                while True:
                    line = f_src.readline()
                    if not line:
                        return
                    if line == id_line:
                        p = DocParagraph.get_latest(self.doc_id, par_id, files_root=self.files_root)
                        p.remove_link(self.doc_id)
                    else:
                        f.write(line)

    @contract
    def insert_paragraph(self, text: 'str',
                         insert_before_id:'str|None',
                         attrs: 'dict|None'=None,
                         par_id: 'str|None'=None) -> 'DocParagraph':
        """
        Inserts a paragraph before a given paragraph id.
        :param par_id: The id of the new paragraph or None if it should be autogenerated.
        :param attrs: The attributes for the paragraph.
        :param text: New paragraph text.
        :param insert_before_id: Id of the paragraph to insert before, or None if last.
        :return: The inserted paragraph object.
        """
        if not insert_before_id:
            return self.add_paragraph(text, attrs, par_id=par_id)

        p = DocParagraph(text, files_root=self.files_root, attrs=attrs, par_id=par_id, doc_id=self.doc_id)
        p.add_link(self.doc_id)
        old_ver = self.get_version()
        new_ver = self.__increment_version('Inserted', p.get_id(), increment_major=True, op_params={'before_id': insert_before_id})
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
    def modify_paragraph(self, par_id: 'str', new_text: 'str', new_attrs: 'dict|None'=None) -> 'DocParagraph':
        """
        Modifies the text of the given paragraph.
        :param par_id: Paragraph id.
        :param new_text: New text.
        :return: The new paragraph object.
        """
        if not self.has_paragraph(par_id):
            raise KeyError('No paragraph {} in document {} version {}'.format(par_id, self.doc_id, self.get_version()))
        p_src = DocParagraph.get_latest(self.doc_id, par_id, files_root=self.files_root)
        p_src.remove_link(self.doc_id)
        old_hash = p_src.get_hash()
        p = DocParagraph(new_text, doc_id=self.doc_id, par_id=par_id, attrs=new_attrs, files_root=self.files_root)
        new_hash = p.get_hash()
        p.add_link(self.doc_id)
        self.__increment_version('Modified', par_id, increment_major=False, op_params={'old_hash': old_hash, 'new_hash': new_hash})
        return p

    @contract
    def update_section(self, text: 'str', par_id_first: 'str', par_id_last: 'str') -> 'tuple(str,str)':
        """Updates a section of the document.

        :param text: The text of the section.
        :param par_id_first: The id of the paragraph that denotes the start of the section.
        :param par_id_last: The id of the paragraph that denotes the end of the section.
        """
        new_pars = DocumentParser(text).add_missing_attributes().validate_structure().get_blocks()
        new_par_id_set = set([par['id'] for par in new_pars])
        all_pars = [par for par in self]
        all_par_ids = [par.get_id() for par in all_pars]
        start_index, end_index = all_par_ids.index(par_id_first), all_par_ids.index(par_id_last)
        old_pars = all_pars[start_index:end_index + 1]
        other_par_ids = all_par_ids[:]
        del other_par_ids[start_index:end_index + 1]
        intersection = new_par_id_set & set(other_par_ids)
        if intersection:
            raise TimDbException('Duplicate id(s): ' + str(intersection))
        return self._perform_update(new_pars,
                                    old_pars,
                                    last_par_id=all_par_ids[end_index + 1]
                                    if end_index + 1 < len(all_par_ids) else None)

    @contract
    def update(self, text: 'str'):
        """Replaces the document's contents with the specified text.

        :param text: The new text for the document.
        """
        new_pars = DocumentParser(text).add_missing_attributes().validate_structure().get_blocks()
        old_pars = [par for par in self]

        self._perform_update(new_pars, old_pars)

    @contract
    def _perform_update(self, new_pars: 'list(dict)',
                        old_pars: 'list(DocParagraph)',
                        last_par_id=None) -> 'tuple(str,str)':
        old_ids = [par.get_id() for par in old_pars]
        new_ids = [par['id'] for par in new_pars]
        s = SequenceMatcher(None, old_ids, new_ids)
        opcodes = s.get_opcodes()
        # Do delete operations first to avoid duplicate ids
        for tag, i1, i2, j1, j2 in [opcode for opcode in opcodes if opcode[0] in ['delete', 'replace']]:
            for par_id in old_ids[i1:i2]:
                self.delete_paragraph(par_id)
        for tag, i1, i2, j1, j2 in opcodes:
            if tag == 'replace':
                for par in new_pars[j1:j2]:
                    before_i = i2
                    while before_i < len(old_ids) and not self.has_paragraph(old_ids[before_i]):
                        before_i += 1
                    self.insert_paragraph(par['md'],
                                          attrs=par.get('attrs'),
                                          par_id=par['id'],
                                          insert_before_id=old_ids[before_i] if before_i < len(old_ids) else last_par_id)
            elif tag == 'insert':
                for par in new_pars[j1:j2]:
                    before_i = i2
                    while before_i < len(old_ids) and not self.has_paragraph(old_ids[before_i]):
                        before_i += 1
                    self.insert_paragraph(par['md'],
                                          attrs=par.get('attrs'),
                                          par_id=par['id'],
                                          insert_before_id=old_ids[before_i] if i2 < len(old_ids) else last_par_id)
            elif tag == 'equal':
                for new_par, old_par in zip(new_pars[j1:j2], old_pars[i1:i2]):
                    if new_par['t'] != old_par.get_hash() or new_par.get('attrs', {}) != old_par.get_attrs():
                        self.modify_paragraph(old_par.get_id(),
                                              new_par['md'],
                                              new_attrs=new_par.get('attrs'))
        return new_ids[0], new_ids[-1]

    @contract
    def get_index(self) -> 'list(tuple)':
        # todo: optimization?
        html_table = [par.get_html() for par in self if (par.get_markdown().startswith('#') or
                                                         par.get_exported_markdown().startswith('```'))]
        index = []
        for html in html_table:
            index_entry = etree.fromstring(html)
            if index_entry.tag == 'div':
                for header in index_entry.iter('h1', 'h2', 'h3'):
                    level = int(header.tag[1:])
                    index.append((header.get('id'), header.text, level))
            elif index_entry.tag.startswith('h'):
                level = int(index_entry.tag[1:])
                index.append((index_entry.get('id'), index_entry.text, level))
        return index

    @contract
    def get_changelog(self, max_entries : 'int' = 100) -> 'list(dict)':
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
                try:
                    log.append(json.loads(line))
                except ValueError:
                    print("doc id {}: malformed log line: {}".format(self.doc_id, line))
                lc -= 1
        
        return log

    def get_paragraph_by_task(self, task_id_name):
        for p in self:
            if 'taskId' in p.get_attrs() and p.get_attrs()['taskId'] == task_id_name:
                return p
        return None

    def get_last_modified(self):
        log = self.get_changelog(max_entries=1)
        return log[0]['time'] if log is not None and len(log) > 0 else ''

    def delete_section(self, area_start, area_end):
        all_par_ids = [par.get_id() for par in self]
        start_index, end_index = all_par_ids.index(area_start), all_par_ids.index(area_end)
        old_pars = all_par_ids[start_index:end_index + 1]
        for par in old_pars:
            self.delete_paragraph(par)

    def get_named_section(self, section_name):
        start_found = False
        end_found = False
        pars = []
        for par in self:
            if par.get_attrs().get('area') == section_name:
                start_found = True
            if start_found:
                pars.append(par)
            if par.get_attrs().get('area_end') == section_name:
                end_found = True
                break
        if not start_found or not end_found:
            raise TimDbException('Area not found: ' + section_name)
        return pars


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
                return DocParagraph.get_latest(self.doc.doc_id, line.replace('\n', ''), self.doc.files_root)

    def __close(self):
        if self.f:
            self.f.close()
            self.f = None
