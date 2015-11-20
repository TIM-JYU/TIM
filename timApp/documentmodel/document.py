from difflib import SequenceMatcher
import functools
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
from documentmodel.docsettings import DocSettings
from documentmodel.documentparser import DocumentParser
from documentmodel.documentwriter import DocumentWriter
from documentmodel.exceptions import DocExistsError
from timdb.timdbbase import TimDbException


class Document:
    default_files_root = 'tim_files'

    @contract()
    def __init__(self, doc_id: 'int|None'=None, files_root = None, modifier_group_id: 'int|None' = 0):
        self.doc_id = doc_id if doc_id is not None else Document.get_next_free_id(files_root)
        self.files_root = self.get_default_files_root() if not files_root else files_root
        self.modifier_group_id = modifier_group_id
        self.version = None

    @classmethod
    def get_default_files_root(cls):
        return cls.default_files_root

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

    @classmethod
    @contract
    def version_exists(cls, doc_id: 'int', doc_ver: 'tuple(int,int)', files_root: 'str|None' = None) -> 'bool':
        """
        Checks if a document version exists.
        :param doc_id: Document id.
        :param doc_ver: Document version.
        :return: Boolean.
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.isfile(os.path.join(froot, 'docs', str(doc_id), str(doc_ver[0]), str(doc_ver[1])))

    @contract
    def get_settings(self) -> 'DocSettings':
        try:
            i = self.__iter__()
            return DocSettings.from_paragraph(next(i))
        except StopIteration:
            return DocSettings()
        finally:
            i.close()

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
        return res

    @contract
    def get_version(self) -> 'tuple(int, int)':
        """
        Gets the latest version of the document as a major-minor tuple.
        :return: Latest version, or (-1, 0) if there isn't yet one.
        """
        if self.version is not None:
            return self.version
        basedir = os.path.join(self.files_root, 'docs', str(self.doc_id))
        major = self.__get_largest_file_number(basedir, default=0)
        minor = 0 if major < 1 else self.__get_largest_file_number(os.path.join(basedir, str(major)), default=0)
        self.version = major, minor
        return major, minor

    @contract
    def get_id_version(self) -> 'tuple(int, int, int)':
        major, minor = self.get_version()
        return self.doc_id, major, minor

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
    def __increment_version(self, op: 'str', par_id: 'str', increment_major: 'bool',
                            op_params: 'dict|None' = None) -> 'tuple(int, int)':
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
        self.version = ver
        return ver

    @contract
    def has_paragraph(self, par_id: 'str') -> 'bool':
        """
        Checks if the document has the given paragraph.
        :param par_id: The paragraph id.
        :return: Boolean.
        """
        with open(self.get_version_path(self.get_version()), 'r') as f:
            while True:
                line = f.readline()
                if line == '':
                    return False
                if line.split('/', 1)[0].rstrip('\n') == par_id:
                    return True

    @contract
    def get_paragraph(self, par_id: 'str') -> 'DocParagraph':
        return DocParagraph.get_latest(self, par_id, self.files_root)

    @contract
    def add_paragraph_obj(self, p: 'DocParagraph') -> 'DocParagraph':
        """
        Appends a new paragraph into the document.
        :param p: Paragraph to be added.
        :return: The same paragraph object, or None if could not add.
        """
        p.get_html()
        p.add_link(self.doc_id)
        p.set_latest()
        old_ver = self.get_version()
        new_ver = self.__increment_version('Added', p.get_id(), increment_major=True)
        old_path = self.get_version_path(old_ver)
        new_path = self.get_version_path(new_ver)
        if os.path.exists(old_path):
            shutil.copyfile(old_path, new_path)

        with open(new_path, 'a') as f:
            f.write(p.get_id() + '/' + p.get_hash())
            f.write('\n')
        return p

    @contract
    def add_paragraph(
            self,
            text: 'str',
            par_id: 'str|None'=None,
            attrs: 'dict|None'=None,
            properties: 'dict|None'=None
            ) -> 'DocParagraph':
        """
        Appends a new paragraph into the document.
        :param par_id: The id of the paragraph or None if it should be autogenerated.
        :param attrs: The attributes for the paragraph.
        :param text: New paragraph text.
        :return: The new paragraph object.
        """
        p = DocParagraph.create(
            doc=self,
            par_id=par_id,
            md=text,
            attrs=attrs,
            props=properties,
            files_root=self.files_root
        )
        return self.add_paragraph_obj(p)

    @contract
    def add_ref_paragraph(self, src_par: 'DocParagraph', text: 'str|None' = None) -> 'DocParagraph':
        ref_attrs = {
            'rp': src_par.get_id(),
            'rt': src_par.get_hash()
        }
        rd = src_par.get_doc_id()
        if self.get_settings().get_source_document() != rd:
            ref_attrs['rd'] = rd,
        if text:
            ref_attrs['r'] = 'tr'

        return self.add_paragraph(text, attrs=ref_attrs)

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
        with open(self.get_version_path(old_ver), 'r') as f_src:
            with open(self.get_version_path(new_ver), 'w') as f:
                while True:
                    line = f_src.readline()
                    if not line:
                        return
                    if line.startswith(par_id):
                        p = DocParagraph.get_latest(self, par_id, files_root=self.files_root)
                        p.remove_link(self.doc_id)
                    else:
                        f.write(line)

    @contract
    def insert_paragraph(self, text: 'str',
                         insert_before_id: 'str|None',
                         attrs: 'dict|None'=None, properties: 'dict|None'=None,
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
            return self.add_paragraph(text=text, par_id=par_id, attrs=attrs, properties=properties)

        p = DocParagraph.create(
            doc=self,
            par_id=par_id,
            md=text,
            attrs=attrs,
            props=properties,
            files_root=self.files_root
        )

        p.add_link(self.doc_id)
        p.set_latest()
        old_ver = self.get_version()
        new_ver = self.__increment_version('Inserted', p.get_id(), increment_major=True,
                                           op_params={'before_id': insert_before_id})
        with open(self.get_version_path(old_ver), 'r') as f_src:
            with open(self.get_version_path(new_ver), 'w') as f:
                while True:
                    line = f_src.readline()
                    if not line:
                        return p
                    if line.startswith(insert_before_id):
                        f.write(p.get_id() + '/' + p.get_hash())
                        f.write('\n')
                    f.write(line)

    @contract
    def modify_paragraph(self, par_id: 'str', new_text: 'str', new_attrs: 'dict|None'=None, new_properties: 'dict|None'=None) -> 'DocParagraph':
        """
        Modifies the text of the given paragraph.
        :param par_id: Paragraph id.
        :param new_text: New text.
        :return: The new paragraph object.
        """
        if not self.has_paragraph(par_id):
            raise KeyError('No paragraph {} in document {} version {}'.format(par_id, self.doc_id, self.get_version()))
        p_src = DocParagraph.get_latest(self, par_id, files_root=self.files_root)
        p_src.remove_link(self.doc_id)
        old_hash = p_src.get_hash()
        p = DocParagraph.create(
            md=new_text,
            doc=self,
            par_id=par_id,
            attrs=new_attrs,
            props=new_properties,
            files_root=self.files_root
        )
        new_hash = p.get_hash()
        p.add_link(self.doc_id)
        p.set_latest()
        old_ver = self.get_version()
        new_ver = self.__increment_version('Modified', par_id, increment_major=False,
                                           op_params={'old_hash': old_hash, 'new_hash': new_hash})
        old_line = '{}/{}\n'.format(par_id, old_hash)
        old_line_legacy = '{}\n'.format(par_id)
        new_line = '{}/{}\n'.format(par_id, new_hash)
        with open(self.get_version_path(old_ver), 'r') as f_src:
            with open(self.get_version_path(new_ver), 'w') as f:
                while True:
                    line = f_src.readline()
                    if not line:
                        return p
                    if line == old_line or line == old_line_legacy:
                        f.write(new_line)
                    else:
                        f.write(line)
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
    def update(self, text: 'str', original: 'str'):
        """Replaces the document's contents with the specified text.

        :param text: The new text for the document.
        """
        new_pars = DocumentParser(text).add_missing_attributes().validate_structure().get_blocks()
        old_pars = [DocParagraph.from_dict(doc=self, d=d)
                    for d in DocumentParser(original).add_missing_attributes().validate_structure().get_blocks()]

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
                    before_i = self.find_insert_index(i2, old_ids)
                    self.insert_paragraph(par['md'],
                                          attrs=par.get('attrs'),
                                          par_id=par['id'],
                                          insert_before_id=old_ids[before_i] if before_i < len(old_ids) else last_par_id)
            elif tag == 'insert':
                for par in new_pars[j1:j2]:
                    before_i = self.find_insert_index(i2, old_ids)
                    self.insert_paragraph(par['md'],
                                          attrs=par.get('attrs'),
                                          par_id=par['id'],
                                          insert_before_id=old_ids[before_i] if before_i < len(old_ids) else last_par_id)
            elif tag == 'equal':
                for idx, (new_par, old_par) in enumerate(zip(new_pars[j1:j2], old_pars[i1:i2])):
                    if new_par['t'] != old_par.get_hash() or new_par.get('attrs', {}) != old_par.get_attrs():
                        if self.has_paragraph(old_par.get_id()):
                            self.modify_paragraph(old_par.get_id(),
                                                  new_par['md'],
                                                  new_attrs=new_par.get('attrs'))
                        else:
                            before_i = self.find_insert_index(j1 + idx, new_ids)
                            self.insert_paragraph(new_par['md'],
                                                  attrs=new_par.get('attrs'),
                                                  par_id=new_par['id'],
                                                  insert_before_id=old_ids[before_i] if before_i < len(
                                                      old_ids) else last_par_id)
        return new_ids[0], new_ids[-1]

    def find_insert_index(self, i2, old_ids):
        before_i = i2
        while before_i < len(old_ids) and not self.has_paragraph(old_ids[before_i]):
            before_i += 1
        return before_i

    def get_index(self) -> 'list(tuple)':
        return get_index_for_version(self.doc_id, self.get_version())

    @staticmethod
    def add_index_entry(index_table, current_headers, header):
        level = int(header.tag[1:])
        current = {'id': header.get('id'), 'text': header.text, 'level': level}
        if level == 1:
            if current_headers is not None:
                index_table.append(current_headers)
            current_headers = (current, [])
        elif current_headers is not None:
            current_headers[1].append(current)
        return current_headers

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
            if p.get_attr('taskId') == task_id_name:
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
            if par.get_attr('area') == section_name:
                start_found = True
            if start_found:
                pars.append(par)
            if par.get_attr('area_end') == section_name:
                end_found = True
                break
        if not start_found or not end_found:
            raise TimDbException('Area not found: ' + section_name)
        return pars

    @contract
    def get_referenced_document_ids(self) -> 'set':
        refs = set()
        for par in self:
            if par.is_reference():
                try:
                    refs.add(int(par.get_rd()))
                except (ValueError, TypeError):
                    print('Invalid document reference: ' + str(par.get_rd()))


        return refs

    @contract
    def get_paragraphs(self) -> 'list(DocParagraph)':
        return [par for par in self]

    def get_latest_version(self):
        from documentmodel.documentversion import DocumentVersion
        return DocumentVersion(self.doc_id, self.get_version(), self.files_root, self.modifier_group_id)

    def get_original_document(self):
        src_docid = self.get_settings().get_source_document()
        return Document(src_docid) if src_docid is not None else None

new_contract('Document', Document)


class DocParagraphIter:
    def __init__(self, doc: 'Document¦None' = None, version:'tuple(int,int)|None'=None):
        self.doc = doc
        self.next_index = 0
        if doc is not None:
            ver = doc.get_version() if version is None else version
            name = doc.get_version_path(ver)
            self.f = open(name, 'r') if os.path.isfile(name) else None

    def __iter__(self):
        return self

    def __next__(self) -> 'DocParagraph':
        if not self.f:
            raise StopIteration
        while True:
            line = self.f.readline()
            if not line:
                self.close()
                raise StopIteration
            if line != '\n':
                if len(line) > 14:
                    # Line contains both par_id and t
                    par_id, t = line.replace('\n', '').split('/')
                    # Make a copy of the paragraph to avoid modifying cached instance
                    return DocParagraph.get(self.doc, par_id, t, self.doc.files_root)
                else:
                    # Line contains just par_id, use the latest t
                    return DocParagraph.get_latest(self.doc, line.replace('\n', ''), self.doc.files_root)

    def close(self):
        if self.f:
            self.f.close()
            self.f = None


@contract
def get_index_for_version(doc_id: 'int', version: 'tuple(int,int)') -> 'list(tuple)':
    doc = Document(doc_id)
    pars = []
    for par in DocParagraphIter(doc, version):
        md = par.get_markdown()
        if (len(md) > 2 and md[0] == '#' and md[1] != '.')\
            or (par.is_multi_block() and par.has_headers()):
                pars.append(par)

    DocParagraph.preload_htmls(pars, doc.get_settings())
    html_table = [par.get_html() for par in pars]
    index = []
    current_headers = None
    for html in html_table:
        try:
            index_entry = etree.fromstring(html)
        except etree.XMLSyntaxError:
            continue
        if index_entry.tag == 'div':
            for header in index_entry.iter('h1', 'h2', 'h3'):
                current_headers = doc.add_index_entry(index, current_headers, header)
        elif index_entry.tag.startswith('h'):
            current_headers = doc.add_index_entry(index, current_headers, index_entry)
    if current_headers is not None:
        index.append(current_headers)
    return index

