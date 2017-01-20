import json
import os
import shutil
from datetime import datetime, timezone
from difflib import SequenceMatcher
from tempfile import mkstemp
from time import time

import dateutil.parser
from lxml import etree, html
from typing import List, Optional, Set, Tuple, Union, Iterable

from documentmodel.docparagraph import DocParagraph
from documentmodel.docsettings import DocSettings
from documentmodel.documentparser import DocumentParser, AttributesAtEndOfCodeBlockException, ValidationException
from documentmodel.documentparseroptions import DocumentParserOptions
from documentmodel.documentwriter import DocumentWriter
from documentmodel.exceptions import DocExistsError
from timdb.timdbexception import TimDbException
from utils import get_error_html


class Document:
    default_files_root = 'tim_files'

    def __init__(self, doc_id: Optional[int]=None, files_root = None, modifier_group_id: Optional[int] = 0):
        self.doc_id = doc_id if doc_id is not None else Document.get_next_free_id(files_root)
        self.files_root = self.get_default_files_root() if not files_root else files_root
        self.modifier_group_id = modifier_group_id
        self.version = None

        # Used to cache paragraphs in memory on request so the pars don't have to be read from disk in every for loop
        self.par_cache = None

        # Used for accessing previous/next paragraphs quickly based on id
        self.par_map = None

    @classmethod
    def get_default_files_root(cls):
        return cls.default_files_root

    @classmethod
    def get_documents_dir(cls, files_root: str = default_files_root) -> str:
        return os.path.join(files_root, 'docs')

    def __len__(self):
        count = 0
        for _ in self:
            count += 1
        return count

    def __iter__(self) -> 'Union[DocParagraphIter, CacheIterator]':
        if self.par_cache is None:
            return DocParagraphIter(self)
        else:
            return CacheIterator(self.par_cache.__iter__())

    @classmethod
    def __get_largest_file_number(cls, path: str, default=None) -> int:
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
    def doc_exists(cls, doc_id: int, files_root: Optional[str] = None) -> bool:
        """
        Checks if a document id exists.
        :param doc_id: Document id.
        :return: Boolean.
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.exists(os.path.join(cls.get_documents_dir(froot), str(doc_id)))

    @classmethod
    def version_exists(cls, doc_id: int, doc_ver: 'tuple(int,int)', files_root: Optional[str] = None) -> bool:
        """
        Checks if a document version exists.
        :param doc_id: Document id.
        :param doc_ver: Document version.
        :return: Boolean.
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.isfile(os.path.join(cls.get_documents_dir(froot), str(doc_id), str(doc_ver[0]), str(doc_ver[1])))

    def __update_par_map(self):
        self.par_map = {}
        for i in range(0, len(self.par_cache)):
            curr_p = self.par_cache[i].get_id()
            prev_p = self.par_cache[i-1] if i > 0 else None
            next_p = self.par_cache[i+1] if i+1 < len(self.par_cache) else None
            self.par_map[curr_p] = {'p': prev_p, 'n': next_p}

    def load_pars(self):
        """
        Loads the paragraphs from disk to memory so that subsequent iterations for the Document are faster.
        """
        self.par_cache = [par for par in self]
        self.__update_par_map()

    def get_previous_par(self, par, get_last_if_no_prev=False):
        if self.par_map is None:
            self.load_pars()
        prev = self.par_map.get(par.get_id())
        result = None
        if prev:
            result = prev['p']
        if get_last_if_no_prev:
            result = self.par_cache[-1] if self.par_cache else None

        if result is not None and result.get_id() == par.get_id():
            print('WARNING: get_previous_par({}, {}) returning reference to self, returning None instead'.format(
                self.doc_id, par.get_id()))
            return None

        return result

    def get_pars_till(self, par):
        pars = []
        i = self.__iter__()
        try:
            while True:
                p = next(i)
                pars.append(p)
                if par.get_id() == p.get_id():
                    break
        except StopIteration:
            pass

        # TODO: improve this
        # 'i' might be a ListIterator or DocParagraphIter depending on whether the pars were cached
        try:
            i.close()
        except AttributeError:
            pass
        return pars

    def add_setting(self, key: str, value) -> None:
        current_settings = self.get_settings().get_dict()
        current_settings[key] = value
        self.set_settings(current_settings)

    def set_settings(self, settings: dict):
        first_par = None
        with self.__iter__() as i:
            for p in i:
                first_par = p
                break
        new_par = DocSettings(settings).to_paragraph(self)
        if first_par is None:
            self.add_paragraph_obj(new_par)
        else:
            if not first_par.is_setting():
                self.insert_paragraph_obj(new_par, insert_before_id=first_par.get_id())
            else:
                self.modify_paragraph_obj(first_par.get_id(), new_par)

    def get_tasks(self) -> Iterable[DocParagraph]:
        for p in self.get_dereferenced_paragraphs():
            if p.is_task():
                yield p

    def get_settings(self) -> DocSettings:
        if self.par_cache is not None:
            return DocSettings.from_paragraph(self.par_cache[0]) if len(self.par_cache) > 0 else DocSettings()
        try:
            i = self.__iter__()
            return DocSettings.from_paragraph(next(i))
        except StopIteration:
            return DocSettings()
        finally:
            i.close()

    def create(self, ignore_exists : bool = False):
        path = os.path.join(self.get_documents_dir(self.files_root), str(self.doc_id))
        if not os.path.exists(path):
            os.makedirs(path, exist_ok=True)
        elif not ignore_exists:
            raise DocExistsError(self.doc_id)

    def exists(self) -> bool:
        return Document.doc_exists(self.doc_id, self.files_root)

    def export_markdown(self, export_hashes : bool = False) -> str:
        return DocumentWriter([par.dict() for par in self], export_hashes=export_hashes).get_text()

    def export_section(self, par_id_start: Optional[str], par_id_end: Optional[str], export_hashes=False) -> str:
        return DocumentWriter([par.dict() for par in self.get_section(par_id_start, par_id_end)],
                              export_hashes=export_hashes).get_text()

    def get_section(self, par_id_start: Optional[str], par_id_end: Optional[str]) -> List[DocParagraph]:
        if par_id_start is None and par_id_end is None:
            return []
        if par_id_start is None or par_id_end is None:
            raise TimDbException('Either of par_id_start and par_id_end was None')
        all_pars = [par for par in self]
        all_par_ids = [par.get_id() for par in all_pars]
        start_index, end_index = all_par_ids.index(par_id_start), all_par_ids.index(par_id_end)
        return all_pars[start_index:end_index + 1]

    def text_to_paragraphs(self, text: str, break_on_elements: bool):
        options = DocumentParserOptions()
        options.break_on_code_block = break_on_elements
        options.break_on_header = break_on_elements
        options.break_on_normal = break_on_elements
        blocks = [DocParagraph.create(doc=self, md=par['md'], attrs=par.get('attrs'))
                  for par in DocumentParser(text).validate_structure(
                is_whole_document=False).get_blocks(options)]
        return blocks

    @classmethod
    def remove(cls, doc_id: int, files_root: Optional[str] = None, ignore_exists=False):
        """
        Removes the whole document.
        :param doc_id: Document id to remove.
        :return:
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        if cls.doc_exists(doc_id, files_root=froot):
            shutil.rmtree(os.path.join(cls.get_documents_dir(froot), str(doc_id)))
            # todo: remove all paragraph links
        elif not ignore_exists:
            raise DocExistsError(doc_id)

    @classmethod
    def get_next_free_id(cls, files_root: Optional[str] = None) -> int:
        """
        Gets the next free document id.
        :return:
        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        res = 1 + cls.__get_largest_file_number(cls.get_documents_dir(froot), default=0)
        return res

    def get_version(self) -> Tuple[int, int]:
        """
        Gets the latest version of the document as a major-minor tuple.
        :return: Latest version, or (-1, 0) if there isn't yet one.
        """
        if self.version is not None:
            return self.version
        basedir = os.path.join(self.get_documents_dir(self.files_root), str(self.doc_id))
        major = self.__get_largest_file_number(basedir, default=0)
        minor = 0 if major < 1 else self.__get_largest_file_number(os.path.join(basedir, str(major)), default=0)
        self.version = major, minor
        return major, minor

    def get_id_version(self) -> Tuple[int, int, int]:
        major, minor = self.get_version()
        return self.doc_id, major, minor

    def get_doc_version(self, version=None) -> 'Document':
        from documentmodel.documentversion import DocumentVersion
        return DocumentVersion(doc_id=self.doc_id,
                               doc_ver=version if version else self.get_version(),
                               files_root=self.files_root,
                               modifier_group_id=self.modifier_group_id)

    def get_document_path(self) -> str:
        return os.path.join(self.get_documents_dir(self.files_root), str(self.doc_id))

    def get_version_path(self, ver: Optional[Tuple[int, int]]=None) -> str:
        version = self.get_version() if ver is None else ver
        return os.path.join(self.get_documents_dir(self.files_root), str(self.doc_id), str(version[0]), str(version[1]))

    def get_refs_dir(self, ver: Optional[Tuple[int, int]]=None) -> str:
        version = self.get_version() if ver is None else ver
        return os.path.join(self.files_root, 'refs', str(self.doc_id), str(version[0]), str(version[1]))

    def get_reflist_filename(self, ver: Optional[Tuple[int, int]]=None) -> str:
        return os.path.join(self.get_refs_dir(ver), 'reflist_to')

    def getlogfilename(self) -> str:
        return os.path.join(self.get_document_path(), 'changelog')

    def __write_changelog(self, ver: Tuple[int, int], operation: str, par_id: str, op_params: Optional[dict] = None):
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

    def __increment_version(self, op: str, par_id: str, increment_major: bool,
                            op_params: Optional[dict] = None) -> Tuple[int, int]:
        ver_exists = True
        ver = self.get_version()
        while ver_exists:
            old_ver = ver
            ver = (old_ver[0] + 1, 0) if increment_major else (old_ver[0], old_ver[1] + 1)
            ver_exists = os.path.isfile(self.get_version_path(ver))
        if increment_major:
            os.mkdir(os.path.join(self.get_documents_dir(self.files_root), str(self.doc_id), str(ver[0])))
        if old_ver[0] > 0:
            shutil.copyfile(self.get_version_path(old_ver), self.get_version_path(ver))
        else:
            with open(self.get_version_path(ver), 'w'):
                pass
        self.__write_changelog(ver, op, par_id, op_params)
        self.version = ver
        self.par_cache = None
        self.par_map = None
        return ver

    def __update_metadata(self, pars: List[DocParagraph], old_ver: Tuple[int, int], new_ver: Tuple[int, int]):
        if old_ver == new_ver:
            raise TimDbException("__update_metadata called with old_ver == new_ver")
        new_reflist_file = self.get_reflist_filename(new_ver)
        reflist = self.get_referenced_document_ids(old_ver)
        for p in pars:
            if p.is_reference():
                try:
                    referenced_pars = p.get_referenced_pars()
                except TimDbException:
                    pass
                else:
                    for par in referenced_pars:
                        try:
                            reflist.add(int(par.get_doc_id()))
                        except (ValueError, TypeError):
                            print('Invalid document reference: ' + str(par.get_rd()))
        self.__save_reflist(new_reflist_file, reflist)

    def has_paragraph(self, par_id: str) -> bool:
        """
        Checks if the document has the given paragraph.
        :param par_id: The paragraph id.
        :return: Boolean.
        """
        file_name = self.get_version_path(self.get_version())
        if not os.path.isfile(file_name):
            return False

        with open(file_name, 'r') as f:
            while True:
                line = f.readline()
                if line == '':
                    return False
                if line.split('/', 1)[0].rstrip('\n') == par_id:
                    return True

    def get_paragraph(self, par_id: str) -> DocParagraph:
        return DocParagraph.get_latest(self, par_id, self.files_root)

    def add_text(self, text: str) -> Iterable[DocParagraph]:
        """Converts the given text to (possibly) multiple paragraphs and adds them to the document."""
        return [self.add_paragraph_obj(p) for p in self.text_to_paragraphs(text, False)]

    def add_paragraph_obj(self, p: DocParagraph) -> DocParagraph:
        """
        Appends a new paragraph into the document.
        :param p: Paragraph to be added.
        :return: The same paragraph object, or None if could not add.
        """
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

    def add_paragraph(
            self,
            text: str,
            par_id: Optional[str]=None,
            attrs: Optional[dict]=None,
            properties: Optional[dict]=None
            ) -> DocParagraph:
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

    def add_ref_paragraph(self, src_par: DocParagraph, text: Optional[str] = None,
                          attrs: Optional[dict] = None, properties: Optional[dict] = None) -> DocParagraph:

        ref_attrs = {} if attrs is None else attrs.copy()
        ref_attrs['rp'] = src_par.get_id()
        ref_attrs['rt'] = src_par.get_hash()

        rd = src_par.get_doc_id()
        if self.get_settings().get_source_document() != rd:
            ref_attrs['rd'] = str(rd)
        if text is not None:
            ref_attrs['r'] = 'tr'
        else:
            text = ''

        return self.add_paragraph(text, attrs=ref_attrs, properties=properties)

    def add_area_ref_paragraph(self, src_doc: 'Document', src_area_name: str, text: Optional[str] = None,
                               attrs: Optional[dict] = None, properties: Optional[dict] = None) -> DocParagraph:

        ref_attrs = {} if attrs is None else attrs.copy()
        ref_attrs['ra'] = src_area_name
        ref_attrs.pop('rt', None)

        if self.get_settings().get_source_document() != src_doc.doc_id:
            ref_attrs['rd'] = str(src_doc.doc_id)
        if text is not None:
            ref_attrs['r'] = 'tr'
        else:
            text = ''

        return self.add_paragraph(text, attrs=ref_attrs, properties=properties)

    def delete_paragraph(self, par_id: str):
        """
        Removes a paragraph from the document.
        :param par_id: Paragraph id to remove.
        """
        if not self.has_paragraph(par_id):
            return

        old_ver = self.get_version()
        new_ver = self.__increment_version('Deleted', par_id, increment_major=True)
        self.__update_metadata([], old_ver, new_ver)

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

    def insert_paragraph(self, text: str,
                         insert_before_id: Optional[str] = '',
                         insert_after_id: Optional[str] = '',
                         attrs: Optional[dict]=None, properties: Optional[dict]=None,
                         par_id: Optional[str]=None) -> DocParagraph:
        """
        Inserts a paragraph before a given paragraph id.
        :param par_id: The id of the new paragraph or None if it should be autogenerated.
        :param attrs: The attributes for the paragraph.
        :param text: New paragraph text.
        :param insert_before_id: Id of the paragraph to insert before, or None if last.
        :param insert_after_id: Id of the paragraph to insert after, or None if first.
        :return: The inserted paragraph object.
        """
        if insert_before_id == '' and insert_after_id == '':
            raise TimDbException('insert_paragraph: missing argument insert_before_id or insert_after_id')

        p = DocParagraph.create(
            doc=self,
            par_id=par_id,
            md=text,
            attrs=attrs,
            props=properties,
            files_root=self.files_root
        )
        return self.insert_paragraph_obj(p, insert_before_id=insert_before_id, insert_after_id=insert_after_id)

    def insert_paragraph_obj(self, p: DocParagraph,
                             insert_before_id: Optional[str] = '',
                             insert_after_id: Optional[str] = '') -> DocParagraph:

        if insert_before_id == '' and insert_after_id == '':
            raise TimDbException('insert_paragraph_obj: missing argument insert_before_id or insert_after_id')

        if insert_before_id is None:
            return self.add_paragraph_obj(p)

        p.add_link(self.doc_id)
        p.set_latest()
        old_ver = self.get_version()
        new_ver = self.__increment_version('Inserted', p.get_id(), increment_major=True,
                                           op_params={'before_id': insert_before_id})
        self.__update_metadata([p], old_ver, new_ver)

        new_line = p.get_id() + '/' + p.get_hash() + '\n'
        line = None
        with open(self.get_version_path(old_ver), 'r') as f_src:
            with open(self.get_version_path(new_ver), 'w') as f:
                while True:
                    if line is None and insert_before_id is None:
                        f.write(new_line)

                    line = f_src.readline()
                    if not line:
                        return p

                    if insert_before_id and line.startswith(insert_before_id):
                        f.write(new_line)
                    f.write(line)
                    if insert_after_id and line.startswith(insert_after_id):
                        f.write(new_line)

    def modify_paragraph(self, par_id: str, new_text: str, new_attrs: Optional[dict]=None,
                         new_properties: Optional[dict]=None) -> DocParagraph:
        """
        Modifies the text of the given paragraph.
        :param par_id: Paragraph id.
        :param new_text: New text.
        :param new_attrs: New attributes.
        :param new_properties: New properties.
        :return: The new paragraph object.
        """

        if new_attrs is None:
            new_attrs = self.get_paragraph(par_id).get_attrs()
        if new_properties is None:
            new_properties = self.get_paragraph(par_id).get_properties()

        p = DocParagraph.create(
            md=new_text,
            doc=self,
            par_id=par_id,
            attrs=new_attrs,
            props=new_properties,
            files_root=self.files_root
        )
        return self.modify_paragraph_obj(par_id, p)

    def modify_paragraph_obj(self, par_id: str, p: DocParagraph) -> DocParagraph:
        if not self.has_paragraph(par_id):
            raise KeyError('No paragraph {} in document {} version {}'.format(par_id, self.doc_id, self.get_version()))

        p_src = DocParagraph.get_latest(self, par_id, files_root=self.files_root)
        p_src.remove_link(self.doc_id)
        p.set_id(par_id)
        new_hash = p.get_hash()
        p.add_link(self.doc_id)
        p.set_latest()
        old_ver = self.get_version()
        old_hash = p_src.get_hash()
        new_ver = self.__increment_version('Modified', par_id, increment_major=False,
                                           op_params={'old_hash': old_hash, 'new_hash': new_hash})
        self.__update_metadata([p], old_ver, new_ver)

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

    def parwise_diff(self, other_doc: 'Document', check_html:bool=False):
        if self.get_version() == other_doc.get_version():
            raise StopIteration
        old_pars = [par for par in self]
        old_ids = [par.get_id() for par in old_pars]
        new_pars = [par for par in other_doc]
        new_ids = [par.get_id() for par in new_pars]
        s = SequenceMatcher(None, old_ids, new_ids)
        opcodes = s.get_opcodes()
        if check_html:
            DocParagraph.preload_htmls(old_pars, self.get_settings(), persist=False)
            DocParagraph.preload_htmls(new_pars, other_doc.get_settings(), persist=False)
        for tag, i1, i2, j1, j2 in opcodes:
            if tag == 'insert':
                yield {'type': tag, 'after_id': old_ids[i2 - 1] if i2 > 0 else None, 'content': new_pars[j1:j2]}
            if tag == 'replace':
                yield {'type': tag, 'start_id': old_ids[i1], 'end_id': old_ids[i2] if i2 < len(old_ids) else None, 'content': new_pars[j1:j2]}
            if tag == 'delete':
                yield {'type': tag, 'start_id': old_ids[i1], 'end_id': old_ids[i2] if i2 < len(old_ids) else None}
            if tag == 'equal':
                for old, new in zip(old_pars[i1:i2], new_pars[j1:j2]):
                    if old != new:
                        yield {'type': 'change', 'id': old.get_id(), 'content': [new]}
                    elif check_html and not old.is_same_as_html(new):
                        yield {'type': 'change', 'id': old.get_id(), 'content': [new]}

    def update_section(self, text: str, par_id_first: str, par_id_last: str) -> Tuple[str, str]:
        """Updates a section of the document.

        :param text: The text of the section.
        :param par_id_first: The id of the paragraph that denotes the start of the section.
        :param par_id_last: The id of the paragraph that denotes the end of the section.
        """
        new_pars = DocumentParser(text).add_missing_attributes().validate_structure(is_whole_document=False).get_blocks()
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

    def update(self, text: str, original: str, strict_validation=True):
        """Replaces the document's contents with the specified text.

        :param text: The new text for the document.
        :param original: The original text for the document.
        :param strict_validation: Whether to use stricter validation rules for areas etc.
        """
        new_pars = DocumentParser(text).add_missing_attributes().validate_structure(is_whole_document=strict_validation).get_blocks()

        # If the original document has validation errors, it probably means the document export routine has a bug.
        try:
            old_pars = [DocParagraph.from_dict(doc=self, d=d)
                        for d in DocumentParser(original).add_missing_attributes().validate_structure(is_whole_document=strict_validation).get_blocks()]
        except AttributesAtEndOfCodeBlockException as e:
            raise ValidationException('The original document contained a syntax error. '
                                      'This is probably a TIM bug; please report it. '
                                      'Additional information: {}'.format(e))

        self._perform_update(new_pars, old_pars)

    def _perform_update(self, new_pars: List[dict],
                        old_pars: List[DocParagraph],
                        last_par_id=None) -> Union[Tuple[str, str], Tuple[None, None]]:
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
        if not new_ids:
            return None, None
        return new_ids[0], new_ids[-1]

    def find_insert_index(self, i2, old_ids):
        before_i = i2
        while before_i < len(old_ids) and not self.has_paragraph(old_ids[before_i]):
            before_i += 1
        return before_i

    def get_index(self) -> List[Tuple]:
        pars = [par for par in DocParagraphIter(self)]
        DocParagraph.preload_htmls(pars, self.get_settings())
        pars = dereference_pars(pars, source_doc=self.get_original_document())

        # Skip plugins
        html_list = [par.get_html(from_preview=False) for par in pars if not par.is_dynamic()]
        return get_index_from_html_list(html_list)

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

    def get_changelog(self, max_entries: int = 100) -> List[dict]:
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
                    entry = json.loads(line)
                    entry['time'] = dateutil.parser.parse(entry['time']).replace(tzinfo=timezone.utc)
                    log.append(entry)
                except ValueError:
                    print("doc id {}: malformed log line: {}".format(self.doc_id, line))
                lc -= 1

        return log

    def get_paragraph_by_task(self, task_id_name) -> DocParagraph:
        with self.__iter__() as it:
            for p in it:
                if p.get_attr('taskId') == task_id_name:
                    return p
                if p.is_reference():
                    try:
                        ref_pars = p.get_referenced_pars()
                    except TimDbException:  # Ignore invalid references
                        continue
                    else:
                        for rp in ref_pars:
                            if rp.get_attr('taskId') == task_id_name:
                                return rp
        raise TimDbException('Task not found in the document: {}'.format(task_id_name))

    def get_last_modified(self) -> Optional[datetime]:
        log = self.get_changelog(max_entries=1)
        return log[0]['time'] if log is not None and len(log) > 0 else None

    def delete_section(self, area_start, area_end):
        all_par_ids = [par.get_id() for par in self]
        start_index, end_index = all_par_ids.index(area_start), all_par_ids.index(area_end)
        old_pars = all_par_ids[start_index:end_index + 1]
        for par in old_pars:
            self.delete_paragraph(par)

    def get_named_section(self, section_name: str):
        start_found = False
        end_found = False
        pars = []
        with self.__iter__() as i:
            for par in i:
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

    def named_section_exists(self, section_name: str) -> bool:
        with self.__iter__() as i:
            for par in i:
                if par.get_attr('area') == section_name:
                    return True
        return False

    def calculate_referenced_document_ids(self, ver: Optional[Tuple[int, int]]=None) -> Set[int]:
        """Gets all the document ids that are referenced from this document recursively.
        :return: The set of the document ids.
        """

        refs = set()
        source = self
        if ver is not None:
            from documentmodel.documentversion import DocumentVersion
            source = DocumentVersion(self.doc_id, ver, self.files_root)

        for p in source:
            if p.is_reference():
                try:
                    referenced_pars = p.get_referenced_pars()
                except TimDbException:
                    pass
                else:
                    for par in referenced_pars:
                        try:
                            refs.add(int(par.get_doc_id()))
                        except (ValueError, TypeError):
                            print('Invalid document reference: ' + str(par.get_rd()))
        return refs

    def __load_reflist(self, reflist_name: str) -> Set[int]:
        with open(reflist_name, 'r') as reffile:
            return set(json.loads(reffile.read()))

    def __save_reflist(self, reflist_name: str, reflist: Set[int]):
        reflist_dir = os.path.dirname(reflist_name)
        os.makedirs(reflist_dir, exist_ok=True)

        with open(reflist_name, 'w') as reffile:
            reffile.write(json.dumps(list(reflist)))

    def get_referenced_document_ids(self, ver: Optional[Tuple[int, int]]=None) -> Set[int]:
        reflist_name = self.get_reflist_filename(ver)
        if os.path.isfile(reflist_name):
            reflist = self.__load_reflist(reflist_name)
        else:
            reflist = self.calculate_referenced_document_ids(ver)
            self.__save_reflist(reflist_name, reflist)
        return reflist

    def get_paragraphs(self) -> List[DocParagraph]:
        return [par for par in self]

    def get_dereferenced_paragraphs(self) -> List[DocParagraph]:
        return dereference_pars(self.get_paragraphs(), source_doc=self.get_original_document())

    def get_closest_paragraph_title(self, par_id: Optional[str]):
        last_title = None
        for par in self:
            title = par.get_title()
            if title is not None:
                last_title = title
            if par.get_id() == par_id:
                return last_title

        return None

    def get_latest_version(self):
        from documentmodel.documentversion import DocumentVersion
        return DocumentVersion(self.doc_id, self.get_version(), self.files_root, self.modifier_group_id)

    def get_original_document(self):
        src_docid = self.get_settings().get_source_document()
        return Document(src_docid) if src_docid is not None else None

    def get_last_par(self):
        pars = [par for par in self]
        return pars[-1] if pars else None

    def insert_temporary_pars(self, pars, context_par):
        self.load_pars()

        if context_par is None:
            self.par_cache = pars + self.par_cache
        else:
            i = 0
            for i, par in enumerate(self.par_cache):
                if par.get_id() == context_par.get_id():
                    break
            self.par_cache = self.par_cache[:i+1] + pars + self.par_cache[i+1:]
        self.__update_par_map()

    def clear_mem_cache(self):
        self.par_cache = None
        self.par_map = None
        self.version = None


class CacheIterator:
    def __init__(self, i):
        self.i = i

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass

    def __iter__(self):
        return self.i

    def __next__(self) -> DocParagraph:
        return self.i.__next__()


class DocParagraphIter:
    def __init__(self, doc: Document):
        self.doc = doc
        self.next_index = 0
        name = doc.get_version_path(doc.get_version())
        self.f = open(name, 'r') if os.path.isfile(name) else None

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    def __iter__(self):
        return self

    def __next__(self) -> DocParagraph:
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


def get_index_from_html_list(html_table) -> List[Tuple]:
    index = []
    current_headers = None
    for htmlstr in html_table:
        try:
            index_entry = html.fragment_fromstring(htmlstr, create_parent=True)
        except etree.XMLSyntaxError:
            continue
        if index_entry.tag == 'div':
            for header in index_entry.iter('h1', 'h2', 'h3'):
                current_headers = Document.add_index_entry(index, current_headers, header)
        elif index_entry.tag.startswith('h'):
            current_headers = Document.add_index_entry(index, current_headers, index_entry)
    if current_headers is not None:
        index.append(current_headers)
    return index


def dereference_pars(pars: Iterable[DocParagraph], source_doc: Optional[Document]=None) -> List[DocParagraph]:
    """Resolves references in the given paragraphs.

    :param pars: The DocParagraphs to be processed.
    :param source_doc: Default document for referencing.
    """
    new_pars = []
    for par in pars:
        if par.is_reference():
            try:
                new_pars += par.get_referenced_pars(source_doc=source_doc)
            except TimDbException as e:
                err_par = DocParagraph.create(
                    par.doc,
                    par_id=par.get_id(),
                    md='',
                    html=get_error_html(e))

                new_pars.append(err_par)
        else:
            new_pars.append(par)

    return new_pars
