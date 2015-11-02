from copy import deepcopy
import os

from cachetools import cached, LRUCache
from contracts import contract, new_contract

from documentmodel.documentwriter import DocumentWriter
from htmlSanitize import sanitize_html
from markdownconverter import md_to_html
from .randutils import *
from timdb.timdbbase import TimDbException


class DocParagraphBase:
    pass


# This is so the DocParagraph contract can be used in the class itself
new_contract('DocParagraph', DocParagraphBase)


class DocParagraph(DocParagraphBase):
    default_files_root = 'tim_files'

    @contract
    def __init__(self, doc, files_root: 'str|None'=None):
        self.doc = doc
        self.original = None
        self.files_root = self.get_default_files_root() if files_root is None else files_root
        self.html_sanitized = False
        self.html = None
        self.__htmldata = None

    @classmethod
    @contract
    def create(cls,
               doc,
               par_id: 'str|None' = None,
               md: 'str' = '',
               html: 'str|None' = None,
               attrs: 'dict|None' = None,
               props: 'dict|None' = None,
               files_root: 'str|None' = None) -> 'DocParagraph':

        par = DocParagraph(doc, files_root)
        par.html = html
        par.__data = {
            'id': random_id() if par_id is None else par_id,
            'md': md,
            't': hashfunc(md, attrs),
            'links': [],
            'attrs': {} if attrs is None else attrs,
            'props': {} if props is None else props
        }
        par._cache_props()
        return par

    @classmethod
    @contract
    def from_dict(cls, doc, d: 'dict', files_root: 'str|None' = None) -> 'DocParagraph':
        par = DocParagraph(doc, files_root)
        par.__data = dict(d)
        par._cache_props()
        return par

    @classmethod
    @contract
    def get_latest(cls, doc, par_id: 'str', files_root: 'str|None' = None, cache: 'bool' = True) -> 'DocParagraph':
        froot = cls.get_default_files_root() if files_root is None else files_root
        try:
            t = os.readlink(cls._get_path(doc, par_id, 'current', froot))
            if cache:
                return cls.get(doc, par_id, t, files_root=froot)
            else:
                return cls.__get.__wrapped__(cls, doc, par_id, t, files_root=froot)
        except FileNotFoundError as e:
            return DocParagraph.create(doc, par_id, 'ERROR: File not found! ' + str(e), files_root=files_root)

    @classmethod
    @contract
    def get(cls, doc, par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'DocParagraph':
        try:
            par = cls.__get(doc, par_id, t, files_root)

            # Clear html attribute because we don't want it to be in __get's cache.
            par.html = None

            # Update document reference because the document may have been modified; we don't want to use the
            # old reference from cache.
            par.doc = doc

            return par
        except FileNotFoundError as e:
            return DocParagraph.create(doc, par_id, 'ERROR: File not found! ' + str(e), files_root=files_root)

    @classmethod
    @cached(cache=LRUCache(maxsize=65536), key=lambda cls, doc, par_id, t, files_root: (doc.doc_id, par_id, t))
    @contract
    def __get(cls, doc, par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'DocParagraph':
        """Loads a paragraph from file system based on given parameters.
        """
        with open(cls._get_path(doc, par_id, t, files_root), 'r') as f:
            return cls.from_dict(doc, json.loads(f.read()), files_root=files_root)

    def __iter__(self):
        return self.__data.__iter__()

    @classmethod
    def get_default_files_root(cls):
        return cls.default_files_root

    @classmethod
    @contract
    def _get_path(cls, doc, par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'str':
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', str(doc.doc_id), par_id, t)

    @classmethod
    @contract
    def _get_base_path(cls, doc_id: 'int', par_id: 'str', files_root: 'str|None' = None) -> 'str':
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', str(doc_id), par_id)

    @contract
    def dict(self) -> 'dict':
        return self.__data

    def _mkhtmldata(self):
        self._cache_props()

        if self.original:
            self.__htmldata = dict(self.original.__data)
            self.__htmldata['attrs_str'] = self.original.get_attrs_str()
            self.__htmldata['doc_id'] = self.original.doc.doc_id

            self.__htmldata['ref_doc_id'] = self.doc.doc_id
            self.__htmldata['ref_id'] = self.__data['id']
            self.__htmldata['ref_t'] = self.__data['t']
            self.__htmldata['ref_attrs'] = self.__data['attrs']
            self.__htmldata['ref_attrs_str'] = self.get_attrs_str()
        else:
            self.__htmldata = dict(self.__data)
            self.__htmldata['attrs_str'] = self.get_attrs_str()
            self.__htmldata['doc_id'] = self.doc.doc_id

        try:
            self.__htmldata['html'] = self.get_html()
        except Exception as e:
            self.__htmldata['html'] = '<div class="pluginError">{}</div>'.format(e)
        self.__htmldata['cls'] = 'par ' + self.get_class_str()
        self.__htmldata['is_plugin'] = self.is_plugin()
        self.__htmldata['needs_browser'] = True #self.is_plugin() and containerLink.get_plugin_needs_browser(self.get_attr('plugin'))

    def _cache_props(self):
        self.__is_plugin = self.get_attr('plugin') or ""  # self.get_attr('taskId')
        self.__is_ref = self.is_par_reference() or self.is_area_reference()
        self.__is_setting = 'settings' in self.get_attrs()

    @contract
    def html_dict(self) -> 'dict':
        #if self.__htmldata is None:
        self._mkhtmldata()
        return self.__htmldata

    @contract
    def get_doc_id(self) -> 'int':
        return self.doc.doc_id

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
        curr_html = self.html
        if curr_html:
            return curr_html
        if self.is_setting():
            new_html = '<p class="docsettings">&nbsp;</p>'
        else:
            macros, delimiter = self.__get_macro_info(self.doc)
            new_html = self.__get_html_using_macros(macros, delimiter)
        self.__set_html(new_html)
        return new_html

    @contract
    @cached(cache=LRUCache(maxsize=65536),
            key=lambda self, macros, delimiter: (self.doc.doc_id, self.get_id(), self.get_hash(), str(macros), delimiter))
    def __get_html_using_macros(self, macros: 'dict(str,str)', macro_delimiter: 'str') -> 'str':
        return md_to_html(self.get_markdown(), sanitize=True, macros=macros, macro_delimiter=macro_delimiter)

    @contract
    def get_ref_html(self, classname="parref", write_link=False, link_class="parlink", linked_paragraph=None) -> 'str':
        if linked_paragraph is None:
            linked_paragraph = self
        linkhtml = ''
        if write_link:
            from documentmodel.document import Document
            doc = Document(linked_paragraph.get_doc_id())
            linkhtml = '<a class="{2}" href="/view/{0}">{1}</a>'.format(linked_paragraph.get_doc_id(),
                                                                        doc.get_name(), link_class)
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

    def sanitize_html(self):
        if self.html_sanitized or not self.html:
            return
        new_html = sanitize_html(self.html)
        self.__set_html(new_html, True)

    @contract
    def __set_html(self, new_html: 'str', sanitized=False):
        self.html = new_html
        if self.__htmldata is not None:
            self.__htmldata['html'] = new_html
        self.html_sanitized = sanitized

    @contract
    def __set_trans_html(self, referencing_par: 'DocParagraph', show_original: 'bool',
                       from_class: 'str' = 'partranslatefrom', to_class: 'str' = 'partranslate',
                       show_link: 'bool' = False):
        if show_original:
            html = '{}\n{}'.format(referencing_par.get_ref_html(classname=to_class, write_link=show_link,
                                                                link_class="trlink", linked_paragraph=self),
                                   self.get_ref_html(classname=from_class, link_class="trfromlink",
                                                     write_link=show_link))
        else:
            html = referencing_par.get_ref_html(classname=to_class, write_link=show_link, link_class="trlink",
                                                linked_paragraph=self)

        self.__set_html(html)

    @contract
    def get_links(self) -> 'list(str)':
        return self.__data['links']

    @contract
    def get_attr(self, attr_name: 'str', default_value=None, dereference=False):
        if dereference and self.original:
            return self.original.get_attr(attr_name, default_value, True)

        return self.__data['attrs'].get(attr_name, default_value)

    @contract
    def set_attr(self, attr_name: 'str', attr_val, dereference=False):
        if dereference and self.original:
            self.original.set_attr(attr_name, attr_val, True)
        else:
            self.__data['attrs'][attr_name] = attr_val

        if attr_name == 'taskId':
            self.__is_plugin = bool(attr_val)
        elif attr_name == 'rd':
            self.__is_ref = bool(attr_val)

    @contract
    def get_attrs(self) -> 'dict':
        return self.__data['attrs']

    @contract
    def get_properties(self) -> 'dict':
        if 'props' in self.__data:
            return self.__data['props']
        else:
            return {}

    @contract
    def is_multi_block(self) -> 'bool':
        properties = self.get_properties()
        is_multi_block = False
        if 'multi_block' in properties:
            is_multi_block = properties['multi_block']
        return is_multi_block

    @contract
    def has_headers(self) -> 'bool':
        properties = self.get_properties()
        has_headers = False
        if 'has_headers' in properties:
            has_headers = properties['has_headers']
        return has_headers

    @contract
    def get_attrs_str(self) -> 'str':
        return json.dumps(self.__data['attrs'])

    @contract
    def get_class_str(self) -> 'str':
        return ' '.join(self.get_attr('classes', []))

    @contract
    def get_base_path(self) -> 'str':
        return self._get_base_path(self.get_doc_id(), self.get_id(), files_root=self.files_root)

    @contract
    def get_path(self) -> 'str':
        return self._get_path(self.doc, self.get_id(), self.get_hash(), files_root=self.files_root)

    def __read(self):
        if not os.path.isfile(self.get_path()):
            return False
        with open(self.get_path(), 'r') as f:
            self.__data = json.loads(f.read())
            self._cache_props()
            self.__htmldata = None
            return True

    def __write(self):
        file_name = self.get_path()
        should_exist = len(self.get_links()) > 0
        does_exist = os.path.isfile(file_name)

        if does_exist and not should_exist:
            # Uncomment to remove old versions
            #os.unlink(file_name)
            base_path = self.get_base_path()
            if os.listdir(base_path) == ['current']:
                os.unlink(os.path.join(base_path, 'current'))
                if os.path.islink(base_path):
                    os.unlink(base_path)
                else:
                    os.rmdir(base_path)

        if not should_exist:
            return

        if not does_exist and should_exist:
            base_path = self.get_base_path()
            if not os.path.exists(base_path):
                os.makedirs(base_path)

        with open(file_name, 'w') as f:
            f.write(json.dumps(self.__data))

    def set_latest(self):
        linkpath = self._get_path(self.doc, self.get_id(), 'current', files_root=self.files_root)
        if linkpath == self.get_hash():
            return
        if os.path.islink(linkpath) or os.path.isfile(linkpath):
            os.unlink(linkpath)
        os.symlink(self.get_hash(), linkpath)

    @contract
    def add_link(self, doc_id: 'int'):
        #self.__read()
        self.__data['links'].append(str(doc_id))
        self.__write()

    @contract
    def remove_link(self, doc_id: 'int'):
        self.__read()
        if str(doc_id) in self.__data['links']:
            self.__data['links'].remove(str(doc_id))
        elif doc_id in self.__data['links']:
            self.__data['links'].remove(doc_id)
        else:
            print("Couldn't remove link... links contains:")
            print(self.__data['links'])
        self.__write()

    def update_links(self):
        self.__read()
        self.__write()

    def is_reference(self):
        return self.__is_ref

    def is_par_reference(self):
        return self.get_attr('rd') is not None and self.get_attr('rp') is not None

    def is_area_reference(self):
        return self.get_attr('rd') is not None and self.get_attr('ra') is not None

    def __repr__(self):
        return self.__data.__repr__()

    def get_referenced_pars(self, edit_window=False, set_html=True):
        def reference_par(ref_par, attrs, classname='parref', trclassname='partranslate', write_link=False):
            par = deepcopy(ref_par)
            par.set_attr('classes', (self.get_attr('classes') or []) + (ref_par.get_attr('classes') or []))
            if set_html:
                if 'r' in attrs and attrs['r'] == 'tr':
                    par.__set_trans_html(self, edit_window, classname, trclassname, write_link)
                else:
                    par.__set_html(ref_par.get_ref_html(classname=classname, write_link=write_link))
            par.set_original(self)
            return par

        attrs = self.get_attrs()
        from documentmodel.document import Document  # Document import needs to be here to avoid circular import
        try:
            ref_doc = Document(int(attrs['rd']))
        except ValueError as e:
            raise TimDbException('Invalid reference document id: "{}"'.format(attrs['rd']))
        if not ref_doc.exists():
            raise TimDbException('The referenced document does not exist.')
        if self.is_par_reference():
            if self.get_doc_id() == int(attrs['rd']) and self.get_id() == attrs['rp']:
                raise TimDbException('Paragraph is referencing itself!')
            if not ref_doc.has_paragraph(attrs['rp']):
                raise TimDbException('The referenced paragraph does not exist.')

            ref_par = DocParagraph.get_latest(ref_doc, attrs['rp'], ref_doc.files_root, cache=False)
            return [reference_par(ref_par, attrs, write_link=True)]

        elif self.is_area_reference():
            ref_pars = ref_doc.get_named_section(attrs['ra'])
            pars = []
            n = len(ref_pars)

            if n == 1:
                return [reference_par(ref_pars[0], attrs, write_link=True)]

            for i in range(0, len(ref_pars)):
                if i == 0:
                    par = reference_par(ref_pars[i], attrs, classname="parref-begin", write_link=True)
                elif i == n - 1:
                    par = reference_par(ref_pars[i], attrs, classname="parref-end")
                else:
                    par = reference_par(ref_pars[i], attrs, classname="parref-mid")

                pars.append(par)

            return pars
        else:
            assert False

    def set_original(self, orig):
        self.original = orig
        self._cache_props()
        self.__htmldata = None

    def get_original(self):
        return self.original

    def is_plugin(self):
        return self.__is_plugin

    def is_setting(self):
        return self.__is_setting

    @classmethod
    @cached(cache=LRUCache(maxsize=65536), key=lambda cls, doc: doc.get_id_version())
    def __get_macro_info(cls, doc):
        if doc is None:
            return None, None
        settings = doc.get_settings()
        if settings is None:
            return None, None
        return settings.get_macros(), settings.get_macro_delimiter()
