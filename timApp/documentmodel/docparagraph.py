from copy import deepcopy
import os

import shelve
from contracts import contract, new_contract
from fcache.cache import FileCache
from lxml import html

from documentmodel.documentwriter import DocumentWriter
from dumboclient import call_dumbo
from htmlSanitize import sanitize_html
from markdownconverter import md_to_html, md_list_to_html_list, expand_macros, HEADING_TAGS
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
               t: 'str|None' = None,
               html: 'str|None' = None,
               attrs: 'dict|None' = None,
               props: 'dict|None' = None,
               files_root: 'str|None' = None) -> 'DocParagraph':

        par = DocParagraph(doc, files_root)
        par.html = html
        par.__data = {
            'id': random_id() if par_id is None else par_id,
            'md': md,
            't': hashfunc(md, attrs) if t is None else t,
            'links': [],
            'attrs': {} if attrs is None else attrs,
            'props': {} if props is None else props
        }
        par._cache_props()
        return par

    @contract
    def create_reference(self, doc, r: 'str|None' = None, add_rd: 'bool' = True) -> 'DocParagraph':
        if 'r' == 'tr':
            par = DocParagraph.create(doc, files_root=self.files_root, md=self.get_markdown(),
                                      attrs=self.get_attrs(), props=self.get_properties())
        else:
            par = DocParagraph.create(doc, files_root=self.files_root)

        par.set_attr('r', r)
        par.set_attr('rd', self.get_doc_id() if add_rd else None)
        par.set_attr('rp', self.get_id())
        par.set_attr('ra', None)

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
    def get_latest(cls, doc, par_id: 'str', files_root: 'str|None' = None) -> 'DocParagraph':
        froot = cls.get_default_files_root() if files_root is None else files_root
        try:
            t = os.readlink(cls._get_path(doc, par_id, 'current', froot))
            return cls.get(doc, par_id, t, files_root=froot)
        except FileNotFoundError as e:
            return DocParagraph.create(doc, par_id, 'ERROR: File not found! ' + str(e), files_root=files_root)

    @classmethod
    @contract
    def get(cls, doc, par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'DocParagraph':
        try:
            """Loads a paragraph from file system based on given parameters.
            """
            with open(cls._get_path(doc, par_id, t, files_root), 'r') as f:
                return cls.from_dict(doc, json.loads(f.read()), files_root=files_root)
        except FileNotFoundError as e:
            return DocParagraph.create(doc, par_id, 'ERROR: File not found! ' + str(e), files_root=files_root)

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
        self._mkhtmldata()
        return self.__htmldata

    @contract
    def get_doc_id(self) -> 'int':
        return self.doc.doc_id

    @contract
    def get_id(self) -> 'str':
        return self.__data['id']

    @contract
    def get_rd(self) -> 'int|str|None':
        if 'rd' in self.__data['attrs']:
            return self.get_attr('rd')

        default_rd = self.doc.get_settings().get_source_document()
        if default_rd is not None:
            return  default_rd

        return None

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
    def get_title(self) -> 'str|None':
        md = self.__data['md']
        if len(md) < 3 or md[0] != '#' or md[1] == '-':
            return None

        attr_index = md.find('{')
        return md[2:attr_index].strip() if attr_index > 0 else md[2:].strip()

    @contract
    def get_exported_markdown(self) -> 'str':
        return DocumentWriter([self.__data], export_hashes=False, export_ids=False).get_text()

    @contract
    def __get_setting_html(self) -> 'str':
        from documentmodel.docsettings import DocSettings

        if DocSettings.is_valid_paragraph(self):
            return '<p class="docsettings">&nbsp;</p>'
        else:
            return '<div class="pluginError">Invalid settings paragraph detected</div>'

    @contract
    def get_html(self) -> 'str':
        if self.html:
            return self.html
        if self.is_plugin():
            return ''
        if self.is_setting():
            return self.__get_setting_html()

        macros, delimiter = self.__get_macro_info(self.doc)
        m = str(macros) + delimiter
        cached = self.__data.get('h')
        # auto_macros = self.get_auto_macro_values(macros, delimiter)
        macro_hash = hashfunc(m)
        # auto_macro_hash = hashfunc(m + str(auto_macros))
        if cached is None or type(cached) is str:
            self.__data['h'] = {}
            new_html = self.__get_html_using_macros(macros,
                                                    delimiter)
        else:
            new_html = cached.get(macro_hash)
            if new_html is None:
                # Get html from Dumbo (slow!)
                new_html = self.__get_html_using_macros(macros,
                                                        delimiter)

        self.__set_html(new_html)
        return new_html

    @classmethod
    @contract
    def preload_htmls(cls, pars: 'list(DocParagraph)', settings, clear_cache=False):
        """
        Asks for paragraphs in batch from Dumbo to avoid multiple requests.
        :param clear_cache: Whether the HTML cache ('h' key) should be refreshed.
        :param settings: The document settings.
        :param pars: Paragraphs to preload.
        """
        unloaded_pars = []
        macros = settings.get_macros() if settings else None
        macro_delim = settings.get_macro_delimiter() if settings else None
        m = str(macros) + macro_delim

        dyn = 0
        l = 0

        cache = shelve.open('/tmp/tim_auto_macros')
        for par in pars:
            if par.is_dynamic():
                dyn += 1
                continue
            if not clear_cache and par.html is not None:
                continue
            cached = par.__data.get('h')
            auto_macros = par.get_auto_macro_values(macros, macro_delim, cache)
            auto_macro_hash = hashfunc(m + str(auto_macros))
            tup = (par, auto_macro_hash, auto_macros)
            if cached is not None:
                if clear_cache or type(cached) is str:
                    par.__data['h'] = {}
                    unloaded_pars.append(tup)
                else:
                    if auto_macro_hash in cached:
                        par.html = cached[auto_macro_hash]
                        l += 1
                    else:
                        unloaded_pars.append(tup)
            else:
                par.__data['h'] = {}
                unloaded_pars.append(tup)
        cache.close()

        # print("{} paragraphs are marked dynamic".format(dyn))
        # print("{} paragraphs are cached".format(l))
        # print("{} paragraphs are not cached".format(len(unloaded_pars)))

        if len(unloaded_pars) > 0:
            htmls = md_list_to_html_list([par.get_markdown() for par, _, _ in unloaded_pars],
                                         macros=macros,
                                         macro_delimiter=macro_delim,
                                         auto_macros=[auto_macros for _, _, auto_macros in unloaded_pars],
                                         auto_number_headings=settings.auto_number_headings())
            for (par, auto_macro_hash, _), h in zip(unloaded_pars, htmls):
                par.__data['h'][auto_macro_hash] = h
                par.html = h
                par.__write()

    def get_auto_macro_values(self, macros, macro_delim, cache):
        key = str((self.get_id(), self.doc.get_version()))
        cached = cache.get(key)
        if cached is not None:
            return cached

        prev_par = self.doc.get_previous_par(self)
        if prev_par is None:
            prev_par_auto_values = {'h': {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 0}}
        else:
            prev_par_auto_values = prev_par.get_auto_macro_values(macros, macro_delim, cache)

        if prev_par is None or prev_par.is_dynamic():
            cache[key] = prev_par_auto_values
            return prev_par_auto_values

        pre_html = prev_par.__get_html_using_macros(macros, macro_delim)
        tree = html.fragment_fromstring(pre_html, create_parent=True).getchildren()
        if len(tree) == 1 and tree[0].tag == 'div':
            tree = tree[0].getchildren()
        deltas = {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 0}
        for e in tree:
            if e.tag in HEADING_TAGS:
                level = int(e.tag[1])
                deltas[level] += 1
                for i in range(level + 1, 7):
                    deltas[i] = 0
        result = prev_par_auto_values
        found_nonzero = False
        for i in range(1, 7):
            if found_nonzero:
                result['h'][i] = 0
            result['h'][i] += deltas[i]
            found_nonzero = found_nonzero or deltas[i] > 0
        cache[key] = result
        return result

    @contract
    def __get_html_using_macros(self, macros: 'dict(str:str)|None',
                                macro_delimiter: 'str|None',
                                auto_macros: 'dict|None'=None,
                                auto_number_headings: 'bool'=False) -> 'str':
        return md_to_html(self.get_markdown(),
                          sanitize=True,
                          macros=macros,
                          macro_delimiter=macro_delimiter,
                          auto_macros=auto_macros,
                          auto_number_headings=auto_number_headings)

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
        elif attr_val is None:
            self.__data['attrs'].pop(attr_name, None)
        else:
            self.__data['attrs'][attr_name] = attr_val

        if attr_name == 'taskId':
            self.__is_plugin = bool(attr_val)
        elif attr_name == 'rp' or attr_name == 'ra':
            self.__is_ref = self.is_par_reference() or self.is_area_reference()

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
        return self.get_attr('rp') is not None

    def is_area_reference(self):
        return self.get_attr('ra') is not None

    def __repr__(self):
        return self.__data.__repr__()

    @classmethod
    def __rrepl(cls, s, old, new):
        rindex = s.rfind(old)
        return s[:rindex] + new + s[rindex + len(old):] if rindex >= 0 else s

    def get_referenced_pars(self, edit_window=False, set_html=True, source_doc=None, tr_get_one=True):
        def reference_par(ref_par, write_link=False):
            tr = self.get_attr('r') == 'tr'
            doc = ref_par.doc
            md = self.get_markdown() if tr else ref_par.get_markdown()
            attrs = self.get_attrs() if tr else ref_par.get_attrs()
            props = self.get_properties() if tr else ref_par.get_properties()

            par = DocParagraph.create(doc, par_id=ref_par.get_id(), md=md, t=ref_par.get_hash(),
                                           attrs=attrs, props=props)
            par.set_original(self)

            if set_html:
                html = self.get_html() if tr else ref_par.get_html()
                if write_link:
                    srclink = """&nbsp;
                                    <a class="parlink"
                                       href="/view/{0}#{1}"
                                       data-docid="{0}" data-parid="{1}"
                                       <sup>[*]</sup></a>
                                 </p>
                              """.format(ref_par.get_doc_id(), ref_par.get_id())
                    html = self.__rrepl(html, '</p>', srclink)
                par.__set_html(html)
            return par

        attrs = self.get_attrs()
        is_default_rd = False
        if 'rd' in attrs:
            try:
                ref_docid = int(attrs['rd'])
            except ValueError as e:
                raise TimDbException('Invalid reference document id: "{}"'.format(attrs['rd']))
        else:
            if source_doc is not None:
                default_rd = source_doc.doc_id
            else:
                settings = self.doc.get_settings()
                default_rd = settings.get_source_document()
            is_default_rd = True
            if default_rd is None:
                raise TimDbException('Source document for reference not specified.')
            ref_docid = default_rd

        if source_doc is None:
            from documentmodel.document import Document  # Document import needs to be here to avoid circular import
            ref_doc = Document(ref_docid)
        else:
            ref_doc = source_doc

        if not ref_doc.exists():
            raise TimDbException('The referenced document does not exist.')

        rl_attr = attrs.get('rl', 'all')
        write_link = (rl_attr == 'force') or not (is_default_rd or (rl_attr == 'no'))

        if self.is_par_reference():
            if self.get_doc_id() == int(ref_docid) and self.get_id() == attrs['rp']:
                raise TimDbException('Paragraph is referencing itself!')
            if not ref_doc.has_paragraph(attrs['rp']):
                raise TimDbException('The referenced paragraph does not exist.')

            ref_par = DocParagraph.get_latest(ref_doc, attrs['rp'], ref_doc.files_root)
            return [reference_par(ref_par, write_link=write_link)]

        elif self.is_area_reference():
            ref_pars = ref_doc.get_named_section(attrs['ra'])
            if tr_get_one and attrs.get('r', None) == 'tr' and len(ref_pars) > 0:
                return [reference_par(ref_pars[0], write_link=write_link)]
            else:
                return [reference_par(ref_par, write_link=write_link) for ref_par in ref_pars]
        else:
            assert False

    def set_original(self, orig):
        self.original = orig
        self._cache_props()
        self.__htmldata = None

    def get_original(self):
        return self.original

    def is_dynamic(self):
        return self.__is_plugin or self.__is_ref or self.__is_setting

    def is_plugin(self):
        return self.__is_plugin

    def is_setting(self):
        return self.__is_setting

    @classmethod
    def __get_macro_info(cls, doc):
        if doc is None:
            return None, None
        settings = doc.get_settings()
        if settings is None:
            return None, None
        return settings.get_macros(), settings.get_macro_delimiter()
