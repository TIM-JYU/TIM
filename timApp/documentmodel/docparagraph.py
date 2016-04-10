import os
import shelve
from collections import defaultdict
from copy import copy

from contracts import contract, new_contract

from documentmodel.documentparser import DocumentParser
from documentmodel.documentparseroptions import DocumentParserOptions
from documentmodel.documentwriter import DocumentWriter
from htmlSanitize import sanitize_html
from markdownconverter import md_to_html, par_list_to_html_list, expand_macros
from timdb.timdbbase import TimDbException
from utils import count_chars, get_error_html
from .randutils import *


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
        self.ref_pars = None

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
        except FileNotFoundError:
            raise TimDbException('Document {}: Paragraph not found: {}'.format(doc.doc_id, par_id))

    @classmethod
    @contract
    def get(cls, doc, par_id: 'str', t: 'str', files_root: 'str|None' = None) -> 'DocParagraph':
        try:
            """Loads a paragraph from file system based on given parameters.
            """
            with open(cls._get_path(doc, par_id, t, files_root), 'r') as f:
                return cls.from_dict(doc, json.loads(f.read()), files_root=files_root)
        except FileNotFoundError:
            raise TimDbException('Document {}: Paragraph not found: {}'.format(doc.doc_id, par_id))

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

    def _mkhtmldata(self, from_preview: 'bool' = True):
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
            self.__htmldata['html'] = self.get_html(from_preview=from_preview)
        except Exception as e:
            self.__htmldata['html'] = get_error_html(e)

        self.__htmldata['cls'] = 'par ' + self.get_class_str()
        self.__htmldata['is_plugin'] = self.is_plugin()
        self.__htmldata['is_question'] = self.is_question()
        self.__htmldata['needs_browser'] = True #self.is_plugin() and containerLink.get_plugin_needs_browser(self.get_attr('plugin'))

    def _cache_props(self):
        self.__is_plugin = self.get_attr('plugin') or ""  # self.get_attr('taskId')
        self.__is_question = self.get_attr('question') or ""
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
        if self.is_par_reference() and self.is_translation():
            # This gives a default translation based on the source paragraph
            # todo: same for area reference
            data = [par.__data for par in self.get_referenced_pars(edit_window=True)]
            return DocumentWriter(data, export_hashes=False, export_ids=False).get_text()

        return DocumentWriter([self.__data],
                              export_hashes=False,
                              export_ids=False).get_text(DocumentParserOptions.single_paragraph())

    @contract
    def __get_setting_html(self) -> 'str':
        from documentmodel.docsettings import DocSettings

        if DocSettings.is_valid_paragraph(self):
            return '<p class="docsettings">&nbsp;</p>'
        else:
            return '<div class="pluginError">Invalid settings paragraph detected</div>'

    @contract
    def get_html(self, from_preview: 'bool' = True) -> 'str':
        """
        Gets the html for the paragraph.
        :param from_preview: Whether this is called from a preview window or not.
                             If True, previous paragraphs are preloaded too and the result is not cached.
                             Safer, but slower. Set explicitly False if you know what you're doing.
        :return: html string
        """
        if self.is_question():
            return self.__set_html('<img class="questionAddedNew" width="30" height="30" src=/static/images/show-question-icon.png/>')
        if self.html is not None:
            return self.html
        if self.is_plugin():
            return self.__set_html('')
        if self.is_setting():
            return self.__set_html(self.__get_setting_html())

        context_par = self.doc.get_previous_par(self, get_last_if_no_prev=False) if from_preview else None
        DocParagraph.preload_htmls([self],
                                   self.doc.get_settings(),
                                   context_par=context_par,
                                   persist=not from_preview)
        return self.html

    @classmethod
    @contract
    def preload_htmls(cls, pars: 'list(DocParagraph)', settings, clear_cache=False, context_par=None, persist=True):
        """
        Loads the HTML for each paragraph in the given list.
        :param context_par: The context paragraph. Required only for previewing for now.
        :param persist: Whether the result of preloading should be saved to disk.
        :param clear_cache: Whether all caches should be refreshed.
        :param settings: The document settings.
        :param pars: Paragraphs to preload.
        :return: A list of paragraphs whose HTML changed as the result of preloading.
        """
        if not pars:
            return

        doc_id_str = str(pars[0].doc.doc_id)
        macro_cache_file = '/tmp/tim_auto_macros_' + doc_id_str
        heading_cache_file = '/tmp/heading_cache_' + doc_id_str

        first_pars = []
        if context_par is not None:
            first_pars = pars[0].doc.get_pars_till(context_par)
            pars = first_pars + pars

        if not persist:
            cache = {}
            heading_cache = {}
            with shelve.open(macro_cache_file) as c,\
                 shelve.open(heading_cache_file) as hc:

                # Basically we want the cache objects to be non-persistent, so we convert them to normal dicts
                # Find out better way if possible...
                for par in first_pars:
                    key = str((par.get_id(), par.doc.get_version()))
                    value = c.get(key)
                    if value is not None:
                        cache[key] = value
                    value = hc.get(par.get_id())
                    if value is not None:
                        heading_cache[par.get_id()] = value
            unloaded_pars = cls.get_unloaded_pars(pars, settings, cache, heading_cache, clear_cache)
        else:
            if clear_cache:
                try:
                    os.remove(macro_cache_file + '.db')
                except FileNotFoundError:
                    pass
                try:
                    os.remove(heading_cache_file + '.db')
                except FileNotFoundError:
                    pass
            with shelve.open(macro_cache_file) as cache,\
                 shelve.open(heading_cache_file) as heading_cache:
                unloaded_pars = cls.get_unloaded_pars(pars, settings, cache, heading_cache, clear_cache)
                for k, v in heading_cache.items():
                    heading_cache[k] = v

        changed_pars = []
        if len(unloaded_pars) > 0:
            htmls = par_list_to_html_list([par for par, _, _, _, _ in unloaded_pars],
                                          auto_macros=({'h': auto_macros['h'], 'headings': hs} for _, _, auto_macros, hs, _ in unloaded_pars),
                                          settings=settings)
            for (par, auto_macro_hash, _, _, old_html), h in zip(unloaded_pars, htmls):
                # h is not sanitized but old_html is, but HTML stays unchanged after sanitization most of the time
                # so they are comparable
                if h != old_html:
                    h = sanitize_html(h)
                    changed_pars.append(par)
                par.__data['h'][auto_macro_hash] = h
                par.__set_html(h, sanitized=True)
                if persist:
                    par.__write()
        return changed_pars

    @classmethod
    def get_unloaded_pars(cls, pars, settings, auto_macro_cache, heading_cache, clear_cache=False):
        """
        Finds out which of the given paragraphs need to be preloaded again.

        :param pars: The list of paragraphs to be processed.
        :param settings: The settings for the document.
        :param auto_macro_cache: The cache object from which to retrieve and store the auto macro data.
        :param heading_cache: A cache object to store headings into. The key is paragraph id and value is a list of headings
         in that paragraph.
        :param clear_cache: Whether all caches should be refreshed.
        :return: A 5-tuple of the form:
          (paragraph, hash of the auto macro values, auto macros, so far used headings, old HTML).
        """
        cumulative_headings = []
        unloaded_pars = []
        dyn = 0
        l = 0
        macros = settings.get_macros() if settings else None
        macro_delim = settings.get_macro_delimiter() if settings else None
        m = str(macros) + macro_delim + str(settings.auto_number_headings()) + str(settings.heading_format())
        for par in pars:
            if par.is_dynamic():
                dyn += 1
                continue
            if not clear_cache and par.html is not None:
                continue
            cached = par.__data.get('h')
            auto_macros = par.get_auto_macro_values(macros, macro_delim, auto_macro_cache, heading_cache)
            auto_macro_hash = hashfunc(m + str(auto_macros))

            par_headings = heading_cache.get(par.get_id())
            if cumulative_headings:
                # Performance optimization: copy only if the set of headings changes
                if par_headings:
                    all_headings_so_far = cumulative_headings[-1].copy()
                else:
                    all_headings_so_far = cumulative_headings[-1]
            else:
                all_headings_so_far = defaultdict(int)
            cumulative_headings.append(all_headings_so_far)
            if par_headings is not None:
                for h in par_headings:
                    all_headings_so_far[h] += 1

            if not clear_cache and cached is not None:
                if type(cached) is str:  # Compatibility
                    old_html = cached
                else:
                    cached_html = cached.get(auto_macro_hash)
                    if cached_html is not None:
                        par.html = cached_html
                        l += 1
                        continue
                    else:
                        try:
                            old_html = next(iter(cached.values()))
                        except StopIteration:
                            old_html = None
            else:
                old_html = None

            tup = (par, auto_macro_hash, auto_macros, all_headings_so_far, old_html)
            par.__data['h'] = {}
            unloaded_pars.append(tup)
        return unloaded_pars

    def has_class(self, class_name):
        return class_name in self.__data.get('attrs', {}).get('classes', {})

    def get_auto_macro_values(self, macros, macro_delim, auto_macro_cache, heading_cache):
        """Gets the auto macros values for the current paragraph.
        Auto macros include things like current heading/table/figure numbers.

        :param heading_cache: A cache object to store headings into. The key is paragraph id and value is a list of headings
         in that paragraph.
        :param macros: Macros to apply for the paragraph.
        :param auto_macro_cache: The cache object from which to retrieve and store the auto macro data.
        :return: Auto macro values as a dict.
        :param macro_delim: Delimiter for macros.
        :return: A dict(str, dict(int,int)) containing the auto macro information.
        """

        key = str((self.get_id(), self.doc.get_version()))
        cached = auto_macro_cache.get(key)
        if cached is not None:
            return cached

        prev_par = self.doc.get_previous_par(self)
        if prev_par is None:
            prev_par_auto_values = {'h': {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 0}}
            heading_cache[self.get_id()] = []
        else:
            prev_par_auto_values = prev_par.get_auto_macro_values(macros, macro_delim, auto_macro_cache, heading_cache)

        if prev_par is None or prev_par.is_dynamic() or prev_par.has_class('nonumber'):
            auto_macro_cache[key] = prev_par_auto_values
            heading_cache[self.get_id()] = []
            return prev_par_auto_values

        md_expanded = expand_macros(prev_par.get_markdown(), macros, macro_delim)
        blocks = DocumentParser(md_expanded).get_blocks(DocumentParserOptions.break_on_empty_lines())
        deltas = copy(prev_par_auto_values['h'])
        titles = []
        for e in blocks:
            level = count_chars(e['md'], '#')
            if level > 0:
                title = e['md'][level:].strip()
                titles.append(title)
                deltas[level] += 1
                for i in range(level + 1, 7):
                    deltas[i] = 0
        heading_cache[self.get_id()] = titles
        result = {'h': deltas}
        auto_macro_cache[key] = result
        return result

    def sanitize_html(self):
        if self.html_sanitized or not self.html:
            return
        new_html = sanitize_html(self.html)
        self.__set_html(new_html, True)

    @contract
    def __set_html(self, new_html: 'str', sanitized=False) -> 'str':
        self.html = new_html
        if self.__htmldata is not None:
            self.__htmldata['html'] = new_html
        self.html_sanitized = sanitized
        return self.html

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
        if attr_name == 'question':
            self.__is_question = bool(attr_val)
        elif attr_name == 'rp' or attr_name == 'ra':
            self.__is_ref = self.is_par_reference() or self.is_area_reference()

    @classmethod
    @contract
    def __combine_md(cls, base_md: 'str|None', over_md: 'str') -> 'str':
        if base_md is None:
            return over_md

        # TODO: combine element by element
        return base_md if over_md == '' else over_md

    @classmethod
    @contract
    def __combine_dict(cls, base_dict: 'dict|None', over_dict: 'dict') -> 'dict':
        if base_dict is None:
            return over_dict
        new_dict = dict(base_dict)
        for key in over_dict:
            new_dict[key] = over_dict[key]
        return new_dict

    @contract
    def get_attrs(self, base_attrs: 'dict|None' = None) -> 'dict':
        return DocParagraph.__combine_dict(base_attrs, self.__data['attrs'])

    @contract
    def get_properties(self, base_props: 'dict|None' = None) -> 'dict':
        return DocParagraph.__combine_dict(base_props, self.__data.get('props', {}))

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
        return self._get_path(self.doc, self.__data['id'], self.__data['t'], files_root=self.files_root)

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

        # Clear cached referenced paragraphs because this was modified
        self.ref_pars = None

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

    def is_translation(self):
        return self.get_attr('r') == 'tr'

    def __repr__(self):
        return self.__data.__repr__()

    @classmethod
    def __rrepl(cls, s, old, new):
        rindex = s.rfind(old)
        return s[:rindex] + new + s[rindex + len(old):] if rindex >= 0 else s

    def get_referenced_pars(self, edit_window=False, set_html=True, source_doc=None, tr_get_one=True, cycle=None):
        if self.ref_pars is not None:
            return self.ref_pars
        if cycle is None:
            cycle = []
        par_doc_id = self.get_doc_id(), self.get_id()
        if par_doc_id in cycle:
            cycle.append(par_doc_id)
            raise TimDbException(
                'Infinite referencing loop detected: ' + ' -> '.join(('{}:{}'.format(d, p) for d, p in cycle)))
        cycle.append(par_doc_id)

        def reference_par(ref_par):
            tr = self.get_attr('r') == 'tr'
            doc = ref_par.doc

            if edit_window:
                md = DocParagraph.__combine_md(ref_par.get_markdown(), self.get_markdown()) if tr else self.get_markdown()
                attrs = self.get_attrs()
                props = self.get_properties()
            else:
                md = DocParagraph.__combine_md(ref_par.get_markdown(), self.get_markdown()) if tr else ref_par.get_markdown()
                attrs = self.get_attrs(ref_par.get_attrs()) #if tr else ref_par.get_attrs()
                props = self.get_properties(ref_par.get_properties()) #if tr else ref_par.get_properties()

                # Remove reference attributes
                for ref_attr in ['r', 'rd', 'rp', 'ra', 'rt']:
                    attrs.pop(ref_attr, None)

            par = DocParagraph.create(doc, par_id=ref_par.get_id(), md=md, t=ref_par.get_hash(),
                                           attrs=attrs, props=props)
            par.set_original(self)

            if set_html:
                html = self.get_html(from_preview=False) if tr else ref_par.get_html(from_preview=False)
                
                # if html is empty, use the source
                if html == '':
                    html = ref_par.get_html(from_preview=False)
                par.__set_html(html)
            return par

        ref_docid = None
        ref_doc = None

        attrs = self.get_attrs()
        if 'rd' in attrs:
            try:
                ref_docid = int(attrs['rd'])
            except ValueError as e:
                raise TimDbException('Invalid reference document id: "{}"'.format(attrs['rd']))
        elif source_doc is not None:
            ref_doc = source_doc
        else:
            settings = self.doc.get_settings()
            ref_docid = settings.get_source_document()

        if ref_doc is None:
            if ref_docid is None:
                raise TimDbException('Source document for reference not specified.')
            from documentmodel.document import Document  # Document import needs to be here to avoid circular import
            ref_doc = Document(ref_docid)

        if not ref_doc.exists():
            raise TimDbException('The referenced document does not exist.')

        if self.is_par_reference():
            if not ref_doc.has_paragraph(attrs['rp']):
                raise TimDbException('The referenced paragraph does not exist.')

            ref_par = DocParagraph.get_latest(ref_doc, attrs['rp'], ref_doc.files_root)
            if ref_par.is_reference():
                ref_pars = ref_par.get_referenced_pars(edit_window=edit_window,
                                                       set_html=set_html,
                                                       cycle=cycle)
            else:
                ref_pars = [ref_par]
        elif self.is_area_reference():
            section_pars = ref_doc.get_named_section(attrs['ra'])
            ref_pars = []
            for p in section_pars:
                if p.is_reference():
                    ref_pars.extend(p.get_referenced_pars(edit_window=edit_window,
                                                          set_html=set_html,
                                                          source_doc=source_doc,
                                                          cycle=cycle))
                else:
                    ref_pars.append(p)
            if tr_get_one and attrs.get('r', None) == 'tr' and len(ref_pars) > 0:
                self.ref_pars = [reference_par(ref_pars[0])]
                return self.ref_pars
        else:
            assert False
        self.ref_pars = [reference_par(ref_par) for ref_par in ref_pars]
        return self.ref_pars

    def set_original(self, orig):
        self.original = orig
        self._cache_props()
        self.__htmldata = None

    def get_original(self):
        return self.original

    def is_dynamic(self):
        return self.__is_plugin \
               or (self.__is_ref and self.__data.get('attrs', {}).get('r', '') != 'tr')\
               or self.__is_setting

    def is_plugin(self):
        return self.__is_plugin

    def is_question(self):
        is_question = self.__is_question
        return is_question

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

    @contract
    def set_id(self, par_id: 'str'):
        self.__data['id'] = par_id
