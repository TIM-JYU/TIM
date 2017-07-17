import json
import os
import shelve
from collections import defaultdict
from copy import copy

import filelock

from documentmodel.documentparser import DocumentParser
from documentmodel.documentparseroptions import DocumentParserOptions
from documentmodel.documentwriter import DocumentWriter
from documentmodel.macroinfo import MacroInfo
from documentmodel.randutils import random_id, hashfunc
from htmlSanitize import sanitize_html
from markdownconverter import par_list_to_html_list, expand_macros
from timdb.invalidreferenceexception import InvalidReferenceException
from timdb.timdbexception import TimDbException
from typing import Optional, Dict, List, Tuple, Any
from utils import count_chars, get_error_html
from utils import parse_yaml


class DocParagraph:
    """Represents a paragraph that is associated with a document.

    A paragraph has the following basic properties:

    * markdown content (md)
    * identifier (id)
    * attributes (attrs)
    * hash (t)

    Markdown content
    ================
    Markdown content is currently Pandoc flavour with some customizations. For Pandoc documentation, see
    http://pandoc.org/MANUAL.html for more information.

    Customizations
    --------------
    TODO

    Identifier
    ==========
    The identifier is a random alphanumeric string of length 12. The last character is a checksum of the first 11 ones
    to prevent accidental modification of the identifier.

    Attributes
    ==========
    A paragraph can have any user-defined attributes. Certain attributes have a reserved meaning.

    An attribute can be either a key-value pair ("x=y") or a class name (".name").

    The attribute "taskId=something" has a shorthand syntax "#something". It can be regarded as a user-defined
    identifier, so the name 'taskId' does not perfectly describe all of its use cases.

    The special attributes are the following:

    * rd : The paragraph is a reference to another paragraph or section, usually in a different document. The HTML
      content for the paragraph is retrieved from the referenced paragraph (unless this is a non-empty translated
      paragraph; see the 'tr' attribute). When specified, this attribute must be accompanied with 'ra' or 'rp'
      attribute, but not both. Value = id of the referenced document.
    * ra : The paragraph is a reference to a named section. Value = the name of the area in the referenced document.
    * rp : The paragraph is a reference to a paragraph. Value = the id of the paragraph.
      in the referenced document.
    * rt : The hash of the referenced paragraph. This is valid only when rd and rp are defined.
    * rl : If defined, either 'force' or 'no'. If 'force', a link to the source paragraph is rendered in the
      document view.
    * r  : If defined, either 'c' or 'tr'. The value 'tr' denotes this is a translated paragraph, and the rp and rd
      attributes identify the source paragraph.
    * settings : The paragraph contains the settings of a document. Currently this must be the first paragraph of the
      document. The value of this attribute is not used.
    * plugin : The paragraph contains a plugin. Value = the type of the plugin.
    * .nonumber : Any headings contained in the paragraph should not be autonumbered.
    * question : The paragraph is a question. The markdown content is a YAML block containing the question data.
      Value = ???

    Hash
    ====
    The hash of the paragraph is based on the markdown content and the attribute values.

    """

    default_files_root = 'tim_files'

    def __init__(self, doc, files_root: Optional[str] = None):
        """Constructs a DocParagraph.

        :param doc: The Document object to which this paragraph is connected.
        :param files_root: The location of the data store for this paragraph, or None to use the default data store.

        """
        self.doc = doc
        self.original = None
        self.files_root = self.get_default_files_root() if files_root is None else files_root
        self.html_sanitized = False
        self.html = None
        self.__htmldata = None
        self.ref_pars = None

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.is_identical_to(other)
        return NotImplemented

    def __ne__(self, other):
        if isinstance(other, self.__class__):
            return not (self == other)
        return NotImplemented

    def __hash__(self):
        return hash(tuple(sorted(self.__dict__.items())))

    @staticmethod
    def help_par():
        """Returns a dummy paragraph with id 'HELP_PAR' that is used as a placeholder for an empty document."""
        return DocParagraph.create(doc=None, par_id='HELP_PAR')

    @classmethod
    def create(cls,
               doc,
               par_id: Optional[str] = None,
               md: str = '',
               par_hash: Optional[str] = None,
               html: Optional[str] = None,
               attrs: Optional[Dict] = None,
               files_root: Optional[str] = None) -> 'DocParagraph':
        """Creates a DocParagraph from the given parameters.

        :param doc: The Document object to which this paragraph is connected.
        :param par_id: The paragraph id or None if it should be autogenerated.
        :param md: The markdown content.
        :param par_hash: The hash for the paragraph or None if it should be computed.
        :param html: The HTML for the paragraph or None if it should be generated based on markdown.
        :param attrs: The attributes for the paragraph.
        :param files_root: The location of the data store for this paragraph, or None to use the default data store.
        :return: The created DocParagraph.

        """
        par = DocParagraph(doc, files_root)
        par.html = html
        par.__data = {
            'id': random_id() if par_id is None else par_id,
            'md': md,
            't': hashfunc(md, attrs) if par_hash is None else par_hash,
            'attrs': {} if attrs is None else attrs
        }
        par._cache_props()
        return par

    def create_reference(self, doc, r: Optional[str] = None, add_rd: bool = True) -> 'DocParagraph':
        """Creates a reference paragraph to this paragraph.

        :param doc: The Document object in which the reference paragraph will reside.
        :param r: The kind of the reference.
        :param add_rd: If True, sets the rd attribute for the reference paragraph.
        :return: The created DocParagraph.

        """
        if 'r' == 'tr':
            par = DocParagraph.create(doc, files_root=self.files_root, md=self.get_markdown(),
                                      attrs=self.get_attrs())
        else:
            par = DocParagraph.create(doc, files_root=self.files_root)

        par.set_attr('r', r)
        par.set_attr('rd', self.get_doc_id() if add_rd else None)
        par.set_attr('rp', self.get_id())
        par.set_attr('ra', None)

        par._cache_props()
        return par

    @classmethod
    def create_area_reference(cls, doc, area_name: str, r: Optional[str] = None, add_rd: Optional[bool] = True,
                              files_root: Optional[str] = None) -> 'DocParagraph':
        """Creates an area reference paragraph.

        :param area_name: The name of the area.
        :param files_root: The location of the data store for the paragraph, or None to use the default data store.
        :param doc: The Document object in which the reference paragraph will reside.
        :param r: The kind of the reference.
        :param add_rd: If True, sets the rd attribute for the reference paragraph.
        :return: The created DocParagraph.

        """
        par = DocParagraph.create(doc, files_root=files_root)
        par.set_attr('r', r)
        par.set_attr('rd', doc.doc_id if add_rd else None)
        par.set_attr('ra', area_name)
        par.set_attr('rp', None)

        par._cache_props()
        return par

    @classmethod
    def from_dict(cls, doc, d: Dict, files_root: Optional[str] = None) -> 'DocParagraph':
        """Creates a paragraph from a dictionary.

        :param doc: The Document object in which the paragraph will reside.
        :param d: The dictionary.
        :param files_root: The location of the data store for the paragraph, or None to use the default data store.
        :return: The created DocParagraph.

        """
        par = DocParagraph(doc, files_root)
        par.__data = dict(d)
        par._cache_props()
        return par

    @classmethod
    def get_latest(cls, doc, par_id: str, files_root: Optional[str] = None) -> 'DocParagraph':
        """Retrieves the latest paragraph version from the data store.

        :param doc: The Document object for which to retrieve the paragraph.
        :param par_id: The paragraph id.
        :param files_root: The location of the data store for the paragraph, or None to use the default data store.
        :return: The retrieved DocParagraph.

        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        try:
            t = os.readlink(cls._get_path(doc, par_id, 'current', froot))
            return cls.get(doc, par_id, t, files_root=froot)
        except FileNotFoundError:
            raise TimDbException('Document {}: Paragraph not found: {}'.format(doc.doc_id, par_id))

    @classmethod
    def get(cls, doc, par_id: str, t: str, files_root: Optional[str] = None) -> 'DocParagraph':
        """Retrieves a specific paragraph version from the data store.

        :param doc: The Document object for which to retrieve the paragraph.
        :param par_id: The paragraph id.
        :param t: The paragraph hash.
        :param files_root: The location of the data store for the paragraph, or None to use the default data store.
        :return: The retrieved DocParagraph.

        """
        try:
            with open(cls._get_path(doc, par_id, t, files_root), 'r') as f:
                return cls.from_dict(doc, json.loads(f.read()), files_root=files_root)
        except FileNotFoundError:
            raise TimDbException('Document {}: Paragraph not found: {}'.format(doc.doc_id, par_id))

    def __iter__(self):
        """Returns an iterator to the internal data dictionary."""
        return self.__data.__iter__()

    @classmethod
    def get_default_files_root(cls):
        """Returns the default data store location for paragraphs."""
        return cls.default_files_root

    @classmethod
    def _get_path(cls, doc, par_id: str, t: str, files_root: Optional[str] = None) -> str:
        """Returns the filesystem location for a specific paragraph version.

        :param doc: The Document object in which the paragraph resides.
        :param par_id: The paragraph id.
        :param t: The paragraph hash.
        :param files_root: The location of the data store for the paragraph, or None to use the default data store.
        :return: The filesystem location for the paragraph.

        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', str(doc.doc_id), par_id, t)

    @classmethod
    def _get_base_path(cls, doc, par_id: str, files_root: Optional[str] = None) -> str:
        """Returns the filesystem location for the versions of a given paragraph.

        :param doc: The Document object in which the paragraph resides.
        :param par_id: The paragraph id.
        :param files_root: The location of the data store for the paragraph, or None to use the default data store.
        :return: The filesystem location for the versions of the paragraph.

        """
        froot = cls.get_default_files_root() if files_root is None else files_root
        return os.path.join(froot, 'pars', str(doc.doc_id), par_id)

    def dict(self) -> Dict:
        """Returns the internal data dictionary."""
        return self.__data

    def _mkhtmldata(self, from_preview: bool = True, output_md: bool = False):
        """Prepares the internal __htmldata dictionary that contains all the information required for embedding the
        paragraph in HTML."""
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

        if output_md:
            self.__htmldata['md'] = self.get_markdown()
        else:
            try:
                self.__htmldata['html'] = self.get_html(from_preview=from_preview)

            except Exception as e:
                self.__htmldata['html'] = get_error_html(e)

        self.__htmldata['cls'] = ' '.join(['par']
                                          + self.get_classes()
                                          + (['questionPar'] if self.is_question() else [])
                                          + ([self.get_attr('plugin')] if self.is_plugin() and not self.is_question() else [])
                                          )
        self.__htmldata['is_plugin'] = self.is_plugin()
        self.__htmldata['is_question'] = self.is_question()
        # self.is_plugin() and containerLink.get_plugin_needs_browser(self.get_attr('plugin'))
        self.__htmldata['needs_browser'] = self.is_plugin() and not self.is_question()

    def _cache_props(self):
        """Caches some boolean properties about this paragraph in internal attributes."""

        self.__is_ref = self.is_par_reference() or self.is_area_reference()
        self.__is_setting = 'settings' in self.get_attrs()

    def html_dict(self, use_md: bool = False) -> Dict:
        """Returns a dictionary that contains all the information required for embedding the paragraph in HTML."""
        self._mkhtmldata(output_md=use_md)
        return self.__htmldata

    def get_doc_id(self) -> int:
        """Returns the Document id to which this paragraph is attached."""
        return self.doc.doc_id

    def get_id(self) -> str:
        """Returns the id of this paragraph."""
        return self.__data['id']

    def get_rd(self) -> Optional[int]:
        """Returns the id of the Document to which this paragraph refers, or None if this is not a reference
        paragraph."""
        if 'rd' in self.__data['attrs']:
            try:
                return int(self.get_attr('rd'))
            except ValueError:
                return None

        default_rd = self.doc.get_settings().get_source_document()
        if default_rd is not None:
            return default_rd

        return None

    def is_identical_to(self, par: 'DocParagraph'):
        return self.is_same_as(par) and self.get_id() == par.get_id()

    def is_different_from(self, par: 'DocParagraph') -> bool:
        """Determines whether the given paragraph is different from this paragraph content-wise."""
        return not self.is_same_as(par)

    def is_same_as(self, par: 'DocParagraph') -> bool:
        """Determines whether the given paragraph is same as this paragraph content-wise."""
        return self.get_hash() == par.get_hash() and self.get_attrs() == par.get_attrs()

    def is_same_as_html(self, par: 'DocParagraph'):
        return self.is_same_as(par) and self.get_html(from_preview=True) == par.get_html(from_preview=True)

    def get_hash(self) -> str:
        """Returns the hash of this paragraph."""
        return self.__data['t']

    def get_markdown(self) -> str:
        """Returns the markdown of this paragraph."""
        return self.__data['md']

    def get_expanded_markdown(self, macroinfo: Optional[MacroInfo]=None, ignore_errors: bool = False) -> str:
        """Returns the macro-processed markdown for this paragraph.

        :param macroinfo: The MacroInfo to use. If None, the MacroInfo is taken from the document that has the
        paragraph.
        :param ignoreErrors: Whether or not to ignore errors when expanding the macros
        :return: The expanded markdown.

        """
        if macroinfo is None:
            macroinfo = self.doc.get_settings().get_macroinfo()
        return expand_macros(self.get_markdown(), macroinfo.get_macros(),
                             macroinfo.get_macro_delimiter(), ignore_errors=ignore_errors)

    def get_title(self) -> Optional[str]:
        """Attempts heuristically to return a title for this paragraph.

        :return: The title for this paragraph or None if there is no sensible title.

        """
        md = self.__data['md']
        if len(md) < 3 or md[0] != '#' or md[1] == '-':
            return None

        attr_index = md.find('{')
        return md[2:attr_index].strip() if attr_index > 0 else md[2:].strip()

    def get_exported_markdown(self) -> str:
        """Returns the markdown in exported form for this paragraph."""
        if self.is_par_reference() and self.is_translation():
            # This gives a default translation based on the source paragraph
            # todo: same for area reference
            data = []
            for par in self.get_referenced_pars():
                d = self.__data.copy()  # todo: needs copy or not?
                md = par.get_markdown()
                if md:
                    d['md'] = md
                data.append(d)
            return DocumentWriter(data, export_hashes=False, export_ids=False).get_text()

        return DocumentWriter([self.__data],
                              export_hashes=False,
                              export_ids=False).get_text(DocumentParserOptions.single_paragraph())

    def __get_setting_html(self) -> str:
        """Returns the HTML for the settings paragraph."""
        from documentmodel.docsettings import DocSettings

        if DocSettings.is_valid_paragraph(self):
            return '<p class="docsettings">&nbsp;</p>'
        else:
            return '<div class="pluginError">Invalid settings paragraph detected</div>'

    def get_html(self, from_preview: bool = True) -> str:
        """Returns the html for the paragraph.

        :param from_preview: Whether this is called from a preview window or not.
                             If True, previous paragraphs are preloaded too and the result is not cached.
                             Safer, but slower. Set explicitly False if you know what you're doing.
        :return: html string

        """
        if self.is_question():
            from plugin import Plugin
            from plugin import PluginException
            try:
                values = Plugin.from_paragraph(self).values
            except PluginException as e:
                if not self.get_attr('plugin'):
                    return get_error_html('This question is missing plugin="qst" attribute. Please add it.')
                return get_error_html(e)
            title = values.get("json", {}).get("questionTitle", "")
            if not title:  # compability for old
                title = values.get("json", {}).get("title", "question_title")
            return self.__set_html(sanitize_html(
                '<a class="questionAddedNew"><span class="glyphicon glyphicon-question-sign" title="{0}"></span></a>'
                '<p class="questionNumber">{0}</p>'.format(title)))
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
    def preload_htmls(cls, pars: List['DocParagraph'], settings,
                      clear_cache: bool = False, context_par: Optional['DocParagraph'] = None, persist: Optional[bool] = True):
        """Loads the HTML for each paragraph in the given list.

        :param context_par: The context paragraph. Required only for previewing for now.
        :param persist: Whether the result of preloading should be saved to disk.
        :param clear_cache: Whether all caches should be refreshed.
        :param settings: The document settings.
        :param pars: Paragraphs to preload.
        :return: A list of paragraphs whose HTML changed as the result of preloading.

        """
        if not pars:
            return []

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
            with shelve.open(macro_cache_file) as c, \
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
            with filelock.FileLock("/tmp/cache_lock_{}".format(doc_id_str)):
                if clear_cache:
                    try:
                        os.remove(macro_cache_file + '.db')
                    except FileNotFoundError:
                        pass
                    try:
                        os.remove(heading_cache_file + '.db')
                    except FileNotFoundError:
                        pass
                with shelve.open(macro_cache_file) as cache, \
                        shelve.open(heading_cache_file) as heading_cache:
                    unloaded_pars = cls.get_unloaded_pars(pars, settings, cache, heading_cache, clear_cache)
                    for k, v in heading_cache.items():
                        heading_cache[k] = v

        changed_pars = []
        if len(unloaded_pars) > 0:
            htmls = par_list_to_html_list([par for par, _, _, _, _ in unloaded_pars],
                                          auto_macros=({'h': auto_macros['h'], 'headings': hs}
                                                       for _, _, auto_macros, hs, _ in unloaded_pars),
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
        """Finds out which of the given paragraphs need to be preloaded again.

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
        macroinfo = settings.get_macroinfo()
        macros = macroinfo.get_macros() if settings else None
        macro_delim = macroinfo.get_macro_delimiter() if settings else None
        m = str(macros) + macro_delim + str(settings.auto_number_headings()) + str(settings.heading_format())
        for par in pars:
            if par.is_dynamic():
                dyn += 1
                continue
            if not clear_cache and par.html is not None:
                continue
            cached = par.__data.get('h')
            try:
                auto_macros = par.get_auto_macro_values(macros, macro_delim, auto_macro_cache, heading_cache)
            except RecursionError:
                raise TimDbException(
                    'Infinite recursion detected in get_auto_macro_values; the document may be broken.')
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
        """Returns whether this paragraph has the specified class."""
        return class_name in self.__data.get('attrs', {}).get('classes', {})

    def add_class(self, class_name):
        """Adds the specified class to this paragraph."""
        if not self.has_class(class_name):
            if 'attrs' not in self.__data:
                self.__data['attrs'] = {}
            if 'classes' not in self.__data['attrs']:
                self.__data['attrs']['classes'] = []
            self.__data['attrs']['classes'].append(class_name)

    def get_auto_macro_values(self, macros, macro_delim, auto_macro_cache, heading_cache):
        """Returns the auto macros values for the current paragraph. Auto macros include things like current
        heading/table/figure numbers.

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
        """Sanitizes the HTML for this paragraph.

        If the HTML has already been sanitized or the HTML has not been loaded, this method does nothing.

        """
        if self.html_sanitized or not self.html:
            return
        new_html = sanitize_html(self.html)
        self.__set_html(new_html, True)

    def __set_html(self, new_html: str, sanitized: bool = False) -> str:
        """Sets the HTML for this paragraph.

        :param new_html: The new HTML.
        :param sanitized: Whether the HTML is sanitized. Default is False.
        :return: The HTML.

        """
        self.html = new_html
        if self.__htmldata is not None:
            self.__htmldata['html'] = new_html
        self.html_sanitized = sanitized
        return self.html

    def get_attr(self, attr_name: str, default_value=None):
        """Returns the value of the specified attribute.

        :param attr_name: The name of the attribute to get.
        :param default_value: The default value to return if the attribute does not exist.
        :return: The attribute value.

        """
        return self.__data['attrs'].get(attr_name, default_value)

    def set_markdown(self, new_md: str):
        """Sets markdown for this paragraph.

        :param new_md: The new markdown.

        """
        self.__data['md'] = new_md
        self.__data['t'] = hashfunc(new_md, self.get_attrs())

    def set_attr(self, attr_name: str, attr_val: Any):
        """Sets the value of the specified attribute.

        :param attr_name: The name of the attribute to set.
        :param attr_val: The value for the attribute.

        """
        if attr_val is None:
            self.__data['attrs'].pop(attr_name, None)
        else:
            self.__data['attrs'][attr_name] = attr_val

        self._cache_props()

    def is_task(self):
        """Returns whether the paragraph is a task."""
        return self.get_attr('taskId') is not None and self.get_attr('plugin') is not None

    @classmethod
    def __combine_dict(cls, base_dict: Optional[Dict], over_dict: Dict) -> Dict:
        """Merges two Dicts together."""
        if base_dict is None:
            return over_dict
        new_dict = dict(base_dict)
        for key in over_dict:
            new_dict[key] = over_dict[key]
        return new_dict

    def get_attrs(self, base_attrs: Optional[Dict] = None) -> Dict:
        return DocParagraph.__combine_dict(base_attrs, self.__data['attrs'])

    def get_attrs_str(self) -> str:
        """Returns the attributes as a JSON string."""
        return json.dumps(self.__data['attrs'], sort_keys=True)

    def get_classes(self) -> List[str]:
        return self.get_attr('classes', [])

    def get_class_str(self) -> str:
        """Returns the classes as a space-separated string."""
        return ' '.join(self.get_classes())

    def get_base_path(self) -> str:
        """Returns the filesystem path for the versions of this paragraph."""
        return self._get_base_path(self.doc, self.get_id(), files_root=self.files_root)

    def get_path(self) -> str:
        """Returns the filesystem path for this paragraph."""
        return self._get_path(self.doc, self.__data['id'], self.__data['t'], files_root=self.files_root)

    def __read(self) -> bool:
        if not os.path.isfile(self.get_path()):
            return False
        with open(self.get_path(), 'r') as f:
            self.__data = json.loads(f.read())
            self._cache_props()
            self.__htmldata = None
            return True

    def __write(self):
        file_name = self.get_path()
        does_exist = os.path.isfile(file_name)

        if not does_exist:
            base_path = self.get_base_path()
            if not os.path.exists(base_path):
                os.makedirs(base_path)

        with open(file_name, 'w') as f:
            f.write(json.dumps(self.__data))

    def set_latest(self):
        """Updates the 'current' symlink to point to this paragraph version."""
        linkpath = self._get_path(self.doc, self.get_id(), 'current', files_root=self.files_root)
        if linkpath == self.get_hash():
            return
        if os.path.islink(linkpath) or os.path.isfile(linkpath):
            os.unlink(linkpath)
        os.symlink(self.get_hash(), linkpath)

    def clone(self) -> 'DocParagraph':
        """Clones the paragraph.

        A new ID is generated for the cloned paragraph.

        :return: The cloned paragraph.

        """
        return DocParagraph.create(self.doc,
                                   md=self.get_markdown(),
                                   attrs=self.get_attrs(),
                                   files_root=self.files_root)

    def save(self, add=False):
        """Performs a save operation for this paragraph.

        This updates the document version and paragraph list appropriately.

        :param add: Whether to add (True) or modify an existing (False).

        """
        # TODO: Possibly get rid of 'add' parameter altogether.
        if add:
            self.doc.add_paragraph_obj(self)
        else:
            self.doc.modify_paragraph_obj(self.get_id(), self)

    def store(self):
        """Stores the paragraph to disk."""
        self.__write()

        # Clear cached referenced paragraphs because this was modified
        self.ref_pars = None

    def is_reference(self) -> bool:
        """Returns whether this paragraph is a reference to some other paragraph."""
        return self.__is_ref

    def is_par_reference(self) -> bool:
        """Returns whether this paragraph is a reference to a single paragraph."""
        return self.get_attr('rp') is not None

    def is_area_reference(self) -> bool:
        """Returns whether this paragraph is a reference to an area."""
        return self.get_attr('ra') is not None

    def is_translation(self) -> bool:
        """Returns whether this paragraph is a translated paragraph."""
        return self.get_attr('r') == 'tr'

    def __repr__(self):
        return self.__data.__repr__()

    def get_referenced_pars(self, set_html: bool = True, source_doc: bool = None,
                            tr_get_one: bool = True, cycle: Optional[List[Tuple[int, str]]] = None) -> List[
            'DocParagraph']:
        """Returns the paragraphs that are referenced by this paragraph.

        The references are resolved recursively, i.e. if the referenced paragraphs are references themselves, they
        will also be resolved, and so on, until we get a list of non-reference paragraphs.

        :param set_html: Whether to automatically set HTML for the resolved paragraphs.
        :param source_doc: The assumed source document in case the rd attribute of a paragraph is absent.
        :param tr_get_one: If True and this paragraph is a translation and the result contains more than one paragraph,
          only the first one of them will be returned.
        :param cycle: A list of already visited paragraphs to prevent infinite recursion.
        :return: The list of resolved paragraphs.

        """
        if self.ref_pars is not None:
            return self.ref_pars
        if cycle is None:
            cycle = []
        par_doc_id = self.get_doc_id(), self.get_id()
        if par_doc_id in cycle:
            cycle.append(par_doc_id)
            raise InvalidReferenceException(
                'Infinite referencing loop detected: ' + ' -> '.join(('{}:{}'.format(d, p) for d, p in cycle)))
        cycle.append(par_doc_id)

        def create_final_par(ref_par) -> 'DocParagraph':
            if self.is_translation() and self.get_markdown():
                md = self.get_markdown()
            else:
                md = ref_par.get_markdown()

            # TODO: decide whether this attr combination makes sense
            new_attrs = self.get_attrs(ref_par.get_attrs())

            # Remove reference attributes
            for ref_attr in ['r', 'rd', 'rp', 'ra', 'rt']:
                new_attrs.pop(ref_attr, None)

            final_par = DocParagraph.create(ref_par.doc, par_id=ref_par.get_id(), md=md, par_hash=ref_par.get_hash(),
                                            attrs=new_attrs)
            final_par.original = self
            final_par._cache_props()
            final_par.__htmldata = None

            if set_html:
                html = self.get_html(from_preview=False) if self.is_translation(
                ) else ref_par.get_html(from_preview=False)

                # if html is empty, use the source
                if html == '':
                    html = ref_par.get_html(from_preview=False)
                final_par.__set_html(html)
            return final_par

        ref_docid = None
        ref_doc = None

        attrs = self.get_attrs()
        if 'rd' in attrs:
            try:
                ref_docid = int(attrs['rd'])
            except ValueError:
                raise InvalidReferenceException('Invalid reference document id: "{}"'.format(attrs['rd']))
        elif source_doc is not None:
            ref_doc = source_doc
        else:
            settings = self.doc.get_settings()
            ref_docid = settings.get_source_document()

        if ref_doc is None:
            if ref_docid is None:
                raise InvalidReferenceException('Source document for reference not specified.')
            from documentmodel.document import Document  # Document import needs to be here to avoid circular import
            ref_doc = Document(ref_docid)

        if not ref_doc.exists():
            raise InvalidReferenceException('The referenced document does not exist.')

        if self.is_par_reference():
            try:
                par = DocParagraph.get_latest(ref_doc, attrs['rp'], ref_doc.files_root)
                if not ref_doc.has_paragraph(attrs['rp']):
                    # par.set_attr('deleted', 'True')
                    par.add_class('deleted')

            except TimDbException:
                raise InvalidReferenceException('The referenced paragraph does not exist.')

            if par.is_reference():
                ref_pars = par.get_referenced_pars(set_html=set_html,
                                                   source_doc=source_doc,
                                                   cycle=cycle,
                                                   tr_get_one=tr_get_one)
            else:
                ref_pars = [par]
        elif self.is_area_reference():
            section_pars = ref_doc.get_named_section(attrs['ra'])
            ref_pars = []
            for p in section_pars:
                if p.is_reference():
                    ref_pars.extend(p.get_referenced_pars(set_html=set_html,
                                                          source_doc=source_doc,
                                                          cycle=cycle,
                                                          tr_get_one=tr_get_one))
                else:
                    ref_pars.append(p)
            if tr_get_one and self.is_translation() and len(ref_pars) > 0:
                self.ref_pars = [create_final_par(ref_pars[0])]
                return self.ref_pars
        else:
            assert False
        self.ref_pars = [create_final_par(ref_par) for ref_par in ref_pars]
        return self.ref_pars

    def is_dynamic(self) -> bool:
        """Returns whether this paragraph is a dynamic paragraph.

        A dynamic paragraph is a paragraph which is either

        * a plugin,
        * a reference which is not a translation, or
        * a setting.

        """
        return self.is_plugin() \
            or (self.__is_ref and not self.is_translation()) \
            or self.__is_setting

    def is_plugin(self) -> bool:
        """Returns whether this paragraph is a plugin."""

        return bool(self.get_attr('plugin'))

    def is_question(self) -> bool:
        """Returns whether this paragraph is a question paragraph."""
        # preview = self.get("preview", False)
        return bool(self.get_attr('question'))

    def is_setting(self) -> bool:
        """Returns whether this paragraph is a settings paragraph."""
        return self.__is_setting

    def set_id(self, par_id: str):
        """Sets the id for this paragraph.

        :param par_id: The new id for the paragraph.

        """
        self.__data['id'] = par_id


def is_real_id(par_id: Optional[str]):
    """Returns whether the given paragraph id corresponds to some real paragraph
    instead of being None or a placeholder value ('HELP_PAR').
    
    :param par_id: The paragraph id.
    :return: True if the given paragraph id corresponds to some real paragraph, False otherwise.
    """
    return par_id is not None and par_id != 'HELP_PAR'
