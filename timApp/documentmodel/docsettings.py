from datetime import timedelta
from typing import Optional, List, Dict, Tuple, Iterable

import yaml

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.macroinfo import MacroInfo
from timApp.documentmodel.specialnames import DEFAULT_PREAMBLE_DOC
from timApp.documentmodel.yamlblock import YamlBlock
from timApp.timdb.exceptions import TimDbException, InvalidReferenceException


class DocSettings:
    global_plugin_attrs_key = 'global_plugin_attrs'
    css_key = 'css'
    macros_key = 'macros'
    globalmacros_key = 'globalmacros'
    doctexmacros_key = 'doctexmacros'
    macro_delimiter_key = 'macro_delimiter'
    source_document_key = 'source_document'
    auto_number_headings_key = 'auto_number_headings'
    auto_number_start_key = 'auto_number_start'
    heading_format_key = 'heading_format'
    show_task_summary_key = 'show_task_summary'
    no_question_auto_numbering_key = 'no_question_auto_numbering'
    slide_background_url_key = 'slide_background_url'
    slide_background_color_key = 'slide_background_color'
    bookmark_key = 'bookmarks'
    lazy_key = 'lazy'
    hide_links_key = 'hide_links'
    point_sum_rule_key = 'point_sum_rule'
    max_points_key = 'max_points'
    nomacros_key = 'nomacros'
    texplain_key = 'texplain'
    live_updates_key = 'live_updates'
    plugin_md_key = 'plugin_md'
    print_settings_key = 'print_settings'
    preamble_key = 'preamble'
    show_authors_key = 'show_authors'
    read_expiry_key = 'read_expiry'
    add_par_button_text_key = 'add_par_button_text'

    @staticmethod
    def settings_to_string(par: DocParagraph) -> str:
        if not par.is_setting():
            raise TimDbException(f'Not a setting paragraph: {par.get_id()}')
        if par.is_reference() and not par.is_translation():
            par = par.get_referenced_pars(set_html=False)[0]
        if not par.is_setting():
            raise TimDbException(f'The referenced paragraph is not a setting paragraph.')
        yb = DocSettings.parse_values(par)
        return str(yb.values)

    @classmethod
    def from_paragraph(cls, par: DocParagraph):
        """Constructs DocSettings from the given DocParagraph.

        :param par: The DocParagraph to extract settings from.
        :return: The DocSettings object.

        """
        if not par.is_setting():
            raise TimDbException(f'Not a settings paragraph: {par.get_id()}')
        try:
            yaml_vals = DocSettings.parse_values(par)
        except yaml.YAMLError as e:
            raise TimDbException(f'Invalid YAML: {e}')
        else:
            return DocSettings(par.doc, settings_dict=yaml_vals)

    @staticmethod
    def parse_values(par) -> YamlBlock:
        return YamlBlock.from_markdown(par.get_markdown())

    def __init__(self, doc: 'Document', settings_dict: Optional[YamlBlock] = None):
        self.doc = doc
        self.__dict = settings_dict if settings_dict else YamlBlock()
        self.user = None

    def to_paragraph(self) -> DocParagraph:
        text = '```\n' + self.__dict.to_markdown() + '\n```'
        return DocParagraph.create(self.doc, md=text, attrs={"settings": ""})

    def get_dict(self) -> YamlBlock:
        return self.__dict

    def global_plugin_attrs(self) -> dict:
        return self.__dict.get(self.global_plugin_attrs_key, {})

    def css(self):
        return self.__dict.get(self.css_key)

    def get_macroinfo(self, user=None, key=None) -> MacroInfo:
        if not key:
            key = self.macros_key
        return MacroInfo(self.doc, macro_map=self.__dict.get(key, {}),
                         macro_delimiter=self.get_macro_delimiter(),
                         user=user, nocache_user=self.user)

    def get_macro_delimiter(self) -> str:
        return self.__dict.get(self.macro_delimiter_key, '%%')

    def get_globalmacros(self) -> str:
        return self.__dict.get(self.globalmacros_key, {})

    def get_doctexmacros(self) -> str:
        return self.__dict.get(self.doctexmacros_key, '')

    def auto_number_questions(self) -> bool:
        return self.__dict.get(self.no_question_auto_numbering_key, False)

    def get_source_document(self) -> Optional[int]:
        return self.__dict.get(self.source_document_key)

    def get_slide_background_url(self, default=None) -> Optional[str]:
        return self.__dict.get(self.slide_background_url_key, default)

    def get_slide_background_color(self, default=None) -> Optional[str]:
        return self.__dict.get(self.slide_background_color_key, default)

    def get_bookmarks(self, default=None):
        if default is None:
            default = []
        return self.__dict.get(self.bookmark_key, default)

    def get_print_settings(self, default=None):
        if default is None:
            default = []
        return self.__dict.get(self.print_settings_key, default)

    def lazy(self, default=False):
        return self.__dict.get(self.lazy_key, default)

    def set_bookmarks(self, bookmarks: List[Dict]):
        self.__dict[self.bookmark_key] = bookmarks

    def set_source_document(self, source_docid: Optional[int]):
        self.__dict[self.source_document_key] = source_docid

    def auto_number_headings(self) -> int:
        return self.__dict.get(self.auto_number_headings_key, 0)

    def auto_number_start(self) -> int:
        return self.__dict.get(self.auto_number_start_key, 0)

    def heading_format(self) -> dict:
        level = self.auto_number_headings()
        defaults = {1: '{h1}. {text}',
                    2: '{h1}.{h2} {text}',
                    3: '{h1}.{h2}.{h3} {text}',
                    4: '{h1}.{h2}.{h3}.{h4} {text}',
                    5: '{h1}.{h2}.{h3}.{h4}.{h5} {text}',
                    6: '{h1}.{h2}.{h3}.{h4}.{h5}.{h6} {text}'}
        if level == 2:
            defaults = {
                1: '{text}',
                2: '{h2}. {text}',
                3: '{h2}.{h3} {text}',
                4: '{h2}.{h3}.{h4} {text}',
                5: '{h2}.{h3}.{h4}.{h5} {text}',
                6: '{h2}.{h3}.{h4}.{h5}.{h6} {text}'
            }
        if level == 3:
            defaults = {
                1: '{text}',
                2: '{text}',
                3: '{h3}. {text}',
                4: '{h3}.{h4} {text}',
                5: '{h3}.{h4}.{h5} {text}',
                6: '{h3}.{h4}.{h5}.{h6} {text}'
            }
        if level == 4:
            defaults = {
                1: '{text}',
                2: '{text}',
                3: '{text}',
                4: '{h4}. {text}',
                5: '{h4}.{h5} {text}',
                6: '{h4}.{h5}.{h6} {text}'
            }
        hformat = self.__dict.get(self.heading_format_key)
        if hformat is None:
            return defaults
        return {1: hformat.get(1, defaults[1]),
                2: hformat.get(2, defaults[2]),
                3: hformat.get(3, defaults[3]),
                4: hformat.get(4, defaults[4]),
                5: hformat.get(5, defaults[5]),
                6: hformat.get(6, defaults[6])}

    def show_task_summary(self, default=False) -> bool:
        return self.__dict.get(self.show_task_summary_key, default)

    def hide_links(self, default=None):
        return self.__dict.get(self.hide_links_key, default)

    def point_sum_rule(self, default=None):
        return self.__dict.get(self.point_sum_rule_key, default)

    def max_points(self, default=None):
        return self.__dict.get(self.max_points_key, default)

    def live_updates(self, default=None):
        return self.__dict.get(self.live_updates_key, default)

    def plugin_md(self, default=True):
        return self.__dict.get(self.plugin_md_key, default)

    def nomacros(self, default=False):
        nm = self.__dict.get(self.nomacros_key, None)
        if nm is None:
            nm = self.get(self.texplain_key, None)
            if nm is None:
                nm = default
        return nm

    def preamble(self, default=DEFAULT_PREAMBLE_DOC):
        return self.__dict.get(self.preamble_key, default)

    def get(self, key, default=None):
        return self.__dict.get(key, default)

    def is_texplain(self):
        texplain = self.__dict.get(self.texplain_key, False)
        return texplain

    def show_authors(self, default=False):
        return self.__dict.get(self.show_authors_key, default)

    def read_expiry(self, default=timedelta(weeks=9999)) -> timedelta:
        r = self.__dict.get(self.read_expiry_key)
        if not isinstance(r, int):
            return default
        return timedelta(minutes=r)

    def add_par_button_text(self, default='Add paragraph') -> str:
        return self.__dict.get(self.add_par_button_text_key, default)


def resolve_settings_for_pars(pars: Iterable[DocParagraph]) -> YamlBlock:
    result, _ = __resolve_final_settings_impl(pars)
    return result


def __resolve_final_settings_impl(pars: Iterable[DocParagraph]) -> Tuple[YamlBlock, bool]:
    result = YamlBlock()
    had_settings = False
    for curr in pars:
        if not curr.is_setting():
            break
        if not curr.is_reference():
            try:
                settings = DocSettings.from_paragraph(curr)
            except TimDbException:
                break
            result = result.merge_with(settings.get_dict())
            had_settings = True
        else:
            curr_own_settings = None

            # If the paragraph does not have rd attribute (for example if it is a translated one),
            # we have to dig the source_document so that we can resolve the reference without getting into infinite recursion.
            if curr.get_rd() is None:
                try:
                    curr_own_settings = DocSettings.from_paragraph(curr).get_dict()
                except TimDbException:
                    curr_own_settings = YamlBlock()
                if curr.is_citation():
                    from timApp.documentmodel.document import Document
                    source_doc_id = result.get('source_document', curr_own_settings.get('source_document'))
                    source_doc = Document(source_doc_id) if source_doc_id else None
                else:
                    source_doc = curr.doc.get_source_document()
            else:
                source_doc = None

            # If we still don't know the source document and there is no rd, we can't proceed.
            if curr.get_rd() is None and source_doc is None:
                # TODO: Should this be break?
                continue

            try:
                from timApp.documentmodel.document import Document
                # We temporarily pretend that this isn't a translated paragraph so that we always get the original markdown.
                tr_attr = curr.get_attr('r')
                curr.set_attr('r', None)
                refs = curr.get_referenced_pars(set_html=False, source_doc=source_doc)
                curr.set_attr('r', tr_attr)
            except InvalidReferenceException:
                break
            ref_settings, ref_had_settings = __resolve_final_settings_impl(refs)
            if ref_had_settings:
                result = result.merge_with(ref_settings)
                had_settings = True
                if curr.is_translation() or curr.is_citation():
                    result = result.merge_with(curr_own_settings)
            else:
                break
    return result, had_settings
