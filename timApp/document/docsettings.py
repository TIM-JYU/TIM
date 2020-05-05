import json
import re
from datetime import timedelta
from typing import Optional, List, Dict, Tuple, Iterable

import yaml
from flask.globals import request, g

from timApp.answer.pointsumrule import PointSumRule
from timApp.document.docparagraph import DocParagraph
from timApp.document.macroinfo import MacroInfo
from timApp.document.randutils import hashfunc
from timApp.document.specialnames import DEFAULT_PREAMBLE_DOC
from timApp.document.yamlblock import YamlBlock
from timApp.markdown.dumboclient import MathType, DumboOptions, InputFormat
from timApp.timdb.exceptions import TimDbException, InvalidReferenceException
from timApp.util.rndutils import get_rands_as_dict


def add_rnd_macros(yaml_vals):
    if DocSettings.rndmacros_key not in yaml_vals.values:
        return

    try:
        rnd_seed = g.user.name
    except AttributeError or NameError:
        rnd_seed = None
    state = None
    if not yaml_vals.values.get('macros', None):
        yaml_vals.values["macros"] = {}
    rndmacros = yaml_vals.values.get(DocSettings.rndmacros_key)
    if not isinstance(rndmacros, list):
        rndmacros = [rndmacros]
    for rndm in rndmacros or {}:
        if rndm:
            if isinstance(rndm, str):
                try:
                    rndm = json.loads(rndm)
                except json.JSONDecodeError as e:
                    raise TimDbException(f'Invalid YAML: {e}')  # TODO don't panic the page, but show the error?
            if not 'rndnames' in rndm:
                rndnames = []
                for rnd_name in rndm:
                    if rnd_name not in ["seed"]:  # todo put other non rnd names here
                        rndnames.append(rnd_name)
                rndm["rndnames"] = ",".join(rndnames)
            rands, rnd_seed, state = get_rands_as_dict(rndm, rnd_seed, state)
            for rnd_name in rands or {}:
                rnd = rands[rnd_name]
                yaml_vals.values["macros"][rnd_name] = rnd


def add_url_macros(yaml_vals):
    if DocSettings.urlmacros_key not in yaml_vals.values:
        return

    urlmacros = yaml_vals.values.get(DocSettings.urlmacros_key)
    for fu in urlmacros:
        if fu:
            # request = par.doc.docinfo.request

            # TODO: if already value and urlmacros.get(fu) then old value wins
            urlvalue = request.args.get(fu, urlmacros.get(fu))
            if urlvalue:
                try:
                    if not yaml_vals.values.get('macros', None):
                        yaml_vals.values["macros"] = {}
                    try:
                        uvalue = float(urlvalue)
                    except ValueError:
                        uvalue = None
                    if uvalue is not None:
                        maxvalue = yaml_vals.values["macros"].get("MAX" + fu, None)
                        if maxvalue is not None:
                            if uvalue > maxvalue:
                                urlvalue = maxvalue
                        minvalue = yaml_vals.values["macros"].get("MIN" + fu, None)
                        if minvalue is not None:
                            if uvalue < minvalue:
                                urlvalue = minvalue
                    urlvalue = DocSettings.urlmacros_tester.sub("", str(urlvalue))
                    yaml_vals.values["macros"][fu] = urlvalue
                except TypeError:
                    pass
    del yaml_vals.values[DocSettings.urlmacros_key]


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
    pars_only_key = 'pars_only'
    hide_top_buttons_key = 'hide_top_buttons'
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
    mathtype_key = 'math_type'
    math_preamble_key = 'math_preamble'
    input_format_key = 'input_format'
    memo_minutes_key = 'memo_minutes'
    comments_key = 'comments'
    course_group_key = 'course_group'
    urlmacros_key = 'urlmacros'
    charmacros_key = 'charmacros'
    postcharmacros_key = 'postcharmacros'
    rndmacros_key = 'rndmacros'
    sisu_require_manual_enroll_key = 'sisu_require_manual_enroll'
    course_allow_manual_enroll_key = 'course_allow_manual_enroll'
    show_velps_key = "show_velps"
    group_key = "group"
    allow_self_confirm_from_key = 'allow_self_confirm_from'
    auto_confirm_key = 'auto_confirm'
    expire_next_doc_message_key = 'expire_next_doc_message'
    exam_mode_key = 'exam_mode'
    answer_grace_period_key = 'answer_grace_period'

    urlmacros_tester = re.compile("[^0-9A-Za-zÅÄÖåäöÜü.,_ \-/]+")


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
            # TODO: Comes here at least 6 times with same par? Could this be cached?
            # TODO: This rnd + URL handling should not be here.
            try:
                add_rnd_macros(yaml_vals)  # Make global random numbers for document
                add_url_macros(yaml_vals)  # Replace some macros with values coming from URL
            except RuntimeError:
                pass

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
        # self.request = None

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

    def get_globalmacros(self) -> Dict[str, str]:
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

    def course_group(self):
        return self.__dict.get(self.course_group_key)

    def lazy(self, default=False):
        return self.__dict.get(self.lazy_key, default)

    def set_bookmarks(self, bookmarks: List[Dict]):
        self.__dict[self.bookmark_key] = bookmarks

    def set_source_document(self, source_docid: Optional[int]):
        self.__dict[self.source_document_key] = source_docid

    def auto_number_headings(self) -> int:
        return self.__dict.get(self.auto_number_headings_key, 0)

    def show_velps(self) -> bool:
        res = self.__dict.get(self.show_velps_key, True)
        return res

    def group(self) -> Optional[str]:
        res = self.__dict.get(self.group_key, None)
        if res is not None and not isinstance(res, str):
            raise ValueError(f'group must be str, not {type(res)}')
        return res

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

    def hide_top_buttons(self, default=None):
        return self.__dict.get(self.hide_top_buttons_key, default)

    def pars_only(self, default=None):
        return self.__dict.get(self.pars_only_key, default)

    def exam_mode(self, default=None):
        return self.__dict.get(self.exam_mode_key, default)

    def point_sum_rule(self, default=None):
        psr_dict = self.__dict.get(self.point_sum_rule_key, default)
        if not psr_dict:
            return None
        try:
            return PointSumRule(psr_dict)
        except:
            return None

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

    def get_charmacros(self, default=None):
        return self.__dict.get(self.charmacros_key, default)

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

    def mathtype(self, default='mathjax') -> MathType:
        return MathType.from_string(self.__dict.get(self.mathtype_key, default))

    def get_hash(self):
        macroinfo = self.get_macroinfo()
        macros = macroinfo.get_macros()
        charmacros = self.get_charmacros() or ""
        macro_delim = macroinfo.get_macro_delimiter()
        return hashfunc(f"{macros}{macro_delim}{charmacros}{self.auto_number_headings()}{self.heading_format()}{self.mathtype()}{self.get_globalmacros()}{self.preamble()}{self.input_format()}")

    def math_preamble(self):
        return self.__dict.get(self.math_preamble_key, '')

    def input_format(self):
        return InputFormat.from_string(self.__dict.get(self.input_format_key, 'markdown'))

    def get_dumbo_options(self):
        return DumboOptions(
            math_type=self.mathtype(),
            math_preamble=self.math_preamble(),
            input_format=self.input_format(),
        )

    def memo_minutes(self) -> bool:
        return self.__dict.get(self.memo_minutes_key, '')

    def comments(self):
        return self.__dict.get(self.comments_key)

    def sisu_require_manual_enroll(self):
        return self.__dict.get(self.sisu_require_manual_enroll_key, False)

    def course_allow_manual_enroll(self):
        return self.__dict.get(self.course_allow_manual_enroll_key, False)

    def expire_next_doc_message(self):
        return self.__dict.get(self.expire_next_doc_message_key)

    def allow_self_confirm_from(self):
        return self.__dict.get(self.allow_self_confirm_from_key)

    def auto_confirm(self):
        return self.__dict.get(self.auto_confirm_key)

    def answer_grace_period(self) -> timedelta:
        r = self.__dict.get(self.answer_grace_period_key, 5)
        if not isinstance(r, int):
            return timedelta(minutes=5)
        return timedelta(minutes=r)


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

            is_tr_or_cit = curr.is_translation() or curr.is_citation()
            if is_tr_or_cit:
                try:
                    curr_own_settings = DocSettings.from_paragraph(curr).get_dict()
                except TimDbException:
                    curr_own_settings = YamlBlock()

            try:
                from timApp.document.document import Document
                # We temporarily pretend that this isn't a translated paragraph
                # so that we always get the original markdown.
                tr_attr = curr.get_attr('r')
                curr.set_attr('r', None)
                refs = curr.get_referenced_pars(set_html=False)
                curr.set_attr('r', tr_attr)
            except InvalidReferenceException:
                break
            ref_settings, ref_had_settings = __resolve_final_settings_impl(refs)
            if ref_had_settings:
                result = result.merge_with(ref_settings)
                had_settings = True
                if is_tr_or_cit:
                    result = result.merge_with(curr_own_settings)
            else:
                break
    return result, had_settings
