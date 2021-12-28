from dataclasses import dataclass, fields
from datetime import timedelta
from typing import Optional, Iterable, TypeVar, Any, TYPE_CHECKING, Union

import yaml
from marshmallow import ValidationError
from marshmallow.fields import Field

from timApp.answer.pointsumrule import PointSumRule
from timApp.document.docparagraph import DocParagraph
from timApp.document.macroinfo import MacroInfo
from timApp.document.randutils import hashfunc
from timApp.document.specialnames import DEFAULT_PREAMBLE_DOC
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, default_view_ctx
from timApp.document.yamlblock import YamlBlock
from timApp.markdown.dumboclient import MathType, DumboOptions, InputFormat
from timApp.timdb.exceptions import TimDbException, InvalidReferenceException
from tim_common.marshmallow_dataclass import field_for_schema

if TYPE_CHECKING:
    from timApp.document.document import Document


@dataclass
class MinimalVisibilitySettings:
    exam_mode: bool = False
    hide_sidemenu: bool = False


def get_minimal_visibility_settings(item: Optional["Document"]):
    if not item:
        return MinimalVisibilitySettings()
    settings = item.get_settings()
    return MinimalVisibilitySettings(
        exam_mode=settings.exam_mode() is not None,
        hide_sidemenu=settings.hide_sidemenu() is not None,
    )


UrlMacroMap = dict[str, Union[int, float, str]]


# TODO: Start moving DocSettings keys to this dataclass
@dataclass
class DocSettingTypes:
    themes: list[str]
    override_user_themes: bool
    hide_sidemenu: Optional[str]
    answer_submit_time_tolerance: int
    scoreboard_docs: list[str]
    show_scoreboard: bool
    hideBrowser: bool
    macros: dict[str, Any]
    texmacros: dict[str, Any]
    urlmacros: UrlMacroMap
    rndmacros: dict[str, str]
    cache: bool
    peer_review: bool
    peer_review_count: int
    access_denied_message: str
    disable_answer: str
    smart_punct: bool
    slide_themes: list[str]
    slide_size: tuple[int, int]
    allow_url_permission_edits: bool


doc_setting_field_map: dict[str, Field] = {
    f.name: field_for_schema(f.type) for f in fields(DocSettingTypes)
}


T = TypeVar("T")


class DocSettings:
    global_plugin_attrs_key = "global_plugin_attrs"
    css_key = "css"
    globalmacros_key = "globalmacros"
    doctexmacros_key = "doctexmacros"
    macro_delimiter_key = "macro_delimiter"
    source_document_key = "source_document"
    auto_number_headings_key = "auto_number_headings"
    auto_number_start_key = "auto_number_start"
    heading_format_key = "heading_format"
    heading_ref_format_key = "heading_ref_format"
    show_task_summary_key = "show_task_summary"
    no_question_auto_numbering_key = "no_question_auto_numbering"
    slide_background_url_key = "slide_background_url"
    slide_background_color_key = "slide_background_color"
    bookmark_key = "bookmarks"
    lazy_key = "lazy"
    hide_links_key = "hide_links"
    pars_only_key = "pars_only"
    hide_top_buttons_key = "hide_top_buttons"
    point_sum_rule_key = "point_sum_rule"
    max_points_key = "max_points"
    nomacros_key = "nomacros"
    texplain_key = "texplain"
    textplain_key = "textplain"
    live_updates_key = "live_updates"
    plugin_md_key = "plugin_md"
    print_settings_key = "print_settings"
    preamble_key = "preamble"
    show_authors_key = "show_authors"
    read_expiry_key = "read_expiry"
    add_par_button_text_key = "add_par_button_text"
    mathtype_key = "math_type"
    math_preamble_key = "math_preamble"
    input_format_key = "input_format"
    memo_minutes_key = "memo_minutes"
    comments_key = "comments"
    course_group_key = "course_group"
    charmacros_key = "charmacros"
    postcharmacros_key = "postcharmacros"
    rndmacros_key = "rndmacros"
    sisu_require_manual_enroll_key = "sisu_require_manual_enroll"
    course_allow_manual_enroll_key = "course_allow_manual_enroll"
    show_velps_key = "show_velps"
    group_key = "group"
    allow_self_confirm_from_key = "allow_self_confirm_from"
    auto_confirm_key = "auto_confirm"
    expire_next_doc_message_key = "expire_next_doc_message"
    exam_mode_key = "exam_mode"
    answer_grace_period_key = "answer_grace_period"

    @classmethod
    def from_paragraph(cls, par: DocParagraph):
        """Constructs DocSettings from the given DocParagraph.

        :param par: The DocParagraph to extract settings from.
        :return: The DocSettings object.

        """
        if not par.is_setting():
            raise TimDbException(f"Not a settings paragraph: {par.get_id()}")
        try:
            yaml_vals = DocSettings.parse_values(par)
        except yaml.YAMLError as e:
            raise TimDbException(f"Invalid YAML: {e}")
        else:
            return DocSettings(par.doc, settings_dict=yaml_vals)

    @staticmethod
    def parse_values(par) -> YamlBlock:
        return YamlBlock.from_markdown(par.get_markdown())

    def get_setting_or_default(self, name: str, default: T) -> T:
        try:
            return doc_setting_field_map[name].deserialize(self.__dict.get(name))
        except ValidationError:
            return default

    def __init__(self, doc: "Document", settings_dict: Optional[YamlBlock] = None):
        self.doc = doc
        self.__dict = settings_dict if settings_dict else YamlBlock()
        self.macroinfo_cache = {}

    def to_paragraph(self) -> DocParagraph:
        text = "```\n" + self.__dict.to_markdown() + "\n```"
        return DocParagraph.create(self.doc, md=text, attrs={"settings": ""})

    def get_dict(self) -> YamlBlock:
        return self.__dict

    def get_safe_dict(self) -> dict:
        result = dict(self.__dict.values)
        result.pop("macros", None)
        return result

    def global_plugin_attrs(self) -> dict:
        return self.__dict.get(self.global_plugin_attrs_key, {})

    def css(self):
        return self.__dict.get(self.css_key)

    def get_macroinfo(
        self, view_ctx: ViewContext, user_ctx: Optional[UserContext] = None
    ) -> MacroInfo:
        cache_key = (
            view_ctx,
            (user_ctx.user.id, user_ctx.logged_user.id) if user_ctx else None,
        )
        cached = self.macroinfo_cache.get(cache_key)
        if cached:
            return cached
        mi = MacroInfo(
            view_ctx,
            self.doc,
            macro_map=self.get_setting_or_default("macros", {}),
            macro_delimiter=self.get_macro_delimiter(),
            user_ctx=user_ctx,
        )
        self.macroinfo_cache[cache_key] = mi
        return mi

    def get_texmacroinfo(
        self, view_ctx: ViewContext, user_ctx: Optional[UserContext] = None
    ) -> MacroInfo:
        return MacroInfo(
            view_ctx,
            self.doc,
            macro_map=self.get_setting_or_default("texmacros", {}),
            macro_delimiter=self.get_macro_delimiter(),
            user_ctx=user_ctx,
        )

    def get_macro_delimiter(self) -> str:
        return self.__dict.get(self.macro_delimiter_key, "%%")

    def get_globalmacros(self) -> dict[str, str]:
        return self.__dict.get(self.globalmacros_key, {})

    def get_doctexmacros(self) -> str:
        return self.__dict.get(self.doctexmacros_key, "")

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

    def set_bookmarks(self, bookmarks: list[dict]):
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
            raise ValueError(f"group must be str, not {type(res)}")
        return res

    def auto_number_start(self) -> int:
        return self.__dict.get(self.auto_number_start_key, 0)

    def heading_format_ret(self, defaults, format_key):
        hformat = self.__dict.get(format_key)
        if hformat is None:
            return defaults
        return {
            1: hformat.get(1, defaults[1]),
            2: hformat.get(2, defaults[2]),
            3: hformat.get(3, defaults[3]),
            4: hformat.get(4, defaults[4]),
            5: hformat.get(5, defaults[5]),
            6: hformat.get(6, defaults[6]),
        }

    def heading_format(self) -> dict:
        level = self.auto_number_headings()
        defaults = {
            1: "{h1}. {text}",
            2: "{h1}.{h2} {text}",
            3: "{h1}.{h2}.{h3} {text}",
            4: "{h1}.{h2}.{h3}.{h4} {text}",
            5: "{h1}.{h2}.{h3}.{h4}.{h5} {text}",
            6: "{h1}.{h2}.{h3}.{h4}.{h5}.{h6} {text}",
        }
        if level == 2:
            defaults = {
                1: "{text}",
                2: "{h2}. {text}",
                3: "{h2}.{h3} {text}",
                4: "{h2}.{h3}.{h4} {text}",
                5: "{h2}.{h3}.{h4}.{h5} {text}",
                6: "{h2}.{h3}.{h4}.{h5}.{h6} {text}",
            }
        if level == 3:
            defaults = {
                1: "{text}",
                2: "{text}",
                3: "{h3}. {text}",
                4: "{h3}.{h4} {text}",
                5: "{h3}.{h4}.{h5} {text}",
                6: "{h3}.{h4}.{h5}.{h6} {text}",
            }
        if level == 4:
            defaults = {
                1: "{text}",
                2: "{text}",
                3: "{text}",
                4: "{h4}. {text}",
                5: "{h4}.{h5} {text}",
                6: "{h4}.{h5}.{h6} {text}",
            }
        return self.heading_format_ret(defaults, self.heading_format_key)

    def heading_ref_format(self) -> dict:
        level = self.auto_number_headings()
        defaults = {
            1: "{h1}",
            2: "{h1}.{h2}",
            3: "{h1}.{h2}.{h3}",
            4: "{h1}.{h2}.{h3}.{h4}",
            5: "{h1}.{h2}.{h3}.{h4}.{h5}",
            6: "{h1}.{h2}.{h3}.{h4}.{h5}.{h6}",
        }
        if level == 2:
            defaults = {
                1: "",
                2: "{h2}",
                3: "{h2}.{h3}",
                4: "{h2}.{h3}.{h4}",
                5: "{h2}.{h3}.{h4}.{h5}",
                6: "{h2}.{h3}.{h4}.{h5}.{h6}",
            }
        if level == 3:
            defaults = {
                1: "",
                2: "",
                3: "{h3}",
                4: "{h3}.{h4}",
                5: "{h3}.{h4}.{h5}",
                6: "{h3}.{h4}.{h5}.{h6}",
            }
        if level == 4:
            defaults = {
                1: "",
                2: "",
                3: "",
                4: "{h4}",
                5: "{h4}.{h5}",
                6: "{h4}.{h5}.{h6}",
            }
        return self.heading_format_ret(defaults, self.heading_ref_format_key)

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

    def point_sum_rule(self, default=None) -> Optional[PointSumRule]:
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

    def plugin_md(self, default=True) -> bool:
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

    def get(self, key: str, default: Any = None) -> Any:
        return self.__dict.get(key, default)

    def is_texplain(self):
        texplain = self.__dict.get(self.texplain_key, False)
        return texplain

    def is_textplain(self):
        textplain = self.__dict.get(self.textplain_key, False)
        return textplain

    def show_authors(self, default=False):
        return self.__dict.get(self.show_authors_key, default)

    def read_expiry(self, default=timedelta(weeks=9999)) -> timedelta:
        r = self.__dict.get(self.read_expiry_key)
        if not isinstance(r, int):
            return default
        return timedelta(minutes=r)

    def add_par_button_text(self, default="Add paragraph") -> str:
        return self.__dict.get(self.add_par_button_text_key, default)

    def mathtype(self, default="mathjax") -> MathType:
        return MathType.from_string(self.__dict.get(self.mathtype_key, default))

    def get_hash(self):
        macroinfo = self.get_macroinfo(default_view_ctx)
        macros = macroinfo.get_macros()
        charmacros = self.get_charmacros() or ""
        macro_delim = macroinfo.get_macro_delimiter()
        return hashfunc(
            f"{macros}{macro_delim}{charmacros}{self.auto_number_headings()}{self.heading_format()}{self.mathtype()}{self.get_globalmacros()}{self.preamble()}{self.input_format()}{self.smart_punct()}"
        )

    def math_preamble(self):
        return self.__dict.get(self.math_preamble_key, "")

    def input_format(self):
        return InputFormat.from_string(
            self.__dict.get(self.input_format_key, "markdown")
        )

    def get_dumbo_options(self) -> DumboOptions:
        return DumboOptions(
            math_type=self.mathtype(),
            math_preamble=self.math_preamble(),
            input_format=self.input_format(),
            smart_punct=self.smart_punct(),
        )

    def memo_minutes_settings(self) -> Optional[dict]:
        macros = self.__dict.get("macros", {})
        if not isinstance(macros, dict):
            return None
        dates = macros.get("dates", None)
        knro = macros.get("knro", None)
        stampformat = macros.get("stampformat", None)
        if dates is None and knro is None and stampformat is None:
            return None
        return {"dates": dates, "knro": knro, "stampformat": stampformat}

    def memo_minutes(self) -> bool:
        return self.__dict.get(self.memo_minutes_key, "")

    def comments(self) -> str:
        return self.__dict.get(self.comments_key)

    def sisu_require_manual_enroll(self):
        return self.__dict.get(self.sisu_require_manual_enroll_key, False)

    def course_allow_manual_enroll(self) -> bool:
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

    def themes(self) -> list[str]:
        return self.get_setting_or_default("themes", [])

    def override_user_themes(self) -> bool:
        return self.get_setting_or_default("override_user_themes", False)

    def hide_sidemenu(self) -> Optional[str]:
        return self.get_setting_or_default("hide_sidemenu", None)

    def answer_submit_time_tolerance(self) -> timedelta:
        r = self.get_setting_or_default("answer_submit_time_tolerance", 1000)
        return timedelta(milliseconds=r if r >= 0 else 0)

    def scoreboard_docs(self) -> list[str]:
        return self.get_setting_or_default("scoreboard_docs", [])

    def show_scoreboard(self) -> bool:
        return self.get_setting_or_default("show_scoreboard", False)

    def hide_browser(self) -> bool:
        return self.get_setting_or_default("hideBrowser", False)

    def urlmacros(self) -> UrlMacroMap:
        return self.get_setting_or_default("urlmacros", {})

    def rndmacros(self) -> dict[str, str]:
        return self.get_setting_or_default("rndmacros", {})

    def is_cached(self):
        return self.get_setting_or_default("cache", False)

    def peer_review(self) -> bool:
        return self.get_setting_or_default("peer_review", False)

    def peer_review_count(self) -> int:
        return self.get_setting_or_default("peer_review_count", 1)

    def access_denied_message(self) -> Optional[str]:
        return self.get_setting_or_default("access_denied_message", None)

    def disable_answer(self) -> Optional[str]:
        return self.get_setting_or_default("disable_answer", None)

    def smart_punct(self) -> bool:
        return self.get_setting_or_default("smart_punct", False)

    def slide_themes(self) -> list[str]:
        return self.get_setting_or_default("slide_themes", ["jyu"])

    def slide_size(self) -> tuple[int, int]:
        return self.get_setting_or_default("slide_size", (960, 700))

    def allow_url_permission_edits(self) -> bool:
        return self.get_setting_or_default("allow_url_permission_edits", False)

    def is_style_document(self) -> bool:
        return self.get("description", None) is not None


def resolve_settings_for_pars(pars: Iterable[DocParagraph]) -> YamlBlock:
    result, _ = __resolve_final_settings_impl(pars)
    return result


def __resolve_final_settings_impl(
    pars: Iterable[DocParagraph],
) -> tuple[YamlBlock, bool]:
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
                tr_attr = curr.get_attr("r")
                curr.set_attr("r", None)
                refs = curr.get_referenced_pars()
                curr.set_attr("r", tr_attr)
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
