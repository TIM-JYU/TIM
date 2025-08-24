"""Provides functions for converting markdown-formatted text to HTML."""
from __future__ import annotations

import random
import re
from dataclasses import dataclass, field
from datetime import date, datetime, timedelta
from dateutil import parser
from re import Pattern
from typing import TYPE_CHECKING, Iterable, Any
from urllib.parse import quote_plus

from jinja2 import TemplateSyntaxError, pass_context
from jinja2.runtime import Context
from lxml import html, etree
from sqlalchemy.orm import load_only, lazyload

from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, default_view_ctx
from timApp.document.yamlblock import YamlBlock
from timApp.markdown.autocounters import (
    AutoCounters,
    TimSandboxedEnvironment,
    HEADING_TAGS,
    add_h_values,
    check_autonumber_error,
)
from timApp.util.utils import get_error_html, title_to_id, slugify
from timApp.util.utils import widen_fields
from tim_common.dumboclient import call_dumbo, DumboOptions
from tim_common.html_sanitize import sanitize_html, presanitize_html_body

if TYPE_CHECKING:
    from timApp.document.docparagraph import DocParagraph
    from timApp.document.docsettings import DocSettings
    from timApp.document.document import Document
    from timApp.document.macroinfo import MacroInfo


def has_macros(text: str, env: TimSandboxedEnvironment):
    return (
        env.variable_start_string in text
        or env.comment_start_string in text
        or env.block_start_string in text
    )


# ------------------------ Jinja filters -------------------------------------------------------------------
# Ks. https://tim.jyu.fi/view/tim/ohjeita/satunnaistus#timfiltterit


# To create a new filter,
#  1. make a class or function
#  2. and map it in create_environment


@pass_context
def local(ctx: Context, name: Any, default: Any = None) -> Any | None:
    """
    Get a local variable from the current context.

    :param ctx: Current context
    :param name: Name of the variable
    :param default: Default value if not found
    :return: Value of the variable or None if not found
    """
    if not isinstance(name, str):
        return None
    return ctx.get(name, default)


def genfields(flds, attrs="", stemfield="stem"):
    """
    Generates fields from namelist like ['se1', 'd1', 'd2=demo2']
    See usescases from: /tim/timApp/tests/server/test_genfields.py

    :param flds: list of fields, maybe with aliases to show in stem
    :param attrs: possible list of attributes
    :param stemfield: field to use to show filed ste, like sten, header or inputstem
    :return: TIM-format of fields
    """
    flds = widen_fields(flds)
    res = ""
    if attrs:
        attrs = ", " + attrs
    for fld in flds:
        parts = fld.split("=")
        fid = parts[0].strip()
        if len(parts) > 1:
            text = parts[1].strip()
        else:
            parts = fid.split(":")
            text = parts[0]
        s = f"{{#{fid} {stemfield}: '{text}'{attrs}#}}"
        res += s
    return res


def gfrange(s, i1, i2, attrs="", stemfield="stem"):
    flds = s.split(";", 1)
    s = flds[0]
    srest = ""
    if len(flds) > 1:
        srest = flds[1]
    # parts = s.split("=")
    # name = f"{parts[0]}({i1},{i2})"
    i = len(s)
    ic = s.find(":")  # a:b => a{0}:b
    ie = s.find("=")  # a=b => a{0}=b{0}  and a:b=c => a{0}:b=b{0}
    if ic >= 0:
        i = ic
    if ie >= 0 and (ie < ic or ic < 0):
        i = ie
    if i < 1:
        return ""
    s1 = s[:i]
    s2 = s[i:]
    f1 = ""
    f2 = ""
    if s1.find("{") < 0:
        f1 = "{0}"
    if s2.find("{") < 0:
        f2 = "{0}"
    s = s1 + f1 + s2
    if ie >= 0:
        s += f2
    s += ";"
    step = 1
    if i1 > i2:
        step = -1
    s = srange(s, i1, i2, step)
    return genfields(s + srest, attrs, stemfield)


def srange(s, i1, i2, step=1, *argv):
    """
    Jinja2 filter for generating indexed names

    :param s: format string for item
    :param i1: start index
    :param i2: exclusive end index
    :param step: how much increment
    :param argv pair of value to add and mul index
    :return: like "d1 d2 d3 "  by call sfrom('d{0} ', 1, 3)
    """
    result = ""
    cor = 1  # correct python range vs normal people range
    if step < 0:
        cor = -1
    if step == 0:
        step = 1
    for i in range(i1, i2 + cor, step):
        ext = []
        for j in range(0, len(argv), 2):
            add = argv[j]
            mul = 1
            if j + 1 < len(argv):
                mul = argv[j + 1]
            ext.append(mul * i + add)
        result += s.format(i, *ext)
    return result


# noinspection PyPep8Naming
def Pz(i):
    """
    Returns number as a string so that from 0 comes "", positive number comes like " + 1"
    and negative comes like " - 1"

    :param i: number to convert
    :return: number as a string suitable for expressions
    """
    if i > 0:
        return " + " + str(i)
    if i < 0:
        return " - " + str(-i)
    return ""


@dataclass
class Belongs:
    user_ctx: UserContext

    def __post_init__(self):
        self.cache = {}

    def belongs_to_group(self, groupname: str):
        b = self.cache.get(groupname, None)
        if b is not None:
            return b
        b = any(
            gr.name == groupname for gr in self.user_ctx.logged_user.effective_groups
        )
        self.cache[groupname] = b
        return b


def week_to_date(week_nr, daynr=1, year=None, frmt=None):
    """
    date object for week
    see: timApp/tests/unit/test_datefilters.py

    :param week_nr:  week number to get the date object
    :param daynr: day of week to get date
    :param year: year to get date
    :param frmt: extended format string
    :return: date object or formatted string
    """
    if week_nr <= 0:
        week_nr = date.today().isocalendar()[1]
    else:
        week_nr = int(week_nr)
    daynr = int(daynr)
    if not year:
        year = date.today().year
    else:
        year = int(year)
    d = date.fromisocalendar(year, week_nr, daynr)
    if frmt is None:
        return d
    return fmt_date(d, frmt)


def month_to_week(month, daynr=1, year=None):
    """
    get week number for month
    see: timApp/tests/unit/test_datefilters.py

    :param month: month number starting from 1
    :param daynr: day number of month
    :param year: from what year
    :return: week number
    """
    daynr = int(daynr)
    if month <= 0:
        month = date.today().month
    else:
        month = int(month)
    if not year:
        year = date.today().year
    else:
        year = int(year)
    d = date(year, month, daynr)
    return d.isocalendar()[1]


def now(frmt=0):
    """
    Used in Jinja macros like tomorrow: %%1 | now%%
    Or this week %% "%w" | now %%

    :param frmt: format for current date or delta for current date
    :return: current date + (fmt as int) if fmt is int, otherwise current timestamp formatted
    """
    if isinstance(frmt, int):
        return datetime.now() + timedelta(days=frmt)
    return fmt_date(datetime.now(), str(frmt))


def fmt_date(d, frmt="", weekdays="ma|ti|ke|to|pe|la|su"):
    """
    Format date using extended %d1, %m1 and %A1 for one number values
    see: timApp/tests/unit/test_datefilters.py

    :param d: date to format
    :param frmt: Python format
    :param weekdays: pipe separated list of weekday names, used for %w
    :return: string from d an format
    """
    if isinstance(d, str):
        d = str_to_date(d)
    ds = "" + str(d.day)
    ms = "" + str(d.month)
    if frmt == "":
        return str(ds) + "." + str(ms)
    frmt = frmt.replace("%d1", ds).replace("%m1", ms)
    if "%A1" in frmt:
        wd = d.weekday()
        weekdays = weekdays.split("|")
        frmt = frmt.replace("%A1", weekdays[wd % len(weekdays)])
    return d.strftime(frmt)


def str_to_date(s, input_fmt=None):
    """
    Convert string to datetime object
    see: timApp/tests/unit/test_datefilters.py

    :param s: string to convert
    :param input_fmt: optional input format for date
    :return: datetime object
    """
    if input_fmt:
        return datetime.strptime(s, input_fmt)

    dayfirst = False
    periods = s.count(".")
    space = s.find(" ")
    if periods > 0:
        dayfirst = True
        if periods == 1:  # parser.parse does not handle 1.2 correctly
            if space < 0:
                s = f"{s}.{date.today().year}"
            else:  # case 1.2 12
                d, h = s.split(" ", 1)
                s = f"{d}.{date.today().year} {h}"
    if periods == 0 and space > 0:
        colons = s.count(":")
        if colons == 0:  # case 1 12  => 1.12.2024 12:00
            s = f"{s}:00"

    try:
        dt = parser.parse(s, dayfirst=dayfirst)
        return dt
    except ValueError as e:
        raise ValueError(f"time data '{s}' does not match any known format") from e


def week_to_text(
    week_nr, year=None, frmt=" %d1.%m1|", days="ma|ti|ke|to|pe|", first_day=1
):
    """
    Convert week to clendar header format
    see: timApp/tests/unit/test_datefilters.py

    :param week_nr: what week to convert
    :param year: what year
    :param frmt: extended Python  date format
    :param days: pipe separated list of day names
    :param first_day: from what weekday to start
    :return: string suitable for calandar header
    """
    if week_nr <= 0:
        week_nr = date.today().isocalendar()[1]
    else:
        week_nr = int(week_nr)
    if not year:
        year = date.today().year
    else:
        year = int(year)
    s = ""
    beg = 0
    daynr = first_day
    first_empty = False
    while True:
        end = days.find("|", beg)
        if end == 0:
            first_empty = True
        if end < 0:
            s += days[beg:]
            break
        ds = fmt_date(week_to_date(week_nr, daynr, year), frmt)
        s += days[beg:end] + ds
        beg = end + 1
        daynr += 1
        if daynr > 7:
            if first_empty:  # starts with separator, we need the last
                end = days.find("|", beg)
                if end < 0:
                    end = 10000
                s += days[beg:end]
            break
    return s


def postinc(v, delta=1):
    old = v[0]
    v[0] += delta
    return old


def preinc(v, delta=1):
    v[0] += delta
    return v[0]


# ------------------------ Jinja filters end ---------------------------------------------------------------


def expand_macros(
    text: str,
    macros,
    settings: DocSettings | None,
    env: TimSandboxedEnvironment,
    ignore_errors: bool = False,
):
    # return text  # comment out when want to take time if this slows things
    charmacros = settings.get_charmacros() if settings else None
    if charmacros:
        for cm_key, cm_value in charmacros.items():
            text = text.replace(cm_key, cm_value)
    if env.counters:
        text = env.counters.do_char_macros(text)
    if not has_macros(text, env):
        return text
    orig_text = text
    try:
        globalmacros = settings.get_globalmacros() if settings else None
        if globalmacros:
            for gmacro in globalmacros:
                macrotext = "%%" + gmacro + "%%"
                pos = text.find(macrotext)
                if pos >= 0:
                    gm = str(globalmacros.get(gmacro, ""))
                    text = text.replace(macrotext, gm)
            gm = str(globalmacros.get("ADDFOREVERY", ""))
            if gm:
                text = gm + "\n" + text
        startstr = env.comment_start_string + "LOCAL"
        beg = text.find(startstr)
        if beg >= 0:
            endstr = env.comment_end_string
            end = text.find(endstr, beg)
            if end >= 0:
                local_macros_yaml = text[beg + len(startstr) : end]
                local_macros = YamlBlock.from_markdown(local_macros_yaml).values
                macros = {**macros, **local_macros}
        # TODO: should local macros be used in counters???
        if env.counters:
            env.counters.start_of_block()
        conv = env.from_string(text).render(macros)
        if env.counters and env.counters.need_update_labels:
            conv = env.counters.update_labels(conv)
        env.counters.is_plugin = False
        return conv
    except TemplateSyntaxError as e:
        if not ignore_errors:
            err = check_autonumber_error(e.message)
            if err is not None:
                return get_error_html(err)
            return get_error_html(f"Syntax error in macro template: {e}")
        return orig_text
    except Exception as e:
        if not ignore_errors:
            # traceback.print_exc()
            return get_error_html(f"Error in expanding macros: {e}")
        return orig_text


def expand_macros_info(text: str, macro_info: "MacroInfo", ignore_errors: bool = False):
    return expand_macros(
        text,
        macro_info.get_macros(),
        macro_info.doc.get_settings(),
        macro_info.jinja_env,
        ignore_errors,
    )


def belongs_placeholder(_s):
    return get_error_html("The belongs filter requires nocache=true attribute.")


def fmt(x, f: str):
    return format(x, f)


def get_document_id(doc_path: Any) -> int:
    from timApp.document.docentry import DocEntry

    if not isinstance(doc_path, str):
        return 0
    doc = DocEntry.find_by_path(
        doc_path, docentry_load_opts=[load_only(DocEntry.id), lazyload(DocEntry._block)]
    )
    return doc.id if doc else 0


def get_document_path(doc_id: Any) -> str:
    from timApp.document.docentry import DocEntry

    if isinstance(doc_id, int):
        doc_id_num = int(doc_id)
    elif isinstance(doc_id, str):
        doc_id_num = int(doc_id)
    else:
        return ""
    doc = DocEntry.find_by_id(
        doc_id_num,
        docentry_load_opts=[load_only(DocEntry.name), lazyload(DocEntry._block)],
    )
    return doc.path if doc else ""


@dataclass
class GetDocumentSettingMacro:
    doc: Document | None = None
    _settings_cache: dict[int, dict] = field(default_factory=dict)
    _allowed_docsetting_macro_attributes: dict[int, list[Pattern]] = field(
        default_factory=dict
    )

    def get_document_setting(self, setting_path: Any) -> Any:
        """
        Get document setting by path.

        Setting path is in format

        [doc_id].setting_name.subsetting_name...

        If doc_id is not given, current document is used if available.

        If no setting or subsetting is set, None is returned.
        Subsettings are parsed as follows:
        - For lists, subsetting can be an index
        - For dicts, subsetting is a name of the dict key
        - For any other values, subsettings are not permitted (None is returned if subsetting is given)

        :param setting_path: Subsetting path.
        :return: Value of the setting or None if not found.
        :raises Exception: If access to the setting is not allowed via the allowedDocsettingMacroAttributes setting.
        """
        from timApp.document.docentry import DocEntry

        if not isinstance(setting_path, str):
            return None
        parts = setting_path.split(".")
        if not parts:
            return None

        current_setting_path = setting_path
        first_part = parts[0]
        doc_id = -1
        if first_part.isdigit():
            doc_id = int(first_part)
            parts = parts[1:]
            current_setting_path = ".".join(parts)

        if not (settings_dict := self._settings_cache.get(doc_id)):
            if doc_id == -1:
                if not self.doc:
                    return None
                doc = self.doc
            else:
                doc = DocEntry.find_by_id(doc_id).document

            doc_settings = doc.get_settings()
            allowed_attrs = doc_settings.allowed_docsetting_macro_attributes()
            allowed_attr_patterns = []
            if allowed_attrs:
                regexes: list[str] = (
                    [allowed_attrs] if isinstance(allowed_attrs, str) else allowed_attrs
                )
                for regex in regexes:
                    allowed_attr_patterns.append(re.compile(regex))

            settings_dict = dict(doc.get_settings().get_dict().values)
            self._settings_cache[doc_id] = settings_dict
            self._allowed_docsetting_macro_attributes[doc_id] = allowed_attr_patterns

        allowed_patterns = self._allowed_docsetting_macro_attributes[doc_id]
        accessible = False
        for pattern in allowed_patterns:
            if pattern.match(current_setting_path):
                accessible = True
                break

        if not accessible:
            raise Exception(
                f"docsetting: Access to document setting {current_setting_path} in document {doc_id} is not allowed. "
                "Set the allowedDocsettingMacroAttributes document setting to allow the path."
            )

        current_val = settings_dict

        # Special case: return entire object if it is allowed
        if len(parts) == 1 and parts[0] == "":
            return current_val

        for part in parts:
            if isinstance(current_val, list) and part.isdigit():
                part = int(part)
                if 0 > part or part >= len(current_val):
                    return None
            elif isinstance(current_val, dict):
                if part not in current_val:
                    return None
            else:
                return None

            current_val = current_val[part]

        return current_val


def url_quote(s: Any) -> str:
    if isinstance(s, str):
        return quote_plus(s)
    return ""


def end_value(s: Any, defvalue: Any = "") -> str:
    if not isinstance(s, str):
        if isinstance(s, int):
            return str(s)
        return str(defvalue)
    i = len(s)
    if i == 0:
        return str(defvalue)
    i -= 1
    c = s[i]
    while i >= 0 and c in "0123456789":
        i -= 1
        c = s[i]
    ret = s[i + 1 :]
    if ret == "":
        ret = str(defvalue)
    return ret


def slugify_str(s: Any, underscore: Any = None) -> str:
    if underscore:
        underscore = True
    else:
        underscore = False
    if isinstance(s, str):
        return slugify(s, underscored=underscore)
    return s


def shuffle(s: Any, seed: Any = None) -> Any:
    """
    Shuffles the given iterable.

    :param s: Iterable to shuffle.
    :param seed: Seed for the random number generator.
    :return: Shuffled iterable.
    """
    try:
        tmp_list = list(s)
    except TypeError:
        return s
    if seed is None:
        random.shuffle(tmp_list)
    else:
        random.Random(seed).shuffle(tmp_list)
    return tmp_list


tim_filters = {
    "Pz": Pz,
    "gfields": genfields,
    "gfrange": gfrange,
    "local": local,
    "srange": srange,
    "now": now,
    "w2date": week_to_date,
    "m2w": month_to_week,
    "w2text": week_to_text,
    "slugify": slugify_str,
    "fmtdate": fmt_date,
    "str2date": str_to_date,
    "preinc": preinc,
    "postinc": postinc,
    "belongs": belongs_placeholder,
    "fmt": fmt,
    "docid": get_document_id,
    "docpath": get_document_path,
    "urlquote": url_quote,
    "endvalue": end_value,
    "shuffle": shuffle,
}


def create_environment(
    macro_delimiter: str,
    user_ctx: UserContext | None,
    view_ctx: ViewContext,
    macros: dict | None,
    doc: Document | None = None,
) -> TimSandboxedEnvironment:
    env = TimSandboxedEnvironment(macro_delimiter)
    env.filters.update(tim_filters)
    env.filters["isview"] = view_ctx.isview
    env.filters["docsetting"] = GetDocumentSettingMacro(doc).get_document_setting

    if macros:
        counters = AutoCounters(macros, doc)
        env.set_counters(counters)  # used in print.py

    if user_ctx:
        env.filters["belongs"] = Belongs(user_ctx).belongs_to_group
    return env


def md_to_html(
    text: str,
    sanitize: bool = True,
    macros: dict[str, object] | None = None,
    dumbo_options: DumboOptions = DumboOptions.default(),
    ignore_errors=False,
) -> str:
    """Converts the specified markdown text to HTML.

    :param ignore_errors: Whether to ignore errors expanding macros
    :param dumbo_options: Options for Dumbo.
    :param macros: The macros to use.
    :param sanitize: Whether the HTML should be sanitized. Default is True.
    :param text: The text to be converted.
    :return: A HTML string.

    """

    text = expand_macros(
        text,
        macros,
        settings=None,
        env=create_environment(
            "%%", user_ctx=None, view_ctx=default_view_ctx, macros=macros
        ),
        ignore_errors=ignore_errors,
    )
    raw = call_dumbo([text], options=dumbo_options)
    if sanitize:
        return sanitize_html(str(raw[0]))
    else:
        return raw[0]


def par_list_to_html_list(
    pars: list[DocParagraph],
    settings: DocSettings,
    view_ctx: ViewContext,
    auto_macros: Iterable[dict] | None = None,
):
    """Converts the specified list of DocParagraphs to an HTML list.

    :param view_ctx:
    :return: A list of HTML strings.
    :param settings: The document settings.
    :param auto_macros: Currently a list(dict) containing the heading information ('h': dict(int,int) of heading counts
           and 'headings': dict(str,int) of so-far used headings and their counts).
    :param pars: The list of DocParagraphs to be converted.

    """

    macroinfo = settings.get_macroinfo(view_ctx)
    # User-specific macros (such as %%username%% and %%realname%%) cannot be replaced here because the result will go
    # to global cache. We will replace them later (in post_process_pars).
    macroinfo.preserve_user_macros = True
    dumbo_opts = settings.get_dumbo_options()
    texts = [
        p.get_expanded_markdown(macroinfo)
        if not p.has_dumbo_options()
        else {
            "content": p.get_expanded_markdown(macroinfo),
            **p.get_dumbo_options(base_opts=dumbo_opts).dict(),
        }
        for p in pars
    ]

    texplain = settings.is_texplain()
    textplain = settings.is_textplain()
    if texplain or textplain:  # add pre-markers to tex paragraphs
        for i in range(0, len(texts)):
            text = texts[i]
            if text.find("```") != 0 and text.find("#") != 0:
                texts[i] = "```\n" + text + "\n```"
    raw = call_dumbo(texts, options=dumbo_opts)

    # Edit html after dumbo
    raw = edit_html_with_own_syntax(raw)

    if auto_macros:
        processed = []
        for pre_html, m, attrs in zip(raw, auto_macros, (p.get_attrs() for p in pars)):
            if "nonumber" in attrs.get("classes", []):
                final_html = pre_html
            else:
                final_html = insert_heading_numbers(
                    pre_html,
                    m,
                    settings.auto_number_headings() > 0,
                    settings.heading_format(),
                    initial_heading_counts=settings.auto_number_start(),
                )
            processed.append(final_html)
        raw = processed

    return raw


# Does changes to html after Dumbo and returns edited html
def edit_html_with_own_syntax(raw: list) -> list:
    index = 0
    while index < len(raw):
        html_text = raw[index]
        raw[index] = make_slide_fragments(html_text)
        # raw[index] = check_and_edit_html_if_surrounded_with(text, fragment_string, change_classes_to_fragment)
        index += 1
    return raw


# Adds the necessary html to make slide fragments work with reveal.js
def make_slide_fragments(html_text: str) -> str:
    # TODO: Make algorithm work with more than 2 levels of fragments
    # TODO: Make different styles of fragments available, possible syntax could be §§{shrink} or something
    # TODO: Refactor to make this more reusable
    # TODO: Make sure that this doesn't break latex conversion

    # Split from fragment area start tag <§
    fragments = html_text.split("&lt;§")
    # If no fragment areas were found we look for fragment pieces
    if len(fragments) < 2:
        new_html = check_and_edit_html_if_surrounded_with(
            html_text, "§§", change_classes_to_fragment
        )
        return new_html
    else:
        index = 1
        # For every fragment area
        while index < len(fragments):
            # Try to find area end
            index_of_area_end = fragments[index].find("§&gt;")
            # If not found
            if index_of_area_end == -1:
                # Look for normal fragments
                fragments[index] = check_and_edit_html_if_surrounded_with(
                    fragments[index], "§§", change_classes_to_fragment
                )
            else:
                # Make a new fragment area if start and end found
                fragments[index] = '</p><div class="fragment"><p>' + fragments[index]
                fragments[index] = fragments[index].replace("§&gt;", "</p></div><p>", 1)
                # Look for inner fragments
                fragments[index] = check_and_edit_html_if_surrounded_with(
                    fragments[index], "§§", change_classes_to_fragment
                )
            index += 1
        new_html = "".join(fragments)
        return new_html


# Checks if html element's content is surrounded with given string and edits it accordingly
def check_and_edit_html_if_surrounded_with(
    html_content: str, string_delimiter: str, editing_function
) -> str:
    # List of strings after splitting html from
    html_list = html_content.split(string_delimiter)
    if len(html_list) < 2:
        return html_content
    else:
        # Edit the list with given function
        new_html = editing_function(html_list)
    return new_html


def change_classes_to_fragment(html_list: list) -> str:
    """If found, html_list[1] will have the content that we need to make a fragment of and html_list[0] might have the
    element tag that will have "fragment" added to it's class.

    There might be multiple fragments in the html list.

    """
    # Start from 1, the previous will contain the html tag to change
    index = 1
    while index < len(html_list):
        # Changes html element's class to fragment
        new_htmls = change_class(html_list[index - 1], html_list[index], "fragment")
        # Apply changes
        html_list[index - 1] = new_htmls[0]
        html_list[index] = new_htmls[1]
        index += 2

    # Join the list into a string
    new_html = "".join(html_list)
    return new_html


def change_class(
    text_containing_html_tag: str, text_content: str, new_class: str
) -> list:
    """Find the last html tag in the list and change that element's class to new_class or add the new class to element's
    classes or surround the new content with span element with the new class."""
    try:
        # Find where the html tag supposedly ends
        index_of_tag_end = text_containing_html_tag.rfind(">")
        # Find where the html tag starts
        index_of_tag_start = text_containing_html_tag.rfind("<", 0, index_of_tag_end)
        # If the previous text ends a html tag
        if index_of_tag_end == len(text_containing_html_tag) - 1:
            # Html tag content is between those 2 indices
            html_tag = text_containing_html_tag[index_of_tag_start:index_of_tag_end]
            # Check if element already has atleast one class, if it does then add new_class
            if "class=" in html_tag:
                # Add the new class to html element classes
                index_of_class = html_tag.rfind("class=")
                text_containing_html_tag = (
                    text_containing_html_tag[
                        : (index_of_tag_start + index_of_class + 7)
                    ]
                    + new_class
                    + " "
                    + text_containing_html_tag[
                        (index_of_tag_start + index_of_class + 7) :
                    ]
                )
            else:
                # If there isn't class in html tag we add that and the new class
                text_containing_html_tag = (
                    text_containing_html_tag[:index_of_tag_end]
                    + ' class="'
                    + new_class
                    + '"'
                    + text_containing_html_tag[index_of_tag_end:]
                )
        else:
            text_content = '<span class="' + new_class + '">' + text_content + "</span>"
    # If there is an error we do nothing but return the original text
    except ValueError:
        pass
    return [text_containing_html_tag, text_content]


# Some header IDs conflict with clientside code, so we need to adjust them
CONFLICTING_HEADER_IDS = {
    "angular",  # Reserved by AngularJS
}


def insert_heading_numbers(
    html_str: str,
    heading_info,
    auto_number_headings: int | bool = True,
    heading_format: dict | None = None,
    initial_heading_counts: dict[int, int] | None = None,
):
    """Applies the given heading_format to the HTML if it is a heading, based on the given heading_info. Additionally
    corrects the id attribute of the heading in case it has been used earlier.

    :param heading_info: A dict containing the heading information ('h': dict(int,int) of heading counts
           and 'headings': dict(str,int) of so-far used headings and their counts).
    :param html_str: The HTML string to be processed.
    :param auto_number_headings: Whether the headings should be formatted at all.
    :param heading_format: A dict(int,str) of the heading formats to be used.
    :param initial_heading_counts: Initial heading counter value for each level
    :return: The HTML with the formatted headings.

    """
    tree = html.fragment_fromstring(presanitize_html_body(html_str), create_parent=True)
    counts = heading_info["h"]
    used = heading_info["headings"]
    for e in tree.iterchildren():
        is_heading = e.tag in HEADING_TAGS
        if not is_heading:
            continue
        curr_id = title_to_id(e.text)
        hcount = used.get(curr_id, 0)
        if hcount > 0 or curr_id.lower() in CONFLICTING_HEADER_IDS:
            try:
                e.attrib["id"] += f"-{hcount}"
            except KeyError:
                e.set("id", f"{curr_id}-{hcount}")
        if auto_number_headings:
            e.text = format_heading(
                e.text or "",
                int(e.tag[1]),
                counts,
                heading_format,
                initial_counts=initial_heading_counts,
            )
    final_html = etree.tostring(tree, encoding="utf-8")
    return final_html


def format_heading(
    text,
    level,
    counts,
    heading_format,
    heading_ref_format: dict = None,
    jump_name: str = None,
    counters: AutoCounters = None,
    initial_counts: dict[int, int] | None = None,
):
    counts[level] += 1
    for i in range(level + 1, 7):
        counts[i] = initial_counts.get(i, 0)
    values = {"text": text}
    add_h_values(counts, values)
    try:
        formatted = heading_format[level].format(**values)

        if heading_ref_format and jump_name and counters:
            formatted_ref = heading_ref_format[level].format(**values)
            counters.add_counter("chap", jump_name, formatted_ref, formatted)
    except (KeyError, ValueError, IndexError):
        formatted = "[ERROR] " + text
    return formatted
