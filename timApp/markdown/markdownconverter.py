"""Provides functions for converting markdown-formatted text to HTML."""
from __future__ import annotations

import json
from dataclasses import dataclass
from datetime import date, datetime, timedelta
from typing import TYPE_CHECKING, Iterable, Any

from jinja2 import TemplateSyntaxError
from jinja2.sandbox import SandboxedEnvironment
from lxml import html, etree
from sqlalchemy.orm import load_only, lazyload

from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, default_view_ctx
from timApp.document.yamlblock import YamlBlock
from timApp.markdown.dumboclient import call_dumbo
from timApp.util.utils import get_error_html, title_to_id
from timApp.util.utils import widen_fields
from tim_common.html_sanitize import sanitize_html, presanitize_html_body

if TYPE_CHECKING:
    from timApp.document.docparagraph import DocParagraph
    from timApp.document.docsettings import DocSettings


class TimSandboxedEnvironment(SandboxedEnvironment):
    def __init__(self, macro_delimiter: str):
        super().__init__(
            variable_start_string=macro_delimiter,
            variable_end_string=macro_delimiter,
            comment_start_string="{!!!",
            comment_end_string="!!!}",
            block_start_string="{%",
            block_end_string="%}",
            lstrip_blocks=True,
            trim_blocks=True,
        )
        self.counters: Counters or None = None

    def set_counters(self, counters: Counters):
        self.counters = counters
        counters.set_env_filters(self)

    def get_counters(self) -> Counters:
        return self.counters


def has_macros(text: str, env: SandboxedEnvironment):
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
    Returns number as a string so that from 0 comes "", postive number comes like " + 1"
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
        b = any(gr.name == groupname for gr in self.user_ctx.logged_user.groups)
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
    :return: date object or formated string
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
    :param month: month numer starting from 1
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
    :return: current date + (fmt as int) if fmt is int, otherwise current timestamp formated
    """
    if isinstance(frmt, int):
        return datetime.now() + timedelta(days=frmt)
    return fmt_date(datetime.now(), str(frmt))


def fmt_date(d, frmt=""):
    """
    Format date using extended %d1 and %m1 for one number values
    see: timApp/tests/unit/test_datefilters.py
    :param d: date to format
    :param frmt: Python format
    :return: string from d an format
    """
    ds = "" + str(d.day)
    ms = "" + str(d.month)
    if frmt == "":
        return str(ds) + "." + str(ms)
    frmt = frmt.replace("%d1", ds).replace("%m1", ms)
    return d.strftime(frmt)


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
    env: SandboxedEnvironment,
    ignore_errors: bool = False,
):
    # return text  # comment out when want to take time if this slows things
    charmacros = settings.get_charmacros() if settings else None
    if charmacros:
        for cm_key, cm_value in charmacros.items():
            text = text.replace(cm_key, cm_value)
    if not has_macros(text, env):
        return text
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
        conv = env.from_string(text).render(macros)
        return conv
    except TemplateSyntaxError as e:
        if not ignore_errors:
            return get_error_html(f"Syntax error in template: {e}")
        return text
    except Exception as e:
        if not ignore_errors:
            return get_error_html(f"Syntax error in template: {e}")
        return text


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


LABEL_PRFIX = "c:"


@dataclass
class Counters:
    macros: dict

    def __post_init__(self):
        self.renumbering = False
        self.counters = {}
        self.c = self.macros.get("c", {})
        self.heading_vals_def = {
            1: 0,
            2: 0,
            3: 0,
            4: 0,
            5: 0,
            6: 0,
        }
        self.heading_vals = None
        self.autocounters_fmt_def = {
            "eq": {
                "show": "{n}",
                "pure": "{n}",
                "ref": "({n})",
                "long": "({n})",
            },
            "chap": {
                "reset": -1,
            },
            "all": {
                "reset": -1,
                "show": "{n}",
                "ref": "{n}",
                "long": "{n}",
                "pure": "{n}",
            },
        }

    def show_ref_value(self, name, text="", showtype=""):
        from_macros = self.c.get(
            name, {"v": name, "s": name, "r": name, "p": name, "l": name}
        )
        p = showtype or "p"
        r = showtype or "r"
        if text:
            s = str(text) + " " + str(from_macros.get(p, ""))
        else:
            s = str(from_macros.get(r, ""))
        return "[" + s + "](#" + from_macros.get("h", name) + ")"

    def show_lref_value(self, name, text="", showtype="l"):
        return self.show_ref_value(name, text, showtype)

    def show_eqref_value(self, name, text=""):
        return self.show_ref_value(name, text)

    def get_counter_type(self, ctype: str):
        counter_type = self.counters.get(ctype)
        if not counter_type:  # first of this type
            counter_type = {"value": 0}
            self.counters[ctype] = counter_type
        return counter_type

    def add_counter(self, ctype: str, name: str, show_val: str, long_val: str = None):
        if self.renumbering:
            counter_type = self.get_counter_type(ctype)
            show = self.get_type_text(ctype, name, show_val, "chap")
            counter = {
                "v": show_val,
                "s": show,
                "r": show_val,
                "l": long_val,
                "p": show_val,
                "h": str(name),
                "n": str(name),
            }
            counter_type[name] = counter

    def new_counter(self, name, ctype):
        if self.renumbering:
            counter_type: dict = self.get_counter_type(ctype)
            counter: dict = counter_type.get(name)
            value = counter_type["value"] + 1
            counter_type["value"] = value
            if counter:  # shold not be
                counter["s"] = "Dublicate " + name
            else:
                show = self.get_type_text(ctype, name, value, "show")
                pure = self.get_type_text(ctype, name, value, "pure")
                ref = self.get_type_text(ctype, name, value, "ref")
                long = self.get_type_text(ctype, name, value, "long")
                counter = {
                    "v": value,  # just value, most numeric
                    "s": show,  # show in place of counter
                    "r": ref,  # normal refence format
                    "l": long,  # long format, mostly for section header
                    "p": pure,  # formated value
                    "h": LABEL_PRFIX + str(name),  # Jump address
                    "n": str(name),  # name of counter
                }
                counter_type[name] = counter
        from_macros = self.c.get(name, {"s": "?" + str(name) + "?"})
        return from_macros["s"]

    def new_label_counter(self, name, ctype):
        s = str(self.new_counter(name, ctype))
        if self.macros.get("tex"):
            return " \\label{" + LABEL_PRFIX + name + "}" + s
        return '<a id="' + LABEL_PRFIX + name + '"></a>' + s

    def eq_counter(self, name):
        return self.new_counter(name, "eq")

    def tag_counter(self, name):
        return "\\tag{" + str(self.new_counter(name, "eq")) + "}"

    def begin_counter(self, what, name):
        if self.macros.get("tex"):
            return (
                "\\begin{"
                + what
                + "}"
                + " \\label{"
                + LABEL_PRFIX
                + name
                + "}"
                + self.tag_counter(name)
            )
        return (
            '<a id="'
            + LABEL_PRFIX
            + name
            + '"></a>\\begin{'
            + what
            + "}"
            + self.tag_counter(name)
        )

    def fig_counter(self, name):
        return self.new_label_counter(name, "fig")

    def tbl_counter(self, name):
        return self.new_label_counter(name, "tbl")

    def ex_counter(self, name):
        return self.new_label_counter(name, "ex")

    def task_counter(self, name):
        return self.new_label_counter(name, "task")

    def get_counters(self, _dummy=0):
        result = "  c:\n"
        for ctype in self.counters:
            counter_type = self.counters[ctype]
            for name in counter_type:
                if name == "value":
                    continue
                counter = counter_type[name]
                jso = json.dumps(counter)
                result += "    " + name + ": " + jso + "\n"
        return result

    def set_counter(self, ctype: str, value: int):
        counter_type = self.counters.get(ctype)
        if not counter_type:
            return
        counter_type["value"] = value
        return ""

    def get_type_text(
        self,
        ctype: str,
        name: str,
        value: str,
        showtype: str,
    ) -> str:
        """
        Get how to show counter with type name ctype
        :param ctype: countres type name
        :param value: value of counter to show
        :param showtype: for what purpose value is formated
        :return: formated show value
        """
        vals = self.heading_vals
        if vals is None:
            vals = self.heading_vals_def
        autocounters = self.macros.get("autocounters", {})
        fmt_all = self.autocounters_fmt_def["all"]
        fmt_def = self.autocounters_fmt_def.get(ctype, fmt_all)

        counter_fmt = autocounters.get(ctype, fmt_def)
        if not counter_fmt:  # should not occur!
            return str(value)
        showformat = counter_fmt.get(showtype, fmt_all.get(showtype, None))
        s = showformat
        if s is None or not isinstance(s, str):
            return str(value)
        values = {"v": value, "n": name}
        add_h_values(vals, values)
        text = s.format(**values)
        return text

    def reset_counters(self, n: int):
        """
        Reset all counters that should be reset when n heading level n
        cahenges
        :param n: what heading level to check
        :return: None
        """
        # TODO: throw exception if error in counters
        autocounters = self.macros.get("autocounters", None)
        if not autocounters:
            return
        for counter_type_name in autocounters:
            counter = autocounters[counter_type_name]
            counter_type = self.counters.get(counter_type_name, None)
            if not counter or not counter_type:
                continue
            reset = counter.get("reset", None)
            if not reset or n != reset:
                continue
            counter_type["value"] = 0

    def set_heading_vals(self, vals: dict):
        oldvals = self.heading_vals
        if oldvals is None:
            oldvals = self.heading_vals_def
        self.heading_vals = vals
        if not oldvals:
            return
        for n in vals:
            if oldvals[n] != vals[n]:
                self.reset_counters(n)

    def set_env_filters(self, env: TimSandboxedEnvironment):
        env.filters["lref"] = self.show_lref_value
        env.filters["ref"] = self.show_ref_value
        env.filters["eqref"] = self.show_eqref_value
        env.filters["c_"] = self.new_counter
        env.filters["c_label"] = self.new_label_counter
        env.filters["c_eq"] = self.eq_counter
        env.filters["c_tag"] = self.tag_counter
        env.filters["c_begin"] = self.begin_counter
        env.filters["c_fig"] = self.fig_counter
        env.filters["c_tbl"] = self.tbl_counter
        env.filters["c_ex"] = self.ex_counter
        env.filters["c_task"] = self.task_counter
        env.filters["c_set"] = self.set_counter


tim_filters = {
    "Pz": Pz,
    "gfields": genfields,
    "gfrange": gfrange,
    "srange": srange,
    "now": now,
    "w2date": week_to_date,
    "m2w": month_to_week,
    "w2text": week_to_text,
    "fmtdate": fmt_date,
    "preinc": preinc,
    "postinc": postinc,
    "belongs": belongs_placeholder,
    "fmt": fmt,
    "docid": get_document_id,
    "docpath": get_document_path,
    "ref": belongs_placeholder,
    "c_": belongs_placeholder,
    "c_eq": belongs_placeholder,
    "counters": belongs_placeholder,
}


def create_environment(
    macro_delimiter: str,
    user_ctx: UserContext | None,
    view_ctx: ViewContext,
    macros: dict | None,
) -> TimSandboxedEnvironment:
    env = TimSandboxedEnvironment(macro_delimiter)
    env.filters.update(tim_filters)
    env.filters["isview"] = view_ctx.isview

    if macros:
        counters = Counters(macros)
        env.set_counters(counters)  # used in print.py

    if user_ctx:
        env.filters["belongs"] = Belongs(user_ctx).belongs_to_group
    return env


def md_to_html(
    text: str, sanitize: bool = True, macros: dict[str, object] | None = None
) -> str:
    """Converts the specified markdown text to HTML.

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
    )

    raw = call_dumbo([text])

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
    if texplain or textplain:  # add pre-markers to tex paragrpahs
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
    html_content: str, string_delimeter: str, editing_function
) -> str:
    # List of strings after splitting html from
    html_list = html_content.split(string_delimeter)
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


def insert_heading_numbers(
    html_str: str,
    heading_info,
    auto_number_headings: int or bool = True,
    heading_format: dict = None,
):
    """Applies the given heading_format to the HTML if it is a heading, based on the given heading_info. Additionally
    corrects the id attribute of the heading in case it has been used earlier.

    :param heading_info: A dict containing the heading information ('h': dict(int,int) of heading counts
           and 'headings': dict(str,int) of so-far used headings and their counts).
    :param html_str: The HTML string to be processed.
    :param auto_number_headings: Whether the headings should be formatted at all.
    :param heading_format: A dict(int,str) of the heading formats to be used.
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
        if hcount > 0:
            try:
                e.attrib["id"] += "-" + str(hcount)
            except KeyError:
                e.set("id", f"{curr_id}-{hcount}")
        if auto_number_headings:
            e.text = format_heading(e.text or "", int(e.tag[1]), counts, heading_format)
    final_html = etree.tostring(tree)
    return final_html


def add_h_values(counts: dict, values: dict):
    """
    Add all non-zero h-value from counts to values
    :param counts: where to finds h-values
    :param values: where to add h-values
    :return: None
    """
    for last_non_zero in range(6, 0, -1):
        if counts[last_non_zero] != 0:
            break
    # noinspection PyUnboundLocalVariable
    for i in range(1, last_non_zero + 1):
        values[HEADING_TAGS[i - 1]] = counts[i]


def format_heading(
    text,
    level,
    counts,
    heading_format,
    heading_ref_format: dict = None,
    jump_name: str = None,
    counters: Counters = None,
):
    counts[level] += 1
    for i in range(level + 1, 7):
        counts[i] = 0
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


HEADING_TAGS = ["h1", "h2", "h3", "h4", "h5", "h6"]
