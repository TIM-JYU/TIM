"""Provides functions for converting markdown-formatted text to HTML."""
from __future__ import annotations

import datetime
from dataclasses import dataclass
from datetime import date
from typing import Optional, Dict, TYPE_CHECKING, List, Iterable

from jinja2 import TemplateSyntaxError
from jinja2.sandbox import SandboxedEnvironment
from lxml import html, etree

from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, default_view_ctx
from timApp.document.yamlblock import YamlBlock
from timApp.markdown.dumboclient import call_dumbo
from timApp.markdown.htmlSanitize import sanitize_html
from timApp.util.utils import get_error_html, title_to_id
from timApp.util.utils import widen_fields

if TYPE_CHECKING:
    from timApp.document.docparagraph import DocParagraph
    from timApp.document.docsettings import DocSettings


def has_macros(text: str, env: SandboxedEnvironment):
    return env.variable_start_string in text or env.comment_start_string in text or env.block_start_string in text


# ------------------------ Jinja filters -------------------------------------------------------------------
# Ks. https://tim.jyu.fi/view/tim/ohjeita/satunnaistus#timfiltterit


# To create a new filter,
#  1. make a class or function
#  2. and map it in create_environment

def genfields(flds, attrs='', stemfield='stem'):
    """
    Generates fields from namelist like ['se1', 'd1', 'd2=demo2']
    See usescases from: /tim/timApp/tests/server/test_genfields.py
    :param flds: list of fields, maybe with aliases to show in stem
    :param attrs: possible list of attributes
    :param stemfield: field to use to show filed ste, like sten, header or inputstem
    :return: TIM-format of fields
    """
    flds = widen_fields(flds)
    res = ''
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


def gfrange(s, i1, i2, attrs='', stemfield='stem'):
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
    return genfields(s+srest, attrs, stemfield)


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
    result = ''
    cor = 1  # correct python range vs normal people range
    if step < 0:
        cor = -1
    if step == 0:
        step = 1
    for i in range(i1, i2+cor, step):
        ext = []
        for j in range(0, len(argv), 2):
            add = argv[j]
            mul = 1
            if j + 1 < len(argv):
                mul = argv[j+1]
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


def week_to_date(week_nr, daynr=1, year=None, fmt=None):
    """
    date object for week
    see: timApp/tests/unit/test_datefilters.py
    :param week_nr:  week number to get the date object
    :param daynr: day of week to get date
    :param year: year to get date
    :param fmt: extended format string
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
    if fmt == None:
        return d
    return fmt_date(d, fmt)




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
    d = datetime.date(year, month, daynr)
    return d.isocalendar()[1]


def fmt_date(d, fmt=""):
    """
    Format date using extended %d1 and %m1 for one number values
    see: timApp/tests/unit/test_datefilters.py
    :param d: date to format
    :param fmt: Python format
    :return: string from d an format
    """
    ds = "" + str(d.day)
    ms = "" + str(d.month)
    if fmt == "":
        return str(ds) + "." + str(ms)
    fmt = fmt.replace("%d1", ds).replace("%m1", ms)
    return d.strftime(fmt)


def week_to_text(week_nr, year=None, fmt=" %d1.%m1|", days="ma|ti|ke|to|pe|", first_day=1):
    """
    Convert week to clendar header format
    see: timApp/tests/unit/test_datefilters.py
    :param week_nr: what week to convert
    :param year: what year
    :param fmt: extended Python  date format
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
        ds = fmt_date(week_to_date(week_nr, daynr, year), fmt)
        s += days[beg:end] + ds
        beg = end + 1
        daynr += 1
        if daynr > 7:
            if first_empty: # starts with separator, we need the last
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
        settings: Optional[DocSettings],
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
                macrotext = "%%"+gmacro+"%%"
                pos = text.find(macrotext)
                if pos >= 0:
                    gm = str(globalmacros.get(gmacro, ""))
                    text = text.replace(macrotext, gm)
            gm = str(globalmacros.get("ADDFOREVERY", ''))
            if gm:
                text = gm + "\n" + text
        startstr = env.comment_start_string + "LOCAL"
        beg = text.find(startstr)
        if beg >= 0:
            endstr = env.comment_end_string
            end = text.find(endstr, beg)
            if end >= 0:
                local_macros_yaml = text[beg+len(startstr):end]
                local_macros = YamlBlock.from_markdown(local_macros_yaml).values
                macros = {**macros, **local_macros}
        conv = env.from_string(text).render(macros)
        return conv
    except TemplateSyntaxError as e:
        if not ignore_errors:
            return get_error_html(f'Syntax error in template: {e}')
        return text
    except Exception as e:
        if not ignore_errors:
            return get_error_html(f'Syntax error in template: {e}')
        return text


tim_filters = {
    'Pz': Pz,
    'gfields': genfields,
    'gfrange': gfrange,
    'srange': srange,
    'w2date': week_to_date,
    'm2w': month_to_week,
    'w2text': week_to_text,
    'fmtdate': fmt_date,
    'preinc': preinc,
    'postinc': postinc,
}


def create_environment(
        macro_delimiter: str,
        user_ctx: Optional[UserContext],
        view_ctx: ViewContext,
) -> SandboxedEnvironment:
    env = SandboxedEnvironment(
        variable_start_string=macro_delimiter,
        variable_end_string=macro_delimiter,
        comment_start_string='{!!!',
        comment_end_string='!!!}',
        block_start_string='{%',
        block_end_string='%}',
        lstrip_blocks=True,
        trim_blocks=True,
    )
    env.filters.update(tim_filters)
    env.filters['isview'] = view_ctx.isview

    if user_ctx:
        env.filters['belongs'] = Belongs(user_ctx).belongs_to_group
    return env


def md_to_html(text: str,
               sanitize: bool = True,
               macros: Optional[Dict[str, object]] = None) -> str:
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
        env=create_environment('%%', user_ctx=None, view_ctx=default_view_ctx),
    )

    raw = call_dumbo([text])

    if sanitize:
        return sanitize_html(raw[0])
    else:
        return raw[0]


def par_list_to_html_list(
        pars: List[DocParagraph],
        settings: DocSettings,
        view_ctx: ViewContext,
        auto_macros: Optional[Iterable[dict]] = None,
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
    texts = [p.get_expanded_markdown(macroinfo) if not p.has_dumbo_options() else {
        'content': p.get_expanded_markdown(macroinfo),
        **p.get_dumbo_options(base_opts=dumbo_opts).dict(),
    } for p in pars]

    texplain = settings.is_texplain()
    textplain = settings.is_textplain()
    if texplain or textplain:  # add pre-markers to tex paragrpahs
        for i in range(0, len(texts)):
            text = texts[i]
            if text.find('```') != 0 and text.find('#') != 0:
                texts[i] = '```\n' + text + "\n```"
    raw = call_dumbo(texts, options=dumbo_opts)

    # Edit html after dumbo
    raw = edit_html_with_own_syntax(raw)

    if auto_macros:
        processed = []
        for pre_html, m, attrs in zip(raw, auto_macros, (p.get_attrs() for p in pars)):
            if 'nonumber' in attrs.get('classes', {}):
                final_html = pre_html
            else:
                final_html = insert_heading_numbers(pre_html, m, settings.auto_number_headings(),
                                                    settings.heading_format())
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
        new_html = check_and_edit_html_if_surrounded_with(html_text, "§§", change_classes_to_fragment)
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
                    fragments[index], "§§", change_classes_to_fragment)
            else:
                # Make a new fragment area if start and end found
                fragments[index] = '</p><div class="fragment"><p>' + fragments[index]
                fragments[index] = fragments[index].replace("§&gt;", "</p></div><p>", 1)
                # Look for inner fragments
                fragments[index] = check_and_edit_html_if_surrounded_with(
                    fragments[index], "§§", change_classes_to_fragment)
            index += 1
        new_html = "".join(fragments)
        return new_html


# Checks if html element's content is surrounded with given string and edits it accordingly
def check_and_edit_html_if_surrounded_with(html_content: str, string_delimeter: str, editing_function) -> str:
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


def change_class(text_containing_html_tag: str, text_content: str, new_class: str) -> list:
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
                text_containing_html_tag = text_containing_html_tag[:(
                    index_of_tag_start + index_of_class + 7)] + new_class + " " + text_containing_html_tag[(
                        index_of_tag_start + index_of_class + 7):]
            else:
                # If there isn't class in html tag we add that and the new class
                text_containing_html_tag = text_containing_html_tag[
                    :index_of_tag_end] + ' class="' + new_class + '"' + text_containing_html_tag[
                    index_of_tag_end:]
        else:
            text_content = '<span class="' + new_class + '">' + text_content + '</span>'
    # If there is an error we do nothing but return the original text
    except ValueError:
        pass
    return [text_containing_html_tag, text_content]


def insert_heading_numbers(html_str: str, heading_info, auto_number_headings: bool = True, heading_format: str = ''):
    """Applies the given heading_format to the HTML if it is a heading, based on the given heading_info. Additionally
    corrects the id attribute of the heading in case it has been used earlier.

    :param heading_info: A dict containing the heading information ('h': dict(int,int) of heading counts
           and 'headings': dict(str,int) of so-far used headings and their counts).
    :param html_str: The HTML string to be processed.
    :param auto_number_headings: Whether the headings should be formatted at all.
    :param heading_format: A dict(int,str) of the heading formats to be used.
    :return: The HTML with the formatted headings.

    """
    tree = html.fragment_fromstring(html_str, create_parent=True)
    counts = heading_info['h']
    used = heading_info['headings']
    for e in tree.iterchildren():
        is_heading = e.tag in HEADING_TAGS
        if not is_heading:
            continue
        curr_id = title_to_id(e.text)
        hcount = used.get(curr_id, 0)
        if hcount > 0:
            try:
                e.attrib['id'] += '-' + str(hcount)
            except KeyError:
                e.set('id', f'{curr_id}-{hcount}')
        if auto_number_headings:
            e.text = format_heading(e.text or '', int(e.tag[1]), counts, heading_format)
    final_html = etree.tostring(tree)
    return final_html


def format_heading(text, level, counts, heading_format):
    counts[level] += 1
    for i in range(level + 1, 7):
        counts[i] = 0
    for i in range(6, 0, -1):
        if counts[i] != 0:
            break
    values = {'text': text}
    # noinspection PyUnboundLocalVariable
    for i in range(1, i + 1):
        values['h' + str(i)] = counts[i]
    try:
        formatted = heading_format[level].format(**values)
    except (KeyError, ValueError, IndexError):
        formatted = '[ERROR] ' + text
    return formatted


HEADING_TAGS = {'h1', 'h2', 'h3', 'h4', 'h5', 'h6'}
