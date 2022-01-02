from __future__ import annotations

import json
from dataclasses import dataclass, field
from typing import Optional, Union

import attr
from jinja2.sandbox import SandboxedEnvironment

"""
Class for autocounters to be used as Jinja2 filters
"""

LABEL_PRFIX = "c:"

AUTOCOUNTERS_FMT_DEF: dict[str, dict[str, Union[int, str]]] = {
    "eq": {"ref": "({p})", "long": "({p})", "text": "({p})", "prefix": "eq:"},
    "all": {
        "reset": -1,
        "pure": "{v}",
        "show": "{p}",
        "ref": "{p}",
        "long": "{p}",
        "text": "{p}",
        "prefix": "c:",
    },
}

TName = Union[str, int]
TAttrib = Union[str, int]
TCounter = dict[str, TAttrib]
TMacroCounter = dict[str, TAttrib]
TMacroCounters = dict[str, TMacroCounter]
TAutoNames = dict[str, str]


@dataclass
class TOneCounterType:
    count: int = 0
    counters: dict[str, TCounter] = field(default_factory=dict)


TCounters = dict[str, TOneCounterType]


@attr.s(auto_attribs=True, init=False)
class AutoCounters:
    renumbering: bool = False
    heading_vals: Optional[dict] = None
    heading_vals_def: dict[int, int] = attr.Factory(
        lambda: {
            1: 0,
            2: 0,
            3: 0,
            4: 0,
            5: 0,
            6: 0,
        }
    )
    pure_reset_formats = attr.Factory(
        lambda: ["{v}", "{v}", "{v}", "{v}", "{v}", "{v}", "{v}"]
    )
    counter_stack: list[str] = attr.Factory(list)
    autocnts: TMacroCounters = attr.Factory(dict)
    autonames: TAutoNames = attr.Factory(dict)
    new_autonames: TAutoNames = attr.Factory(dict)
    tex: bool = False
    auto_number_headings: int = 0

    # counters for generating counter names automativally
    auto_name_base: Optional[str] = None
    auto_name_counter: int = 0
    auto_name_ctype: str = "eq"
    need_auto_name_reset: bool = True
    auto_name_plugin: bool = False

    doing_latex_environment = False

    # counters for label cache to be replaced after block is ready
    # needed if in LaTeX environment no base name is given
    auto_labels: bool = False
    label_cache: list[list[str]] = attr.Factory(list)
    need_label_update = False  # total counter for cached labels (sum of in label_cache)
    current_labels: list[str] = attr.Factory(list)

    def __init__(self, macros: Optional[dict]):
        """
        Initialize autonumber counters to use macros
        :param macros: macros to use for these counters
        """
        # noinspection PyUnresolvedReferences
        self.__attrs_init__()  # type: ignore[attr-defined]
        self.counters: TCounters = {}
        self.macros: dict
        self.task_id: Optional[str] = None
        if macros:
            self.macros = macros
            self.autocnts = macros.get("autocnts", {})
            if self.autocnts is None:
                self.autocnts = {}
            self.autonames = macros.get("autonames", {})
            self.tex = macros.get("tex", False)
        else:
            self.macros = {}

        self.use_autonumbering: bool = self.macros.get("use_autonumbering", False)
        # TODO: check macros if there is counters_charmacros
        self.charmacros: dict[str, str] = {
            "§n": "%%0|c_%%",
            "§\\": "%%0|c_tag%%\\\\",
            "{§": '%%"',
            "§}": '"|c_%%',
        }

    def do_char_macros(self, text: str) -> str:
        """
        Do counters charmacros
        :param text: what to convert
        :return: converted text
        """
        if not self.use_autonumbering:
            return text
        i = text.find("§")  # TODO: use attribute that can be changed
        if i < 0:
            return text
        for cm_key, cm_value in self.charmacros.items():
            text = text.replace(cm_key, cm_value)
        return text

    def set_auto_name(self, base_name: str) -> str:
        """
        Set start of autonames
        :param base_name: base name for counters in this block
        :param ctype: default type for counters in this block
        :return: emtpy string because used from filter
        """
        self.auto_name_base = base_name
        if base_name:
            self.need_auto_name_reset = False
        return ""

    def set_auto_names(self, base_name: Optional[TName], ctype: str = "eq") -> str:
        """
        Set start of autonames
        :param base_name: base name for counters in this block
        :param ctype: default type for counters in this block
        :return: emtpy string because used from filter
        """
        if isinstance(base_name, int):
            base_name = None
        self.auto_name_base = base_name
        self.auto_name_ctype = ctype
        self.auto_name_counter = 0
        if base_name:
            self.need_auto_name_reset = False
        return ""

    def start_of_block(self) -> None:
        """
        Do things needed to know when convertiong of new block starts
        Can autoname and cache labels in one block.
        Remember to do reset_label_cache when used first time
        Also reset auto names before first use
        :return: None
        """
        self.auto_labels: bool = False
        # self.need_auto_name_reset = True
        self.set_auto_names(None)
        self.doing_latex_environment = False
        self.reset_label_cache()

    def reset_label_cache(self) -> None:
        """
        Reset label cache so that new label list's can start
        :return: None
        """
        self.need_label_update = False
        self.label_cache = []
        self.current_labels = []

    def get_base_name(self, name: str) -> str:
        """
        Returns basename that is udes for name
        :param name: name to use
        :return: basename
        """
        if self.auto_name_base:
            return self.auto_name_base
        if self.task_id:
            return self.task_id
        return name

    def get_auto_name(self, name: str, ctype: str) -> tuple[str, str, Optional[str]]:
        """
        Get automatic name and ctype if not given
        :param name: if empty, give autoname
        :param ctype: if empty give ctype from auto name
        :return: name, ctype, error
        """
        if self.need_auto_name_reset:
            self.set_auto_names(None)
        self.auto_name_plugin: bool = False
        self.auto_name_counter += 1
        if name:
            if not ctype:
                ctype = self.auto_name_ctype
            return name, ctype, None

        if not ctype:
            if self.task_id:
                ctype = "task"
            else:
                ctype = self.auto_name_ctype
        if not self.auto_name_base:
            if self.task_id is None:
                return "", "", self.error("Missing base name, use c_auto")
            self.auto_name_plugin: bool = True
            sname = self.task_id
            self.set_auto_name(sname)
            return sname, ctype, None
        return self.auto_name_base + str(self.auto_name_counter), ctype, None

    def set_auto_number_headings(self, n: int) -> None:
        """
        Set from what level the headings are numbered.
        Make also the dafault counter number template for that level
        like "{h2}.{v}" if counting start from level 2.
        :param n: from what level to start heading counting
        :return: none
        """
        self.auto_number_headings = n
        if n < 1:
            return

        self.pure_reset_formats = ["{v}"]
        for reset in range(1, 6 + 1):
            pfmt = ""
            for i in range(self.auto_number_headings, reset + 1):
                pfmt += f"{{h{i}}}."
            pfmt += "{v}"
            self.pure_reset_formats.append(pfmt)

    def error(self, s: str) -> str:
        """
        return string as md red
        :param s: string to show as red
        :return: s surrounded be []{.red}
        """
        if self.doing_latex_environment:
            return s.replace("_", "\\_")
        return f"[{s}]{{.red}}"

    @staticmethod
    def get_texts_and_name(name: TName) -> tuple[str, str, str]:
        """
        Separate from "t1|name|t2" t1, name and t2.
        If like "name" jsu return "", name, t2
        :param name: counter name where pre and post texts are separated
        :return: t1, name, t2
        """
        sname = str(name) if name else ""
        text1 = ""
        text2 = ""
        if sname.find("|") >= 0:
            params = sname.split("|")
            sname = params[1].strip()
            text1 = params[0].lstrip()
            if len(params) > 2:
                text2 = params[2]
        return text1, sname, text2

    def get_show_params(
        self, name: TName, showtype: str = "r"
    ) -> tuple[str, str, TMacroCounter]:
        """
        return  string name, text to show for chosen showtype and macros for sname
        :param name: name to separate t1, name nand t2
        :param showtype: how to show counter with name
        :return: sname, text for counter, macros for counter
        """
        sname = str(name)
        text1 = ""
        text2 = ""
        if sname.find("|") >= 0:
            params = sname.split("|")
            sname = params[1].strip()
            text1 = params[0].strip()
            if len(params) > 2:
                text2 = params[2]
            if len(params) > 3 and not showtype:
                showtype = params[3]
            if text2 and not showtype:
                showtype = "t"

        from_macros = self.autocnts.get(
            sname,
            {"v": sname, "s": sname, "r": sname, "p": sname, "l": sname, "t": sname},
        )
        t = showtype or "t"
        r = showtype or "r"

        if text1:
            s = str(text1) + "&nbsp;" + str(from_macros.get(t, ""))
        else:
            s = str(from_macros.get(r, ""))
        if text2:
            s += text2.replace(" ", "&nbsp;", 1)
        return sname, s, from_macros

    def show_pref_value(self, name: TName, showtype: str = "") -> str:
        """
        return pure reference for counter name (without jump link)
        :param name: counter's name
        :param showtype: how to show counter
        :return: string for reference
        """
        _, s, _ = self.get_show_params(name, showtype)
        return s

    def show_ref_value(self, name: TName, showtype: str = "") -> str:
        """
        return reference to counter using hyper link
        :param name: counter's name
        :param showtype: how to show counter
        :return: string for reference
        """
        sname, s, from_macros = self.get_show_params(name, showtype)
        anchor = from_macros.get("h", sname)
        return f"[{s}](#{anchor})"

    def show_lref_value(self, name: TName, showtype: str = "l") -> str:
        """
        return long reference to counter using hyper link
        :param name: counter's name
        :param showtype: how to show counter
        :return: string for reference
        """
        return self.show_ref_value(name, showtype)

    def get_counter_type(self, ctype: str) -> TOneCounterType:
        """
        Get type couter, so with value of how many has been totally
        during the whole document.  If this is first call for ctype,
        create new type counter.
        :param ctype: counter's type
        :return: type counter with value-
        """
        counters_type = self.counters.get(ctype)
        if not counters_type:  # first of this type
            counters_type = TOneCounterType()
            self.counters[ctype] = counters_type
        return counters_type

    def add_counter(
        self, ctype: str, name: str, show_val: str, long_val: str = ""
    ) -> Optional[dict]:
        """
        Used to add chapter and paragraph counters.
        :param ctype: usually "chap"
        :param name: counters name
        :param show_val: value like 2.4
        :param long_val: long for with tilte like 2.4 Counters
        :return: None
        """
        if self.renumbering:
            counter_type = self.get_counter_type(ctype)
            show = self.get_type_text(ctype, name, show_val, "chap", str(show_val))
            counter: TCounter = {
                "v": show_val,
                "p": show_val,
                "s": show,
                "r": show_val,
                "t": show_val,
                "l": long_val,
                "h": str(name),
                "n": str(name),
                "y": ctype,
            }
            counter_type.counters[name] = counter
            return counter
        return None

    def create_new_counter(
        self, name: TName, ctype: str
    ) -> tuple[str, str, TMacroCounter]:
        """
        Create new counter as text that has name and ctype
        :param name: counters name, can include pre text t1 and post text t2
        :param ctype: counters type
        :return: counter as text and name
        """
        text1, orig_name, text2 = self.get_texts_and_name(name)
        sname, ctype, error = self.get_auto_name(orig_name, ctype)
        if error:
            return error, sname, {}
        if self.renumbering:
            counters_type: TOneCounterType = self.get_counter_type(ctype)
            use_counters_type = counters_type

            link = self.macros.get("autocounters", {}).get(ctype, {}).get("link", None)
            if link is not None:
                use_counters_type = self.get_counter_type(link)

            counter: Optional[TCounter] = use_counters_type.counters.get(sname)
            use_counters_type.count += 1
            value: int = use_counters_type.count
            if counter:  # shold not be
                counter["s"] = self.error("Duplicate " + sname)
            else:
                pure = self.get_type_text(ctype, sname, value, "pure", str(value))
                show = (
                    text1
                    + self.get_type_text(ctype, sname, value, "show", pure)
                    + text2
                )
                ref = self.get_type_text(ctype, sname, value, "ref", pure)
                text = self.get_type_text(ctype, sname, value, "text", pure)
                long = self.get_type_text(ctype, sname, value, "long", show)
                prefix = self.get_type_text(ctype, sname, value, "prefix", LABEL_PRFIX)
                hname = sname
                if self.doing_latex_environment and not self.auto_labels:
                    # in case of latex env we use the base name
                    hname = self.get_base_name(orig_name)
                if self.auto_name_plugin:
                    prefix = ""
                counter = {
                    "v": value,  # just value, mostly numeric
                    "p": pure,  # formated value
                    "s": show,  # show in place of counter
                    "r": ref,  # normal refence format
                    "t": text,  # with text
                    "l": long,  # long format, mostly for section header
                    "h": prefix + hname.replace(" ", "_"),  # Jump address
                    "n": sname,  # name of counter
                    "y": ctype,
                }
                counters_type.counters[sname] = counter
        from_macros = self.autocnts.get(sname, {"s": self.error(f"?{sname}?")})
        if self.auto_labels:
            anchor = from_macros.get("h", "")
            if anchor:
                self.current_labels.append(sname)
        return str(from_macros["s"]), sname, from_macros

    def new_counter(self, name: TName, ctype: str = "") -> str:
        """
        For filter c_n
        :param name: counter's name
        :param ctype: counter's type
        :return: counter as text
        """
        s, _, _ = self.create_new_counter(name, ctype)
        return s

    def new_label_counter(self, name: TName, ctype: str = "") -> str:
        """
        For filter c_
        :param name: counter's name can be also "text|name"
        :param ctype: counter's type
        :return: counter as text with label where to jump
        """
        if self.doing_latex_environment:
            return self.tag_counter(name, ctype)
        s, name, from_macros = self.create_new_counter(name, ctype)
        hyper_jmp = from_macros.get("h", "")
        if self.auto_name_plugin:
            return s
        if self.tex:
            return f" \\label{{{hyper_jmp}}}{s}"
        return f'<a id="{hyper_jmp}"></a>{s}'

    def labels(self, names: list) -> str:
        """
        For filter labels
        Creates a a-tag or LaTeX label list from counter names.
        :param names: list of counter names that are converted to labels
        :return: string to output either a-tag's or LaTeX labels
        """
        result = ""
        for name in names:
            hname = self.autocnts.get(name, {})
            label = hname.get("h", None)
            if label is None:
                continue
            if self.tex:
                result += f" \\label{{{label}}}"
            result += f'<a id="{label}"></a>'
        return result

    def eq_counter(self, name: TName) -> str:
        """
        For filter c_eq, same as   "name" | c_n(eq")
        :param name: counter's name
        :return:
        """
        return self.new_counter(name, "eq")

    def tag_counter(self, name: TName, ctype: str = "eq") -> str:
        """
        For filter c_tag
        Counter inside LaTeX \tag{}
        :param name: counter's name
        :param ctype: type for the counter, default for eq
        :return:  tag counter
        """
        return f"\\tag{{{self.new_counter(name, ctype)}}}"

    @staticmethod
    def label_place_holder(n: int) -> str:
        """
        Placeholder for labels that should come before \begin
        :param n: What is the number of this olaceholder in this block
        :return: string like <!-- LABEL002 -->
        """
        return f"<!–– LABELS{n:03} -->"

    def update_labels(self, text: str) -> str:
        """
        Replace label placeholders by actual labels.
        This should be called when block is totally converted.
        :param text: block text where to replace labels
        :return: text with labels inserted
        """
        for n, lbls in enumerate(self.label_cache):
            # optimize not using replace
            ph = self.label_place_holder(n)
            text = text.replace(ph, self.labels(lbls))
        self.reset_label_cache()
        self.auto_labels = False
        return text

    def get_label_placeholder(self) -> str:
        """
        returns next labels placeholder if there is labels
        that can not be anchored to begining of the LaTeX environment
        :return: placeholder for label that is replaced by
                 real labes after whole block is ready
        """
        self.need_label_update = True
        self.auto_labels = True
        self.current_labels = []
        self.label_cache.append(self.current_labels)
        return self.label_place_holder(len(self.label_cache) - 1)

    def make_latex_envoronment_begin(
        self, name: TName, what: str = "align*", ctype: str = "eq"
    ) -> str:
        """
        Crete LaTeX environment begin with label before
        :param name: base name for equations
        :param what: what LaTeX environment to start
        :param ctype: type for counters
        :return:
        """
        self.doing_latex_environment = True
        t1, sname, t2 = self.get_texts_and_name(name)
        if not sname:
            self.auto_name_ctype = ctype
        else:
            self.set_auto_names(sname, ctype)
        self.counter_stack.append(what)
        if self.renumbering:
            if sname:
                prefix = self.get_type_text(ctype, sname, 0, "prefix", LABEL_PRFIX)
                anchor = prefix + sname
                self.new_autonames[sname] = anchor
            else:
                self.auto_labels = True
            return ""
        anchor = self.autonames.get(sname, "")
        if self.tex:
            if anchor:
                label = f" \\label{{{anchor}}}"
            else:
                label = self.get_label_placeholder()
            return f"\\begin{{{what}}}{label}"
        if anchor:
            label = f'<a id="{anchor}"></a>'
        else:
            label = self.get_label_placeholder()
        return f"{label}\\begin{{{what}}}"

    def begin1_counter(
        self, name: TName, what: str = "align*", ctype: str = "eq"
    ) -> str:
        """
        For filter c_begin1
        Creates one label, LaTeX environment begin
        and counter for first equation
        :param name: name + base name for autoname
        :param what: what LaTeX environment to start
        :param ctype: what is default type for counters
        :return: label, LaTeX begin commands and one counter
        """
        label_and_begin = self.make_latex_envoronment_begin(name, what, ctype)
        cnt = ""
        if name:
            cnt = self.tag_counter(name, ctype)
        return label_and_begin + cnt

    def begin_counter(
        self, name: TName, what: str = "align*", ctype: str = "eq"
    ) -> str:
        """
        For filter c_begin
        Cretes placfeholder for labels and LaTeX environment begin
        :param name: base name for autoname
        :param what: what LaTeX environment to start
        :param ctype: what is default type for counters
        :return: label's placeholder and LaTeX begin commands
        """
        return self.make_latex_envoronment_begin(name, what, ctype)

    def end_counter(self, _dummy: str = "") -> str:
        """
        For filter c_end
        End last started LaTeX \begin command
        Move last used labels to cache
        :param _dummy: this filter has no parameters
        :return: LaTeX environment end command
        """
        self.doing_latex_environment = False
        self.auto_labels = False
        if len(self.counter_stack) == 0:
            return ""
        what = self.counter_stack.pop()
        return "\\end{" + what + "}"

    def fig_counter(self, name: TName) -> str:
        return self.new_label_counter(name, "fig")

    def tbl_counter(self, name: TName) -> str:
        return self.new_label_counter(name, "tbl")

    def ex_counter(self, name: TName) -> str:
        return self.new_label_counter(name, "ex")

    def task_counter(self, name: TName) -> str:
        return self.new_label_counter(name, "task")

    def get_counter_macros(self, _dummy: TName = 0) -> str:
        """
        Return counter values as string to be appended to settings
        :param _dummy:  if used as filter
        :return: counter values as settigs macro
        """
        result = """
macros:        
  use_autonumbering: true
  autocnts:
"""
        for ctype in self.counters:
            counter_type = self.counters[ctype]
            for name in counter_type.counters:
                counter = counter_type.counters[name]
                jso = json.dumps(counter)
                result += "    " + name + ": " + jso + "\n"
        result += "  autonames:"
        jso = json.dumps(self.new_autonames)
        result += "   " + jso + "\n"
        return result

    def set_counter(self, value: int, ctype: str) -> str:
        """
        For filter c_set
        Set's the type counter value
        :param value: new value
        :param ctype: for what type of counters
        :return: ""
        """
        counter_type = self.counters.get(ctype)
        if not counter_type:
            return ""
        counter_type.count = value
        return ""

    def get_counter_value(self, ctype: str) -> int:
        """
        For filter c_get
        Get's the value of counter while renumbering
        :param ctype: for what type of counters
        :return: 0 in view, but value of counter in renumbering
        """
        counter_type = self.counters.get(ctype)
        if not counter_type:
            return 0
        return counter_type.count

    def set_renumbering(self, value: bool) -> None:
        """
        This should be called from print command that generates new values
        :param value: is renumebring true or false
        :return: None
        """
        self.renumbering = value
        self.macros["use_autonumbering"] = value
        self.use_autonumbering = value

    def get_type_text(
        self, ctype: str, name: str, value: Union[int, str], showtype: str, pure: str
    ) -> str:
        """
        Get how to show counter with type name ctype
        :param ctype: countres type name
        :param name: name of counter
        :param value: value of counter to show
        :param showtype: for what purpose value is formated
        :param pure: pure value of counter
        :return: formated show value
        """
        vals = self.heading_vals
        if vals is None:
            vals = self.heading_vals_def
        autocounters = self.macros.get("autocounters", {})

        fmt_def = (
            AUTOCOUNTERS_FMT_DEF["all"]
            | AUTOCOUNTERS_FMT_DEF.get(ctype, {})
            | autocounters.get("all", {})
        )

        counter_fmt = autocounters.get(ctype, {})
        counter_fmt = fmt_def | counter_fmt

        reset = counter_fmt.get("reset", 0)
        if reset > 0:
            counter_type = autocounters.get(ctype, autocounters.get("all", {}))
            pfmt = counter_type.get("pure", self.pure_reset_formats[reset])
            counter_fmt["pure"] = pfmt

        # showformat = counter_fmt.get(showtype, fmt_all.get(showtype, None))
        showformat = counter_fmt.get(showtype, None)
        s = showformat
        if s is None or not isinstance(s, str):
            return str(value)
        values = {"v": value, "n": name, "p": pure}
        add_h_values(vals, values)
        try:
            text = s.format(**values)
            return text
        except Exception as ex:
            return self.error(str(ex) + " " + s)

    def reset_counters(self, n: int) -> None:
        """
        Reset all counters that should be reset when heading level n
        changes
        :param n: what heading level to check
        :return: None
        """
        # TODO: throw exception if error in counters
        if not self.macros:
            return
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
            counter_type.count = 0

    def set_heading_vals(self, vals: dict) -> None:
        """
        This should be called every time when handling heading line.
        Check whta counters should be reseted when heading numebrs changes
        :param vals: new values for current heading numbers
        :return: None
        """
        oldvals = self.heading_vals
        if oldvals is None:
            oldvals = self.heading_vals_def
        self.heading_vals = vals
        if not oldvals:
            return
        for n in vals:
            if oldvals[n] != vals[n]:
                self.reset_counters(n)

    def set_env_filters(self, env: SandboxedEnvironment) -> None:
        """
        Add new filters to environment
        :param env: to what environment to add values
        :return: None
        """
        if self.macros and self.use_autonumbering:
            env.filters["c_auto"] = self.set_auto_names
            env.filters["lref"] = self.show_lref_value
            env.filters["ref"] = self.show_ref_value
            env.filters["pref"] = self.show_pref_value
            env.filters["labels"] = self.labels
            env.filters["c_"] = self.new_label_counter
            env.filters["c_n"] = self.new_counter
            env.filters["c_eq"] = self.eq_counter
            env.filters["c_tag"] = self.tag_counter
            env.filters["c_begin"] = self.begin_counter
            env.filters["c_begin1"] = self.begin1_counter
            env.filters["c_end"] = self.end_counter
            env.filters["c_fig"] = self.fig_counter
            env.filters["c_tbl"] = self.tbl_counter
            env.filters["c_ex"] = self.ex_counter
            env.filters["c_task"] = self.task_counter
            env.filters["c_set"] = self.set_counter
            env.filters["c_get"] = self.get_counter_value


def check_autonumber_error(m: str) -> str | None:
    if m.find("ref") >= 0 or m.find("labels") >= 0 or m.find("c_") >= 0:
        return f"{m}: Run Refresh numbering under Cogwheel first!"
    return None


class TimSandboxedEnvironment(SandboxedEnvironment):
    """
    Environment to replace Jinja2 environment.
    Add auto counters to environment
    """

    def __init__(self, macro_delimiter: str = "%%", autoescape: bool = False) -> None:
        super().__init__(
            variable_start_string=macro_delimiter,
            variable_end_string=macro_delimiter,
            comment_start_string="{!!!",
            comment_end_string="!!!}",
            block_start_string="{%",
            block_end_string="%}",
            lstrip_blocks=True,
            trim_blocks=True,
            autoescape=autoescape,
        )
        self.counters: Optional[AutoCounters] = None

    def set_counters(self, counters: AutoCounters) -> None:
        self.counters = counters
        counters.set_env_filters(self)

    def get_counters(self) -> Optional[AutoCounters]:
        return self.counters


HEADING_TAGS = ["h1", "h2", "h3", "h4", "h5", "h6"]


def add_h_values(counts: dict, values: dict) -> None:
    """
    Add all non-zero h-value from counts to values
    :param counts: where to finds h-values
    :param values: where to add h-values
    :return: None
    """
    """
    for last_non_zero in range(6, 0, -1):
        if counts[last_non_zero] != 0:
            break
    """
    # noinspection PyUnboundLocalVariable
    # for i in range(1, last_non_zero + 1):

    for i in range(1, len(HEADING_TAGS) + 1):
        values[HEADING_TAGS[i - 1]] = counts[i]
