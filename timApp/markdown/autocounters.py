import json
from jinja2.sandbox import SandboxedEnvironment

"""
Class for autocounters to be used as Jinja2 filters
"""

LABEL_PRFIX = "c:"


class AutoCounters:
    def __init__(self, macros: dict or None):
        """
        Initialize autonumber counters to use macros
        :param macros: macros to use for these counters
        """
        self.renumbering = False
        self.heading_vals: dict or None = None
        self.heading_vals_def = {
            1: 0,
            2: 0,
            3: 0,
            4: 0,
            5: 0,
            6: 0,
        }
        self.macros = macros
        self.counters = {}
        self.counter_stack = []
        self.c = {}
        self.tex = False
        self.auto_number_headings = 0
        self.pure_reset_formats = ["{v}", "{v}", "{v}", "{v}", "{v}", "{v}", "{v}"]
        if self.macros:
            self.c = self.macros.get("c", {})
            self.tex = self.macros.get("tex", False)

        self.autocounters_fmt_def = {
            "eq": {
                "ref": "({p})",
                "long": "({p})",
                "text": "({p})",
            },
            "chap": {
                "reset": -1,
            },
            "all": {
                "reset": -1,
                "pure": "{v}",
                "show": "{p}",
                "ref": "{p}",
                "long": "{p}",
                "text": "{p}",
            },
        }

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
                pfmt += "{h" + str(i) + "}."
            pfmt += "{v}"
            self.pure_reset_formats.append(pfmt)

    def show_pref_value(self, name: str or int, text="", showtype="r") -> str:
        from_macros = self.c.get(
            name, {"v": name, "s": name, "r": name, "p": name, "l": name, "t": name}
        )
        t = showtype or "t"
        r = showtype or "r"
        if text:
            s = str(text) + "&nbsp;" + str(from_macros.get(t, ""))
        else:
            s = str(from_macros.get(r, ""))
        return s

    def show_ref_value(
        self, name: str or int, text: str = "", showtype: str = ""
    ) -> str:
        from_macros = self.c.get(
            name, {"v": name, "s": name, "r": name, "p": name, "l": name, "t": name}
        )
        t = showtype or "t"
        r = showtype or "r"
        if text:
            s = str(text) + "&nbsp;" + str(from_macros.get(t, ""))
        else:
            s = str(from_macros.get(r, ""))
        return "[" + s + "](#" + from_macros.get("h", name) + ")"

    def show_lref_value(self, name: str or int, text="", showtype="l") -> str:
        return self.show_ref_value(name, text, showtype)

    def get_counter_type(self, ctype: str) -> dict:
        counter_type = self.counters.get(ctype)
        if not counter_type:  # first of this type
            counter_type = {"value": 0}
            self.counters[ctype] = counter_type
        return counter_type

    def add_counter(
        self, ctype: str, name: str, show_val: str, long_val: str = ""
    ) -> dict or None:
        if self.renumbering:
            counter_type = self.get_counter_type(ctype)
            show = self.get_type_text(ctype, name, show_val, "chap", str(show_val))
            counter = {
                "v": show_val,
                "p": show_val,
                "s": show,
                "r": show_val,
                "t": show_val,
                "l": long_val,
                "h": str(name),
                "n": str(name),
            }
            counter_type[name] = counter
            return counter
        return None

    def new_counter(self, name: int or str, ctype: str) -> str:
        if self.renumbering:
            counter_type: dict = self.get_counter_type(ctype)
            counter: dict = counter_type.get(name)
            value = counter_type["value"] + 1
            counter_type["value"] = value
            if counter:  # shold not be
                counter["s"] = "Dublicate " + name
            else:
                pure = self.get_type_text(ctype, name, value, "pure", str(value))
                show = self.get_type_text(ctype, name, value, "show", pure)
                ref = self.get_type_text(ctype, name, value, "ref", pure)
                text = self.get_type_text(ctype, name, value, "text", pure)
                long = self.get_type_text(ctype, name, value, "long", pure)
                counter = {
                    "v": value,  # just value, mostly numeric
                    "p": pure,  # formated value
                    "s": show,  # show in place of counter
                    "r": ref,  # normal refence format
                    "t": text,  # with text
                    "l": long,  # long format, mostly for section header
                    "h": LABEL_PRFIX + str(name),  # Jump address
                    "n": str(name),  # name of counter
                }
                counter_type[name] = counter
        from_macros = self.c.get(name, {"s": "?" + str(name) + "?"})
        return from_macros["s"]

    def new_label_counter(self, name: int or str, ctype: str) -> str:
        s = str(self.new_counter(name, ctype))
        if self.tex:
            return " \\label{" + LABEL_PRFIX + name + "}" + s
        return '<a id="' + LABEL_PRFIX + name + '"></a>' + s

    def labels(self, names: list) -> str:
        result = ""
        for name in names:
            if self.tex:
                result += " \\label{" + LABEL_PRFIX + name + "}"
            result += '<a id="' + LABEL_PRFIX + name + '"></a>'
        return result

    def eq_counter(self, name: int or str) -> str:
        return self.new_counter(name, "eq")

    def tag_counter(self, name: int or str, ctype="eq") -> str:
        return "\\tag{" + str(self.new_counter(name, ctype)) + "}"

    def begin_counter(self, name: int or str, what="align*", ctype="eq") -> str:
        self.counter_stack.append(what)
        cnt = ""
        if name:
            cnt = self.tag_counter(name, ctype)
        label = ""
        if self.tex:
            if name:
                label = " \\label{" + LABEL_PRFIX + name + "}"
            return "\\begin{" + what + "}" + label + cnt
        if name:
            label = '<a id="' + LABEL_PRFIX + name + '"></a>'
        return label + "\\begin{" + what + "}" + cnt

    def end_counter(self, _dummy: str = "") -> str:
        if len(self.counter_stack) == 0:
            return ""
        what = self.counter_stack.pop()
        return "\\end{" + what + "}"

    def fig_counter(self, name: int or str) -> str:
        return self.new_label_counter(name, "fig")

    def tbl_counter(self, name: int or str) -> str:
        return self.new_label_counter(name, "tbl")

    def ex_counter(self, name: int or str) -> str:
        return self.new_label_counter(name, "ex")

    def task_counter(self, name: int or str) -> str:
        return self.new_label_counter(name, "task")

    def get_counter_macros(self, _dummy: int or str = 0) -> str:
        result = "  use_autonumbering: true\n"
        result += "  c:\n"
        for ctype in self.counters:
            counter_type = self.counters[ctype]
            for name in counter_type:
                if name == "value":
                    continue
                counter = counter_type[name]
                jso = json.dumps(counter)
                result += "    " + name + ": " + jso + "\n"
        return result

    def set_counter(self, ctype: str, value: int) -> str:
        counter_type = self.counters.get(ctype)
        if not counter_type:
            return ""
        counter_type["value"] = value
        return ""

    def set_renumbering(self, value: bool) -> None:
        self.renumbering = value
        self.macros["use_autonumbering"] = value

    def get_type_text(
        self, ctype: str, name: str, value: str, showtype: str, pure: str
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

        fmt_def = {
            **self.autocounters_fmt_def["all"],
            **self.autocounters_fmt_def.get(ctype, {}),
            **autocounters.get("all", {}),
        }

        counter_fmt = autocounters.get(ctype, fmt_def)
        if not counter_fmt:  # should not occur!
            return str(value)

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
        text = s.format(**values)
        return text

    def reset_counters(self, n: int) -> None:
        """
        Reset all counters that should be reset when heading level n
        changes
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

    def set_heading_vals(self, vals: dict) -> None:
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
        if self.macros and self.macros.get("use_autonumbering"):
            env.filters["lref"] = self.show_lref_value
            env.filters["ref"] = self.show_ref_value
            env.filters["pref"] = self.show_pref_value
            env.filters["labels"] = self.labels
            env.filters["c_"] = self.new_label_counter
            env.filters["c_n"] = self.new_counter
            env.filters["c_eq"] = self.eq_counter
            env.filters["c_tag"] = self.tag_counter
            env.filters["c_begin"] = self.begin_counter
            env.filters["c_end"] = self.end_counter
            env.filters["c_fig"] = self.fig_counter
            env.filters["c_tbl"] = self.tbl_counter
            env.filters["c_ex"] = self.ex_counter
            env.filters["c_task"] = self.task_counter
            env.filters["c_set"] = self.set_counter


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
        self.counters: AutoCounters or None = None

    def set_counters(self, counters: AutoCounters) -> None:
        self.counters = counters
        counters.set_env_filters(self)

    def get_counters(self) -> AutoCounters:
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
