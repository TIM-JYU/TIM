"""A test for markdownconverter module."""
import yaml

from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.markdown.autocounters import AutoCounters
from timApp.markdown.markdownconverter import md_to_html, par_list_to_html_list
from timApp.printing.documentprinter import DocumentPrinter
from timApp.tests.db.timdbtest import TEST_USER_1_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User


class AutoCountersTest(TimRouteTest):
    def test_fig_counter(self):
        macros = {}
        counters = AutoCounters(macros)
        s = counters.fig_counter("figA")
        self.assertEqual(
            '<a id=""></a>?figA?',
            s,
            msg="AutoCounters figure before renumbering",
        )

        counters.renumbering = True
        s = counters.fig_counter("figA")
        counters.fig_counter("figB")
        counters.fig_counter("figC")
        self.assertEqual(
            '<a id=""></a>?figA?', s, msg="AutoCounters figure when renumbering"
        )
        auto_macros = counters.get_counter_macros()
        auto_json = yaml.load(auto_macros, Loader=yaml.SafeLoader)
        fig_a = auto_json["macros"]["autocnts"]["figA"]
        fig_a_ex = {
            "h": "c:figA",
            "l": "1",
            "n": "figA",
            "p": "1",
            "r": "1",
            "s": "1",
            "t": "1",
            "v": 1,
            "y": "fig",
        }
        self.assertEqual(
            fig_a_ex, fig_a, msg="AutoCounters figure macros after renumbering"
        )
        macros = auto_json["macros"]
        counters = AutoCounters(macros)
        sa = counters.fig_counter("figA")
        self.assertEqual(
            '<a id="c:figA"></a>1', sa, msg="AutoCounters figure A after renumbering"
        )
        sb = counters.fig_counter("figB")
        self.assertEqual(
            '<a id="c:figB"></a>2', sb, msg="AutoCounters figure B after renumbering"
        )
        sc = counters.fig_counter("figC")
        self.assertEqual(
            '<a id="c:figC"></a>3', sc, msg="AutoCounters figure C after renumbering"
        )

        sb = counters.show_ref_value("figB")
        self.assertEqual("[2](#c:figB)", sb, msg="AutoCounters ref to figB")

        macros["tex"] = True
        counters = AutoCounters(macros)
        sb = counters.fig_counter("figB").strip()
        self.assertEqual("\\label{c:figB}2", sb, msg="AutoCounters figB in LaTeX")

    def check_conversion(self, html, md, macros=None):
        s = md_to_html(md, sanitize=True, macros=macros)
        self.assertEqual(html, s)

    def doc_with_counters(self, docstr, htmls_ex, msg, setstr=None):
        self.login_test1()
        settings = """``` {settings=""}
auto_number_headings: 0
```
"""
        if setstr:
            settings = setstr
        dstr = settings + docstr
        d = self.create_doc(initial_par=dstr)

        doc = d.document
        doc.get_index(default_view_ctx)

        printer = DocumentPrinter(d, template_to_use=None, urlroot="")
        counters = printer.get_autocounters(
            UserContext.from_one_user(db.session.get(User, TEST_USER_1_ID)),
            default_view_ctx,
        )
        new_counter_macro_values = (
            f'``` {{settings="counters"}}\n{counters.get_counter_macros()}```\n'
        )

        dstr = settings + new_counter_macro_values + docstr
        d = self.create_doc(initial_par=dstr)
        doc = d.document
        doc.get_index(default_view_ctx)

        p = d.document.get_paragraphs()[2:]
        htmls = (
            par_list_to_html_list(
                p, settings=d.document.get_settings(), view_ctx=default_view_ctx
            ),
        )
        actual = "\n".join(htmls[0])
        self.assertEqual(htmls_ex.strip("\n"), actual.strip("\n"), msg=msg)

    def cnt_labels(self, labels):
        lbsl = labels.split(" ")
        result = '<p><span class="headerlink cnt-labels">'
        for lbl in labels.split(" "):
            result += f'<span class="cnt-label">{lbl}</span> '
        result = result.strip(" ")
        return result + "</span></p>"

    def test_begin1_environment(self):
        docstr = r"""#- 
%%"Pythagoras |py" | c_begin1%%
a^2+b^2=c^2
%%""|c_end%%
#-
Katso %%"py"|lref%%
"""
        htmls_ex = (
            r"""
<p><a id="eq:py"></a><span class="math display">\[\begin{align*}\tag{Pythagoras 1}
a^2+b^2=c^2
\end{align*}\]</span></p>
"""
            + self.cnt_labels("1=py")
            + r"""
<p>Katso <a href="#eq:py">(Pythagoras 1)</a></p>
"""
        )

        self.doc_with_counters(docstr, htmls_ex, "AutoCounters c_begin1")

    def test_begin_environment(self):
        docstr = r"""#- 
%%"x"|c_begin%%
a+1 §\
a+2 §\
a+3 {§a3§}
a+4 §\
%%""|c_end%%
#-
Katso %%"x1"|ref%%, %%"x2"|ref%%, %%"a3"|ref%%
"""
        htmls_ex = (
            r"""
<p><a id="eq:x"></a><span class="math display">\[\begin{align*}
a+1 \tag{1}\\
a+2 \tag{2}\\
a+3 \tag{3}\\
a+4 \tag{4}\\
\end{align*}\]</span></p>
"""
            + self.cnt_labels("1=x1 2=x2 3=a3 4=x4")
            + r"""
<p>Katso <a href="#eq:x">(1)</a>, <a href="#eq:x">(2)</a>, <a href="#eq:x">(3)</a></p>
"""
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters c_begin")

    def test_begin_no_base_name_environment(self):
        docstr = r"""#- 
%%""|c_begin%%
a+3 {§a3§}
a+4 {§a4§}
%%""|c_end%%
#-
Katso %%"a3"|ref%%, %%"a4"|ref%% 
"""
        htmls_ex = (
            r"""
<p><a id="eq:a3"></a><a id="eq:a4"></a><span class="math display">\[\begin{align*}
a+3 \tag{1}\\
a+4 \tag{2}\\
\end{align*}\]</span></p>
"""
            + self.cnt_labels("1=a3 2=a4")
            + r"""
<p>Katso <a href="#eq:a3">(1)</a>, <a href="#eq:a4">(2)</a></p>
"""
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters c_begin no basename")

    def test_no_c_begin_environment(self):
        docstr = r"""#- 
\begin{align*}
a+3 {§x1\§}
a+4 §\
\end{align*}
#-
Katso %%"x1"|pref%%, %%"x12"|pref%%
"""
        htmls_ex = (
            r"""
<p><span class="math display">\[\begin{align*}
a+3 \tag{1}\\
a+4 \tag{2}\\
\end{align*}\]</span></p>
"""
            + self.cnt_labels("1=x1 2=x12")
            + """
<p>Katso (1), (2)</p>
"""
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters no c_begin")

    def test_no_c_begin_environment_no_man_name(self):
        docstr = r"""#- {#kaava} 
\begin{align*}
a+3 §\
a+4 §\
\end{align*}
#-
Katso %%"kaava1"|pref%%, %%"kaava2"|pref%%
"""
        htmls_ex = (
            r"""
<p><span class="math display">\[\begin{align*}
a+3 \tag{1}\\
a+4 \tag{2}\\
\end{align*}\]</span></p>
"""
            + self.cnt_labels("1=kaava1 2=kaava2")
            + r"""
<p>Katso (1), (2)</p>
"""
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters no c_begin, no man")

    def test_tasks(self):
        docstr = r""" 
``` {plugin="csPlugin" #shell1}
type: shell
path: user
header: 'Tehtävä §n: Eka'
```

``` {plugin="csPlugin" #shell2}
type: shell
path: user
header: 'Tehtävä §n: Toka'
```
#-
Katso %%"shell1"|ref%%, %%"shell2"|ref%% 
"""
        htmls_ex = r"""
<pre><code>type: shell
path: user
header: &#39;Tehtävä 1: Eka&#39;</code></pre>
<pre><code>type: shell
path: user
header: &#39;Tehtävä 2: Toka&#39;</code></pre>
<p>Katso <a href="#shell1">1</a>, <a href="#shell2">2</a></p>
"""
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters tasks")

    def test_set_counter_values(self):
        docstr = r"""#-
Kuva %%"k1"|c_fig%%
%%("fig"|c_get +3)|c_set("fig")%%
Kuva %%"k2"|c_fig%%
"""
        htmls_ex = r"""
<p>Kuva <a id="c:k1"></a>1</p>
<p>Kuva <a id="c:k2"></a>5</p>
""" + self.cnt_labels(
            "1=k1 5=k2"
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters set values")

    def test_autocounter_by_task_id(self):
        docstr = r"""
#- {#oma}
Eka §a
Toka §a
"""
        htmls_ex = r"""
<p>Eka <a id="c:oma1"></a>1 Toka <a id="c:oma2"></a>2</p>
""" + self.cnt_labels(
            "1=oma1 2=oma2"
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters by taskid")

    def test_autocounter_by_task_id_no_jump(self):
        docstr = r"""
#- {#oma}
Eka §n
Toka §n
"""
        htmls_ex = r"""
<p>Eka 1 Toka 2</p>
""" + self.cnt_labels(
            "1=oma1 2=oma2"
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters by tasks id none jump")

    def test_autocounter_by_block_id(self):
        docstr = r"""
#- {id="bJmnL9inGk9g"}
Eka §a
Toka §a
"""
        htmls_ex = r"""
<p>Eka <a id="c:bJmnL9inGk9g1"></a>1 Toka <a id="c:bJmnL9inGk9g2"></a>2</p>
""" + self.cnt_labels(
            "1=bJmnL9inGk9g1 2=bJmnL9inGk9g2"
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters by block id")

    def test_autocounter_by_task_id_no_jump(self):
        docstr = r"""
#- {#oma}
Eka §n
Toka §n
"""
        htmls_ex = r"""
<p>Eka 1 Toka 2</p>
""" + self.cnt_labels(
            "1=oma1 2=oma2"
        )
        self.doc_with_counters(docstr, htmls_ex, "AutoCounters by tasks id none jump")

    def test_autocounter_with_section_numbers(self):
        setstr = r"""``` {settings=""}
auto_number_headings: 2
autocounters:
    all:
      reset: 2  
    eq:
      ref: "formula ({p})"
      long: "formula ({p})"
```
"""
        docstr = r"""#-
# Formulas 
#-        
## First 
#-        
%%"x"| c_begin("align*")%%
a+1 §\
a+2 {§|b|, nice§}
%%""|c_end%%
#-
## Continue {#cont}
#-
%%"y"| c_begin("align*")%%
a+1 §\
a+2 §\
%%""|c_end%%
#-
Look %%"x1"|lref%%,\
%%"b"|ref%%,\
%%"b"|lref%% and\
%%"y2"|ref%%.

Also look par %%"cont"|ref%% that is %%"cont"|lref%%.
"""
        htmls_ex = (
            r"""
<h1 id="formulas">Formulas</h1>
<h2 id="first">First</h2>
<p><a id="eq:x"></a><span class="math display">\[\begin{align*}
a+1 \tag{1.1}\\
a+2 \tag{1.2, nice}\\
\end{align*}\]</span></p>
"""
            + self.cnt_labels("1=x1 2=b")
            + "\n"
            + r"""
<h2 id="continue">Continue</h2>
<p><a id="eq:y"></a><span class="math display">\[\begin{align*}
a+1 \tag{2.3}\\
a+2 \tag{2.4}\\
\end{align*}\]</span></p>
"""
            + self.cnt_labels("3=y1 4=y2")
            + r"""
<p>Look <a href="#eq:x">formula (1.1)</a>,<br />
<a href="#eq:x">formula (1.2)</a>,<br />
<a href="#eq:x">formula (1.2, nice)</a> and<br />
<a href="#eq:y">formula (2.4)</a>.</p>
<p>Also look par <a href="#cont">2</a> that is <a href="#cont">2. Continue</a>.</p>
"""
        )
        self.doc_with_counters(
            docstr, htmls_ex, "AutoCounters with section numbers", setstr
        )

    def test_autotypes(self):
        setstr = r"""``` {settings=""}
auto_number_headings: 2
autocounters:
     autotypes:
       - lause
       - maar
     lause:
        ref: "Lause {p}"
     maar:
        ref: "Määritelmä {p}"
```
"""
        docstr = r"""#-
## Määritelmä {.maar #maarit1}
...
## Määritelmä {.maar #maarit2}
...
## Lause {.lause #lause1}
...
## Lause {.lause #lause2}
#-
- Katso %%"maarit1"|ref%%
- Katso %%"maarit2"|ref%%
- Todista %%"lause1"|ref%%
- Todista %%"lause2"|ref%%
- Todista %%"|lause2"|ref%%
"""
        htmls_ex = """
<h2 id="määritelmä">Määritelmä</h2>
<p>...</p>
<h2 id="määritelmä">Määritelmä</h2>
<p>...</p>
<h2 id="lause">Lause</h2>
<p>...</p>
<h2 id="lause">Lause</h2>
<ul>
<li>Katso <a href="#maarit1">Määritelmä 1</a></li>
<li>Katso <a href="#maarit2">Määritelmä 2</a></li>
<li>Todista <a href="#lause1">Lause 3</a></li>
<li>Todista <a href="#lause2">Lause 4</a></li>
<li>Todista <a href="#lause2">4</a></li>
</ul>
"""
        self.doc_with_counters(
            docstr, htmls_ex, "AutoCounters with section numbers", setstr
        )
