"""A test for markdownconverter module."""
import unittest

import timApp.markdown.dumboclient
from timApp.document.docparagraph import DocParagraph
from timApp.document.viewcontext import default_view_ctx
from timApp.markdown.markdownconverter import md_to_html, par_list_to_html_list
from timApp.tests.db.timdbtest import TimDbTest
from tim_common.cs_sanitizer import tim_sanitize

SMART_PUNCT_MD = """
"Test"

Test1 -- test1

Test2 --- test2

Ellipses...
"""

SMART_PUNCT_HTML = """
<p>“Test”</p>
<p>Test1 – test1</p>
<p>Test2 — test2</p>
<p>Ellipses…</p>
"""

NO_SMART_PUNCT_HTML = """
<p>"Test"</p>
<p>Test1 -- test1</p>
<p>Test2 --- test2</p>
<p>Ellipses...</p>
"""


class MarkdownConverterTest(TimDbTest):
    def check_conversion(self, html, md, macros=None):
        self.assertEqual(html, md_to_html(md, sanitize=True, macros=macros))

    def test_smart_punct(self):
        d = self.create_doc()

        main_par = DocParagraph.create(d.document, md=SMART_PUNCT_MD)

        def check_html(expected: str, par: DocParagraph, reason: str):
            bs = par_list_to_html_list(
                [par],
                settings=d.document.get_settings(),
                view_ctx=default_view_ctx,
            )
            self.assertEqual(expected.strip(), bs[0], reason)

        check_html(NO_SMART_PUNCT_HTML, main_par, "Default case: smart punct disabled")

        d.document.set_settings({"smart_punct": "true"})
        check_html(SMART_PUNCT_HTML, main_par, "Doc smart_punct: true")

        d.document.set_settings({"smart_punct": "false"})
        check_html(NO_SMART_PUNCT_HTML, main_par, "Doc smart_punct: false")

        second_par = DocParagraph.create(d.document, md=SMART_PUNCT_MD)
        check_html(
            NO_SMART_PUNCT_HTML, second_par, "Doc smart_punct: false; second par"
        )

        second_par.set_attr("smart_punct", "true")
        check_html(
            SMART_PUNCT_HTML,
            second_par,
            "Doc smart_punct: false; second par with smart_punct: true",
        )

    def test_markdown(self):
        cases = [
            ("", ""),
            ('<h1 id="hello">Hello</h1>', "# Hello"),
            ('<h2 id="hello">Hello</h2>', "## Hello"),
            ('<h3 id="hello">Hello</h3>', "### Hello"),
            ('<h4 id="hello">Hello</h4>', "#### Hello"),
            ('<h5 id="hello">Hello</h5>', "##### Hello"),
            ('<h6 id="hello-world">Hello world</h6>', "###### Hello world"),
            ('<p class="heading" id="hello">Hello</p>', "####### Hello"),
            ("<p>test</p>", "test"),
        ]

        for html, md in cases:
            self.check_conversion(html, md)
        d = self.create_doc()
        self.assertListEqual(
            [html for html, _ in cases],
            par_list_to_html_list(
                [DocParagraph.create(d.document, md=md) for _, md in cases],
                settings=d.document.get_settings(),
                view_ctx=default_view_ctx,
            ),
        )

        macrotests = [
            ("<p>hello world!</p>", "hello %%somemacro%%!", {"somemacro": "world"})
        ]

        for html, md, macros in macrotests:
            self.check_conversion(html, md, macros)

    def test_rst(self):
        d = self.create_doc(
            initial_par="""
#- {settings=""}
input_format: markdown
#- {input_format=rst}
.. image:: images/hi.png
        """
        )
        p = d.document.get_paragraphs()[1]
        self.assertEqual(
            ['<p><img src="images/hi.png" alt="image" /></p>'],
            par_list_to_html_list(
                [p], settings=d.document.get_settings(), view_ctx=default_view_ctx
            ),
        )

        d = self.create_doc(
            initial_par="""
#- {settings=""}
input_format: rst
#-
.. image:: images/hi.png
#- {input_format=markdown}
.. image:: images/hi.png
            """
        )
        p = d.document.get_paragraphs()[1:]
        self.assertEqual(
            [
                '<p><img src="images/hi.png" alt="image" /></p>',
                "<p>.. image:: images/hi.png</p>",
            ],
            par_list_to_html_list(
                p, settings=d.document.get_settings(), view_ctx=default_view_ctx
            ),
        )

    def test_invalid_inputformat(self):
        d = self.create_doc(
            initial_par="""
#- {input_format=xxx}
.. image:: images/hi.png"""
        )
        p = d.document.get_paragraphs()[0]
        self.assertEqual(
            ["<p>.. image:: images/hi.png</p>"],
            par_list_to_html_list(
                [p], settings=d.document.get_settings(), view_ctx=default_view_ctx
            ),
        )

    def test_bracketed_spans(self):
        self.assertEqual(
            '<p><span class="testing">test</span></p>',
            md_to_html("[test]{.testing}"),
            msg="If this test fails, you probably do not have up-to-date Dumbo. "
            "Run ./pull_all.sh to update.",
        )

    def test_unsafe_not_allowed(self):
        self.assertEqual(
            """
<p><span class="error">Syntax error in template: access to attribute '<strong>class</strong>' of 'str' object is unsafe.</span></p>
        """.strip(),
            md_to_html("""%%''.__class__.__mro__%%""", macros={}),
        )

    def test_markup_md_conversion(self):
        self.assertEqual(
            {"test1": "value1", "test2": "<em>value2</em>"},
            timApp.markdown.dumboclient.call_dumbo(
                {"test1": "value1", "test2": "md:*value2*"}, path="/mdkeys"
            ),
        )
        self.assertEqual(
            [
                {"test1": "value1", "test2": "<em>value2</em>"},
                {"test3": "value3", "test4": "<strong>value4</strong>"},
            ],
            timApp.markdown.dumboclient.call_dumbo(
                [
                    {"test1": "value1", "test2": "md:*value2*"},
                    {"test3": "value3", "test4": "md:**value4**"},
                ],
                path="/mdkeys",
            ),
        )

    def test_markdown_sanitize_neutral(self):
        test_strs = [
            "Image 1",
            "[Image 1](/images/1041/image.png)",
            "[Image 1](/images/1041/image.png){width=300, height=200}",
        ]
        for t in test_strs:
            self.assertEqual(tim_sanitize(t), t)
