"""A test for markdownconverter module."""
import unittest

import timApp.markdown.dumboclient
from timApp.document.docparagraph import DocParagraph
from timApp.document.viewcontext import default_view_ctx
from timApp.markdown.markdownconverter import md_to_html, par_list_to_html_list
from timApp.tests.db.timdbtest import TimDbTest


class MarkdownConverterTest(TimDbTest):

    def check_conversion(self, html, md, macros=None):
        self.assertEqual(html, md_to_html(md, sanitize=True, macros=macros))

    def test_markdown(self):
        cases = [('', ''),
                 ('<h1 id="hello">Hello</h1>', '# Hello'),
                 ('<h2 id="hello">Hello</h2>', '## Hello'),
                 ('<h3 id="hello">Hello</h3>', '### Hello'),
                 ('<h4 id="hello">Hello</h4>', '#### Hello'),
                 ('<h5 id="hello">Hello</h5>', '##### Hello'),
                 ('<h6 id="hello-world">Hello world</h6>', '###### Hello world'),
                 ('<p class="heading" id="hello">Hello</p>', '####### Hello'),
                 ('<p>test</p>', 'test')]

        for html, md in cases:
            self.check_conversion(html, md)
        d = self.create_doc()
        self.assertListEqual([html for html, _ in cases],
                             par_list_to_html_list([DocParagraph.create(d.document, md=md) for _, md in cases],
                                                   settings=d.document.get_settings(), view_ctx=default_view_ctx))

        macrotests = [('<p>hello world!</p>',
                       'hello %%somemacro%%!',
                       {'somemacro': 'world'})]

        for html, md, macros in macrotests:
            self.check_conversion(html, md, macros)

    def test_rst(self):
        d = self.create_doc(initial_par="""
#- {settings=""}
input_format: markdown
#- {input_format=rst}
.. image:: images/hi.png
        """)
        p = d.document.get_paragraphs()[1]
        self.assertEqual(['<p><img src="images/hi.png" alt="image" /></p>'],
                         par_list_to_html_list([p], settings=d.document.get_settings(), view_ctx=default_view_ctx))

        d = self.create_doc(initial_par="""
#- {settings=""}
input_format: rst
#-
.. image:: images/hi.png
#- {input_format=markdown}
.. image:: images/hi.png
            """)
        p = d.document.get_paragraphs()[1:]
        self.assertEqual(['<p><img src="images/hi.png" alt="image" /></p>',
                          '<p>.. image:: images/hi.png</p>'],
                         par_list_to_html_list(p, settings=d.document.get_settings(), view_ctx=default_view_ctx))

    def test_invalid_inputformat(self):
        d = self.create_doc(initial_par="""
#- {input_format=xxx}
.. image:: images/hi.png""")
        p = d.document.get_paragraphs()[0]
        self.assertEqual(['<p>.. image:: images/hi.png</p>'],
                         par_list_to_html_list([p], settings=d.document.get_settings(), view_ctx=default_view_ctx))

    def test_bracketed_spans(self):
        self.assertEqual('<p><span class="testing">test</span></p>', md_to_html('[test]{.testing}'),
                         msg='If this test fails, you probably do not have up-to-date Dumbo. '
                             'Run ./pull_all.sh to update.')

    def test_unsafe_not_allowed(self):
        self.assertEqual("""
<p><span class="error">Syntax error in template: access to attribute &#8216;<strong>class</strong>&#8217; of &#8216;str&#8217; object is unsafe.</span></p>
        """.strip(), md_to_html("""%%''.__class__.__mro__%%""", macros={}))

    def test_markup_md_conversion(self):
        self.assertEqual({'test1': 'value1', 'test2': '<em>value2</em>'}, timApp.markdown.dumboclient.call_dumbo(
            {'test1': 'value1', 'test2': 'md:*value2*'}, path='/mdkeys'))
        self.assertEqual(
            [{'test1': 'value1', 'test2': '<em>value2</em>'}, {'test3': 'value3', 'test4': '<strong>value4</strong>'}],
            timApp.markdown.dumboclient.call_dumbo(
                [{'test1': 'value1', 'test2': 'md:*value2*'}, {'test3': 'value3', 'test4': 'md:**value4**'}],
                path='/mdkeys'))
