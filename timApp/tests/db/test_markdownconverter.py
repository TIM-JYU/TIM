"""A test for markdownconverter module."""
import unittest

import timApp.markdown.dumboclient
from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.markdown.markdownconverter import md_to_html, par_list_to_html_list
from timApp.tests.db.timdbtest import TimDbTest


class MarkdownConverterTest(TimDbTest):

    def check_conversion(self, html, md, macros=None, delimiter=None):
        self.assertEqual(html, md_to_html(md, sanitize=True, macros=macros, macro_delimiter=delimiter))

    def test_markdown(self):
        cases = [('', ''),
                 ('<h1 id="hello">Hello</h1>', '# Hello'),
                 ('<h2 id="hello">Hello</h2>', '## Hello'),
                 ('<h3 id="hello">Hello</h3>', '### Hello'),
                 ('<h4 id="hello">Hello</h4>', '#### Hello'),
                 ('<h5 id="hello">Hello</h5>', '##### Hello'),
                 ('<h6 id="hello-world">Hello world</h6>', '###### Hello world'),
                 ('<p id="hello">Hello</p>', '####### Hello'),
                 ('<p>test</p>', 'test')]

        for html, md in cases:
            self.check_conversion(html, md)
        d = self.create_doc()
        self.assertListEqual([html for html, _ in cases],
                             par_list_to_html_list([DocParagraph.create(d.document, md=md) for _, md in cases],
                                                   settings=d.document.get_settings()))

        macrotests = [('<p>hello world!</p>',
                       'hello %%somemacro%%!',
                       {'somemacro': 'world'})]

        for html, md, macros in macrotests:
            self.check_conversion(html, md, macros, delimiter='%%')

    def test_bracketed_spans(self):
        self.assertEqual('<p><span class="testing">test</span></p>', md_to_html('[test]{.testing}'),
                         msg='If this test fails, you probably do not have up-to-date Dumbo. '
                             'Run ./pull_all.sh to update.')

    def test_unsafe_not_allowed(self):
        self.assertEqual("""
<div class="error">
Syntax error in template: access to attribute &#8216;<strong>class</strong>&#8217; of &#8216;str&#8217; object is unsafe.
</div>
        """.strip(), md_to_html("""%%''.__class__.__mro__%%""", macro_delimiter='%%', macros={}))

    def test_markup_md_conversion(self):
        self.assertEqual({'test1': 'value1', 'test2': '<em>value2</em>'}, timApp.markdown.dumboclient.call_dumbo(
            {'test1': 'value1', 'test2': 'md:*value2*'}, path='/mdkeys'))
        self.assertEqual(
            [{'test1': 'value1', 'test2': '<em>value2</em>'}, {'test3': 'value3', 'test4': '<strong>value4</strong>'}],
            timApp.markdown.dumboclient.call_dumbo(
                [{'test1': 'value1', 'test2': 'md:*value2*'}, {'test3': 'value3', 'test4': 'md:**value4**'}],
                path='/mdkeys'))


if __name__ == '__main__':
    unittest.main()
