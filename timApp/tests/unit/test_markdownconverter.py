"""A test for markdownconverter module."""
import unittest
import time

import dumboclient
from documentmodel.docparagraph import DocParagraph
from documentmodel.docsettings import DocSettings
from markdownconverter import md_to_html, par_list_to_html_list


class MarkdownConverterTest(unittest.TestCase):

    def setUp(self):
        self.d = dumboclient.launch_dumbo()
        time.sleep(0.1)  # Need to wait a bit to make sure Dumbo is up when running the test

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

        self.assertListEqual([html for html, _ in cases],
                             par_list_to_html_list([DocParagraph.create(None, md=md) for _, md in cases],
                                                   settings=DocSettings()))

        macrotests = [('<p>hello world!</p>',
                       'hello %%somemacro%%!',
                       {'somemacro': 'world'})]

        for html, md, macros in macrotests:
            self.check_conversion(html, md, macros, delimiter='%%')

    def test_bracketed_spans(self):
        self.assertEqual('<p><span class="testing">test</span></p>', md_to_html('[test]{.testing}'),
                         msg='If this test fails, you probably do not have up-to-date Dumbo. '
                             'Run ./pull_all.sh to update.')

    def test_markup_md_conversion(self):
        self.assertEqual({'test1': 'value1', 'test2': '<em>value2</em>'}, dumboclient.call_dumbo(
            {'test1': 'value1', 'test2': 'md:*value2*'}, path='/mdkeys'))
        self.assertEqual(
            [{'test1': 'value1', 'test2': '<em>value2</em>'}, {'test3': 'value3', 'test4': '<strong>value4</strong>'}],
            dumboclient.call_dumbo(
                [{'test1': 'value1', 'test2': 'md:*value2*'}, {'test3': 'value3', 'test4': 'md:**value4**'}],
                path='/mdkeys'))

    def tearDown(self):
        self.d.kill()


if __name__ == '__main__':
    unittest.main()
