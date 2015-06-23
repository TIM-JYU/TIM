"""A test for markdownconverter module."""
import unittest
import time

import dumboclient
from markdownconverter import md_to_html, md_list_to_html_list


class MarkdownConverterTest(unittest.TestCase):
    def setUp(self):
        self.d = dumboclient.launch_dumbo()
        time.sleep(0.1)  # Need to wait a bit to make sure Dumbo is up when running the test

    def check_conversion(self, html, md):
        self.assertEqual(html, md_to_html(md))

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

        self.assertListEqual([html for html, _ in cases], md_list_to_html_list([md for _, md in cases]))

    def tearDown(self):
        self.d.kill()


if __name__ == '__main__':
    unittest.main()
