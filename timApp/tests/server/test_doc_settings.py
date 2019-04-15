from timApp.tests.server.timroutetest import TimRouteTest


class DocSettingsTest(TimRouteTest):
    def test_invalid_settings(self):
        self.login_test1()
        d = self.create_doc(initial_par="""#- {settings=""}\nx""")
        t = self.get(d.url, as_tree=True)
        self.assert_content(t, ['Invalid settings: Invalid YAML: Markup must not be a mere string.'])

    def test_invalid_multiline_settings(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {settings=""}
a: |!!
 t
The quick brown fox jumps over the lazy dog.
!!
    """)
        t = self.get(d.url, as_tree=True)
        self.assert_content(
            t,
            ['Invalid settings: Invalid YAML: The line "The quick brown fox jumps..." must be indented at least as '
             'much as the first line.'])

    def test_input_format_change(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
.. image:: images/hi.png
        """)
        r = self.get(d.url, as_tree=True).cssselect('.parContent img')
        self.assertFalse(r)
        d.document.set_settings({'input_format': 'rst'})
        r = self.get(d.url, as_tree=True).cssselect('.parContent img')
        self.assertTrue(r)
