"""Server tests for xxx."""
from timApp.markdownconverter import md_to_html
from timApp.plugin import Plugin
from timApp.tests.db.timdbtest import TEST_USER_1_ID, TEST_USER_2_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.models.user import User
from timApp.timdb.userutils import grant_view_access


class MacroTest(TimRouteTest):
    def test_macro_doc(self):
        self.login_test1()
        doc = self.create_doc(settings={'macro_delimiter': '%%', 'macros': {'rivi': 'kerros'}}).document
        table_text = """
{% set sarakeleveys = 50 %}
{% set sarakkeet = ['eka', 'toka', 'kolmas', 'neljäs'] %}
{% set rivit = ['eka', 'toka', 'kolmas', 'neljäs', 'viides'] %}

{% for x in sarakkeet %} %%'-'*sarakeleveys%% {% endfor %}

{% for r in rivit %}
{% for s in sarakkeet %}%%('{} {}, {} sarake').format(r,rivi,s).rjust(sarakeleveys)%% {% endfor %}

{% endfor %}
{% for x in sarakkeet %} %%'-'*sarakeleveys%% {% endfor %}
            """
        table_html = md_to_html(table_text, sanitize=True, macros={'rivi': 'kerros'}, macro_delimiter='%%')

        self.new_par(doc, table_text, json_key='texts', expect_contains=table_html)
        self.get(f'/view/{doc.doc_id}', expect_contains=table_html)

    def test_user_macros(self):
        self.login_test1()
        timdb = self.get_db()
        d = self.create_doc(initial_par=r"""
Username is %%username%% and real name is %%realname%% and email is %%useremail%%

#-
Percents: \%\%
#-
Broken: %%

``` {#test plugin="csPlugin"}
type: cs
header: %%username%% and %%realname%%
```
            """)
        grant_view_access(timdb.users.get_personal_usergroup_by_id(TEST_USER_2_ID), d.id)

        pars = self.get(d.url, as_tree=True).cssselect('.parContent')
        self.assertEqual('Username is testuser1 and real name is Test user 1 and email is test1@example.com',
                         pars[0].text_content().strip())
        self.assertEqual('Percents: %%',
                         pars[1].text_content().strip())
        self.assertEqual("Syntax error in template: unexpected 'end of template'",
                         pars[2].text_content().strip())
        p = Plugin.from_task_id(f'{d.id}.test', User.query.get(TEST_USER_1_ID))
        self.assertEqual('testuser1 and Test user 1', p.values['header'])
        self.login_test2()
        self.assertEqual('Username is testuser2 and real name is Test user 2 and email is test2@example.com',
                         self.get(f'/view/{d.id}', as_tree=True).cssselect('.parContent')[
                             0].text_content().strip())
        p = Plugin.from_task_id(f'{d.id}.test', User.query.get(TEST_USER_2_ID))
        self.assertEqual('testuser2 and Test user 2', p.values['header'])

    def test_macro_only_delimiter(self):
        self.login_test1()
        doc = self.create_doc(settings={'macro_delimiter': '%%'}).document
        self.new_par(doc, '{% set a = 123456+1 %}%%a%%', json_key='texts', expect_contains='123457')

    def test_null_macros(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin=showVideo}
```
""")
        d.document.set_settings({'macros': None})
        r = self.get(d.url, as_tree=True).cssselect('.parContent')
        self.assertIn('xxxHEXJSONxxx', r[1].text_content().strip())

    def test_invalid_macros(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin=showVideo}
```
""")
        d.document.set_settings({'macros': 'invalid'})
        self.get(d.url)
        d.document.set_settings({'macros': {'a': {'b': 'c'}}})
        self.get(d.url)
