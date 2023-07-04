"""Server tests for macros."""
from timApp.auth.accesstype import AccessType
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
    DEFAULT_PREAMBLE_DOC,
)
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.markdown.markdownconverter import md_to_html
from timApp.plugin.plugin import Plugin
from timApp.tests.db.timdbtest import TEST_USER_1_ID, TEST_USER_2_ID
from timApp.tests.server.timroutetest import TimRouteTest, get_content
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.util.utils import decode_csplugin


class MacroTest(TimRouteTest):
    def test_macro_doc(self):
        self.login_test1()
        doc = self.create_doc(
            settings={"macro_delimiter": "%%", "macros": {"rivi": "kerros"}}
        ).document
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
        table_html = md_to_html(table_text, sanitize=True, macros={"rivi": "kerros"})

        self.new_par(doc, table_text, json_key="texts", expect_contains=table_html)
        self.get(f"/view/{doc.doc_id}", expect_contains=table_html)

    def test_user_macros(self):
        self.login_test1()
        d = self.create_doc(
            initial_par=r"""
Username is %%username%% and real name is %%realname%% and email is %%useremail%% and logged name is %%loggedUsername%% and id is %%userid%%

#-
Percents: \%\%
#-
Broken: %%

``` {#test plugin="csPlugin"}
type: cs
header: %%username%% and %%realname%%
```
            """
        )
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()

        pars = self.get(d.url, as_tree=True).cssselect(".parContent")
        self.assertEqual(
            f"Username is {self.test_user_1.name} and real name is {self.test_user_1.real_name} and email is {self.test_user_1.email} and logged name is {self.test_user_1.name} and id is {self.test_user_1.id}",
            pars[0].text_content().strip(),
        )
        self.assertEqual("Percents: %%", pars[1].text_content().strip())
        self.assertEqual(
            "Syntax error in macro template: unexpected 'end of template'",
            pars[2].text_content().strip(),
        )
        p, _ = Plugin.from_task_id(
            f"{d.id}.test",
            UserContext.from_one_user(db.session.get(User, TEST_USER_1_ID)),
            default_view_ctx,
        )
        self.assertEqual("testuser1 and Test user 1", p.values["header"])
        self.login_test2()
        self.assertEqual(
            f"Username is {self.test_user_2.name} and real name is {self.test_user_2.real_name} and email is {self.test_user_2.email} and logged name is {self.test_user_2.name} and id is {self.test_user_2.id}",
            self.get(f"/view/{d.id}", as_tree=True)
            .cssselect(".parContent")[0]
            .text_content()
            .strip(),
        )
        p, _ = Plugin.from_task_id(
            f"{d.id}.test",
            UserContext.from_one_user(db.session.get(User, TEST_USER_2_ID)),
            default_view_ctx,
        )
        self.assertEqual("testuser2 and Test user 2", p.values["header"])

    def test_user_nocache(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {nocache=true}
I am %%username%%.
        """
        )
        self.assert_content(self.get(d.url, as_tree=True), ["I am testuser1."])

    def test_macro_only_delimiter(self):
        self.login_test1()
        doc = self.create_doc(settings={"macro_delimiter": "%%"}).document
        self.new_par(
            doc,
            "{% set a = 123456+1 %}%%a%%",
            json_key="texts",
            expect_contains="123457",
        )

    def test_null_macros(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {plugin=showVideo}
```
"""
        )
        d.document.set_settings({"macros": None})
        r = self.get(d.url, as_tree=True).cssselect(".parContent tim-video")
        self.assertTrue(r)

    def test_invalid_macros(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {plugin=showVideo}
```
"""
        )
        d.document.set_settings({"macros": "invalid"})
        self.get(d.url)
        d.document.set_settings({"macros": {"a": {"b": "c"}}})
        self.get(d.url)

    def test_doc_macros(self):
        self.login_test1()
        d = self.create_doc()
        d.document.add_paragraph("%%docid%% %%docpath%%")
        e = self.get(d.url, as_tree=True)
        self.assert_content(e, [f"{d.id} {d.path}"])

    def test_host_macros(self):
        self.login_test1()
        d = self.create_doc(initial_par="%%host%%")
        e = self.get(d.url, as_tree=True)
        self.assert_content(e, [app.config["TIM_HOST"]])

    def test_globalmacros_with_reference(self):
        self.login_test1()
        pp = self.current_user.get_personal_folder().path
        d = self.create_doc(
            f"{pp}/a/b/d1",
            initial_par="""
this is a %%x()%%
#- {plugin=csPlugin}
type: cs
stem: this is a %%x()%%
                            """,
            settings={
                "globalmacros": {"ADDFOREVERY": "{% macro x() %}cat{% endmacro %}"}
            },
        )
        self.create_doc(
            f"{pp}/a/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}",
            settings={
                "globalmacros": {"ADDFOREVERY": "{% macro x() %}law{% endmacro %}"}
            },
        )
        d2 = self.create_doc(f"{pp}/a/c/d2", initial_par="""this is the %%x()%%""")
        p = d2.document.get_paragraphs()[0].create_reference(d.document)
        d.document.add_paragraph_obj(p)
        d.document.clear_mem_cache()
        tree = self.get(d.url, as_tree=True)
        pars = get_content(tree)
        self.assertEqual(
            pars[:-2] + pars[-1::], ["", "this is a cat", "this is the law"]
        )
        plugins = tree.cssselect("cs-runner")
        self.assertEqual("this is a cat", decode_csplugin(plugins[0])["stem"])

    def test_usermacro_in_plugin(self):
        """User-specific macros in plugins."""
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=csPlugin}
type: cs
stem: Hi, %%username%%!
        """
        )
        tree = self.get(d.url, as_tree=True)
        plugins = tree.cssselect("cs-runner")
        self.assertEqual("Hi, testuser1!", decode_csplugin(plugins[0])["stem"])

    def test_addforevery_only_settings(self):
        self.login_test1()
        x = self.create_doc(self.get_personal_item_path("x/y"))
        d = self.create_preamble_for(x)
        d.document.add_text(
            """
#- {settings=""}
#{%
globalmacros:
 ADDFOREVERY: hi
        """
        )
        r = self.get(d.url, as_tree=True)
        self.assert_content(r, ["#{%\nglobalmacros:\n ADDFOREVERY: hi"])

    def test_usermacros_no_xss(self):
        script = '<script>alert("hi")</script>'
        u, _ = User.create_with_group(
            UserInfo(
                username=script,
                full_name=script,
                email=script,
            )
        )
        db.session.commit()
        self.login(username=script)
        d = self.create_doc(
            initial_par="macros: %%username%% %%realname%% %%useremail%% %%doctitle%%",
            title=script,
        )
        r = self.get(d.url)
        self.assertNotIn(script, r)
