"""Server tests for jsrunner plugin."""
from typing import List

import requests

from timApp.answer.answer import Answer
from timApp.document.docinfo import DocInfo
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


class JsRunnerTestBase(TimRouteTest):
    def verify_content(self, task: str, content_field: str, content, u: User, expected_count=1):
        anss: List[Answer] = u.answers.filter_by(task_id=task).order_by(Answer.answered_on.desc()).all()
        self.assertEqual(expected_count, len(anss))
        first = anss[0]
        self.assertEqual(content, first.content_as_json[content_field])
        return first

    def create_jsrun(self, md):
        return self.create_doc(initial_par=rf"""
#- {{#r plugin=jsrunner}}
{md}""")

    def do_jsrun(self, d: DocInfo, expect_content=None, expect_status=200, **kwargs):
        return self.post_answer(
            'jsrunner',
            f'{d.id}.r',
            user_input={},
            expect_content=expect_content,
            expect_status=expect_status,
            **kwargs,
        )


class JsRunnerTest(JsRunnerTestBase):
    def setUp(self):
        super().setUp()
        self.login_test1()

    def test_invalid_markup(self):
        invalid_yamls = [
            ('', "{'fields': ['Missing data for required field.']}", 400),
            ('fields: []', "{'_schema': ['Either group or groups must be given.']}", 400),
            ('fields: []\ngroup: 0', "{'group': ['Not a valid string.']}", 400),
            ('fields: []\ngroup: 1', "{'group': ['Not a valid string.']}", 400),
            ('fields: []\nprogram: 1', "{'program': ['Not a valid string.']}", 400),
            ('fields: []\ngroup: xxx', "The following groups were not found: xxx", 404),
            ('fields: []\ngroups: [xxx, yyy]', "The following groups were not found: xxx, yyy", 404),
            ('fields: []\ngroup: testuser1', "Attribute 'program' is required.", 400),
            ('fields: [x.y]\ngroup: testuser1\nprogram: ""',
             'Invalid field access: y', 400),
            ('fields: []\ngroup: testuser1\nprogram: ""\ntimeout: 2000',
             {'web': {'error': 'Invalid input to jsrunner answer route.'}}, 200),
            ('fields: []\ngroup: testuser1\nprogram: ""',
             {"web": {"output": "", "errors": [], "outdata": {}}},
             200),
        ]
        for y, e, s in invalid_yamls:
            d = self.create_jsrun(y)
            self.do_jsrun(
                d,
                expect_content=e,
                expect_status=s,
                json_key='error' if s >= 400 else None,
            )


#     def test_nonexistent_field(self): # TODO: Check if obsolete
#         d = self.create_jsrun("""
# fields:
#  - y
# program: |!!
# tools.setDouble("x", 2.0)
# !!
# group: testuser1
#         """)
#         self.do_jsrun(
#             d,
#             expect_content={'web': {
#                 'error': 'Task not found: x',
#                 'output': '',
#                 'errors': [],
#             }},
#         )

    def test_nonexistent_doc(self):
        d = self.create_jsrun("""
fields:
 - 999.x
program: |!!
tools.setDouble("x", 2.0)
!!
group: testuser1
        """)
        self.do_jsrun(
            d,
            expect_content={"error": "Document 999 not found"},
            expect_status=404,
        )

    def test_infinite_loop(self):
        d = self.create_jsrun("""
fields: []
group: testuser1
timeout: 100
program: |!!
while (true) {}
!!
        """)
        self.do_jsrun(
            d,
            expect_content={'web': {'fatalError': {'msg': 'Script execution timed out.'}, 'output': ''}},
        )

    def test_syntax_error(self):
        d = self.create_jsrun("""
fields: []
group: testuser1
program: |!!
{
!!
            """)
        r = self.do_jsrun(
            d,
        )
        self.assertEqual('Unexpected end of input', r['web']['fatalError']['msg'])
        self.assertTrue(r['web']['fatalError']['stackTrace'].startswith('SyntaxError: Unexpected end of input\n'))

    def test_invalid_field_name(self):
        d = self.create_jsrun("""
fields: []
group: testuser1
program: |!!
tools.setString("1", "a");
!!
        """)
        self.do_jsrun(
            d,
            expect_content={"error": "Invalid task name: 1"},
            expect_status=400,
        )

    def test_no_multiple_saves(self):
        d = self.create_jsrun("""
fields: []
group: testuser1
program: |!!
tools.setString("t1", "a");
!!
        """)
        d.document.add_text("""
#- {plugin=textfield #t1}""")
        self.do_jsrun(
            d,
        )
        self.do_jsrun(
            d,
        )
        self.verify_content(f'{d.id}.t1', 'c', 'a', self.test_user_1, expected_count=1)

    def test_print_nonascii(self):
        d = self.create_jsrun("""
fields: []
group: testuser1
program: |!!
tools.print("häh höh håh");
!!
        """)
        self.do_jsrun(
            d,
            expect_content={'web': {'errors': [], 'output': 'häh höh håh\n', 'outdata': {}}},
        )

    def test_aliases_and_getters(self):
        d1 = self.create_doc(initial_par="""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}
                """)
        d2 = self.create_doc(initial_par="""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}""")
        d = self.create_jsrun('')
        p = d.document.get_paragraphs()[0]
        p.set_markdown(f"""
fields:
 - {d1.id}.t1=a1
 - {d1.id}.t2=a2
 - {d2.id}.t1=a3
 - {d2.id}.t2=a4
 - {d.id}.t1=a5
 - t2=a6
group: testuser1
program: |!!
tools.setString("a1", "al1");
tools.setString("a2", "al2");
tools.setString("a3", "al3");
tools.setString("a4", "al4");
tools.setDouble("a5", tools.getDouble("a5", 0) + 1);
tools.setString("a6", "al6");
tools.setString("t2", "noalias");
tools.setString("{d2.id}.t2", "noalias2");
!!
        """)
        p.save()
        d.document.add_text("""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}""")
        self.do_jsrun(
            d,
        )
        self.verify_content(f'{d1.id}.t1', 'c', 'al1', self.test_user_1)
        self.verify_content(f'{d1.id}.t2', 'c', 'al2', self.test_user_1)
        self.verify_content(f'{d2.id}.t1', 'c', 'al3', self.test_user_1)
        self.verify_content(f'{d2.id}.t2', 'c', 'noalias2', self.test_user_1)
        self.verify_content(f'{d.id}.t1', 'c', 1, self.test_user_1)
        self.verify_content(f'{d.id}.t2', 'c', 'noalias', self.test_user_1)
        self.do_jsrun(
            d,
        )
        self.verify_content(f'{d.id}.t1', 'c', 2, self.test_user_1, expected_count=2)

    def test_invalid_aliases(self):
        invalid_yamls = [
            ('- a=', "Alias cannot be empty: a=", 400),
            ('- a=b\n- c=b', "Duplicate alias b in fields attribute", 400),
            ('- a==', "Invalid alias: a==", 400),
        ]
        for y, e, s in invalid_yamls:
            d = self.create_jsrun(f"""
fields:
{y}
group: testuser1
program: ''
            """)
            self.do_jsrun(
                d,
                expect_content=e,
                expect_status=s,
                json_key='error' if s >= 400 else None,
            )

    def test_multiple_documents(self):
        d1 = self.create_doc(initial_par="""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}
        """)
        d2 = self.create_doc(initial_par="""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}""")

        d = self.create_jsrun('')
        p = d.document.get_paragraphs()[0]
        p.set_markdown(f"""
fields: []
group: testuser1
program: |!!
tools.setString("{d1.id}.t1", "a");
tools.setString("{d1.id}.t2", "b");
tools.setString("{d2.id}.t1", "c");
tools.setString("{d2.id}.t2", "d");
tools.setString("{d.id}.t1", "e");
tools.setString("t2", "f");
!!""")
        p.save()
        d.document.add_text("""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}
        """)

        self.do_jsrun(
            d,
        )
        self.verify_content(f'{d1.id}.t1', 'c', 'a', self.test_user_1)
        self.verify_content(f'{d1.id}.t2', 'c', 'b', self.test_user_1)
        self.verify_content(f'{d2.id}.t1', 'c', 'c', self.test_user_1)
        self.verify_content(f'{d2.id}.t2', 'c', 'd', self.test_user_1)
        self.verify_content(f'{d.id}.t1', 'c', 'e', self.test_user_1)
        self.verify_content(f'{d.id}.t2', 'c', 'f', self.test_user_1)

    def test_setters_and_getters(self):
        d = self.create_jsrun("""
fields: []
program: |!!
tools.setDouble("t01", 2)
tools.setDouble("t02", 2.1)
tools.setDouble("t03", "2")
tools.setDouble("t04", "2.1")
//tools.setDouble("t05", null)
//tools.setDouble("t06", undefined)
//tools.setDouble("t07", {})

tools.   setInt("t08", 2)
//tools.   setInt("t09", 2.4)
//tools.   setInt("t10", 2.5)
//tools.   setInt("t11", 2.6)
//tools.   setInt("t12", "2.4")
tools.   setInt("t13", "2")
//tools.   setInt("t14", null)
//tools.   setInt("t15", undefined)
//tools.   setInt("t16", {})

tools.setString("t17", "")
tools.setString("t18", "a")
tools.setString("t19", "1")
tools.setString("t20", 1)
tools.setString("t21", 1.6)
//tools.setString("t22", null)
//tools.setString("t23", undefined)
//tools.setString("t24", {})
!!
group: testuser1
        """)
        d.document.add_text("""
#- {defaultplugin=textfield}
{% for i in range(1, 25) %}
{#t%%'%02d'|format(i)%%#}
{% endfor %}
""")
        self.do_jsrun(
            d,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}, }},
        )
        self.verify_content(f'{d.id}.t01', 'c', 2, self.test_user_1)
        self.verify_content(f'{d.id}.t02', 'c', 2.1, self.test_user_1)
        self.verify_content(f'{d.id}.t03', 'c', 2, self.test_user_1)
        self.verify_content(f'{d.id}.t04', 'c', 2.1, self.test_user_1)
        self.verify_content(f'{d.id}.t08', 'c', 2, self.test_user_1)
        self.verify_content(f'{d.id}.t13', 'c', 2, self.test_user_1)
        self.verify_content(f'{d.id}.t17', 'c', '', self.test_user_1)
        self.verify_content(f'{d.id}.t18', 'c', 'a', self.test_user_1)
        self.verify_content(f'{d.id}.t19', 'c', '1', self.test_user_1)
        self.verify_content(f'{d.id}.t20', 'c', '1', self.test_user_1)
        self.verify_content(f'{d.id}.t21', 'c', '1.6', self.test_user_1)

    def test_rights(self):
        d = self.create_doc(initial_par="""
#- {plugin=textfield #t}
        """)
        self.test_user_2.grant_access(d.id, 'view')
        self.login_test2()
        d2 = self.create_jsrun(f"""
group: testuser2
fields:
- {d.id}.t
        """)
        self.do_jsrun(
            d2,
            expect_content=f'Missing teacher access for document {d.id}',
            expect_status=403,
            json_key='error',
        )
        d2 = self.create_jsrun(f"""
group: testuser2
fields: []
program: |!!
tools.setString("{d.id}.t", "hi");
!!
        """)
        self.do_jsrun(
            d2,
            expect_content=f'Missing teacher access for document {d.id}',
            expect_status=403,
            json_key='error',
        )

        d2 = self.create_jsrun(f"""
group: testuser1
fields: []
program: |!!
tools.setString("t", "hi");
!!
        """)
        d2.document.add_text('#- {#t plugin=textfield}')
        # TODO: Update this; current get_fields_and_users:
        #  "if no access, give at least own group"
        # self.do_jsrun(
        #     d2,
        #     expect_content=f'Missing view access for group testuser1',
        #     expect_status=403,
        #     json_key='error',
        # )
        self.test_user_2.groups.append(UserGroup.get_teachers_group())
        db.session.commit()
        self.do_jsrun(
            d2,
        )
        a = self.verify_content(f'{d2.id}.t', 'c', 'hi', self.test_user_1)
        self.assertEqual(self.test_user_2, a.saver)

    def test_runscript(self):
        runscript_url = 'http://jsrunner:5000/runScript'
        r = requests.post(runscript_url)
        self.assertEqual(400, r.status_code)
        self.assertEqual({'error': 'Invalid input to jsrunner runScript route.'}, r.json())

        r = requests.post(
            runscript_url,
            json={
                'code': """
            const result = {x: data.a + data.b, y: data.a - data.b};
            return result;
            """,
                'data': {'a': 7, 'b': 11}})
        self.assertEqual(200, r.status_code)
        self.assertEqual({'result': {'x': 18, 'y': -4}, 'output': ''}, r.json())

        r = requests.post(
            runscript_url,
            json={
                'timeout': 10,
                'code': """while(true){}""",
                'data': {}})
        self.assertEqual(200, r.status_code)
        self.assertEqual({'error': 'Script execution timed out.'}, r.json())

        r = requests.post(
            runscript_url,
            json={
                'timeout': 10000,
                'code': """while(true){}""",
                'data': {}})
        self.assertEqual(400, r.status_code)
        self.assertEqual({'error': 'Invalid input to jsrunner runScript route.'}, r.json())

        r = requests.post(
            runscript_url,
            json={
                'code': """{""",
                'data': {}})
        self.assertEqual(200, r.status_code)
        self.assertEqual({'error': 'Unexpected end of input [script.js:18:28]\n<pre>\n11: {\n</pre>\n'}, r.json())

        r = requests.post(
            runscript_url,
            json={
                'code': """
                class X {}
                return X;
                """,
                'data': {}})
        self.assertEqual(200, r.status_code)
        self.assertEqual({'error': 'Script failed to return anything (the return value must be JSON serializable).'},
                         r.json())


class JsRunnerGroupTest(JsRunnerTestBase):
    def test_jsrunner_group(self):
        self.login_test1()
        d = self.create_jsrun("""
fields: [t1, t2]
group: testusers
program: |!!
tools.setString("t1", tools.getString("t1", "") + "-" + tools.getRealName());
tools.setString("t2", tools.getString("t2", "") + "=" + tools.getRealName());
!!
        """)
        d.document.add_text("""
#- {#t1 plugin=textfield#}
#- {#t2 plugin=textfield#}
        """)
        ug = UserGroup.create('testusers')
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        ug.users.append(self.test_user_3)
        self.test_user_1.groups.append(UserGroup.get_teachers_group())
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={'web': {'errors': [], 'output': '', 'outdata': {}}},
        )
        self.verify_content(f'{d.id}.t1', 'c', '-Test user 1', self.test_user_1)
        self.verify_content(f'{d.id}.t2', 'c', '=Test user 1', self.test_user_1)
        self.verify_content(f'{d.id}.t1', 'c', '-Test user 2', self.test_user_2)
        self.verify_content(f'{d.id}.t2', 'c', '=Test user 2', self.test_user_2)
        self.do_jsrun(
            d,
            expect_content={'web': {'errors': [], 'output': '', 'outdata': {}}},
        )
        self.verify_content(f'{d.id}.t1', 'c', '-Test user 1-Test user 1', self.test_user_1, expected_count=2)
        self.verify_content(f'{d.id}.t2', 'c', '=Test user 1=Test user 1', self.test_user_1, expected_count=2)
        self.verify_content(f'{d.id}.t1', 'c', '-Test user 2-Test user 2', self.test_user_2, expected_count=2)
        self.verify_content(f'{d.id}.t2', 'c', '=Test user 2=Test user 2', self.test_user_2, expected_count=2)
        self.do_jsrun(
            d,
            expect_content={'web': {'errors': [], 'output': '', 'outdata': {}}},
        )
        self.verify_content(f'{d.id}.t1', 'c', '-Test user 1-Test user 1-Test user 1', self.test_user_1,
                            expected_count=3)
        self.verify_content(f'{d.id}.t2', 'c', '=Test user 1=Test user 1=Test user 1', self.test_user_1,
                            expected_count=3)
        self.verify_content(f'{d.id}.t1', 'c', '-Test user 2-Test user 2-Test user 2', self.test_user_2,
                            expected_count=3)
        self.verify_content(f'{d.id}.t2', 'c', '=Test user 2=Test user 2=Test user 2', self.test_user_2,
                            expected_count=3)
