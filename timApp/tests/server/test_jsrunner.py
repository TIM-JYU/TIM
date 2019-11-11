"""Server tests for jsrunner plugin."""
import json
from datetime import datetime, timezone

import requests

from timApp.answer.answer import Answer
from timApp.document.docinfo import DocInfo
from timApp.plugin.plugin import Plugin
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember


class JsRunnerTestBase(TimRouteTest):

    def create_jsrun(self, md):
        return self.create_doc(initial_par=rf"""
#- {{#r plugin=jsrunner}}
{md}""")

    def do_jsrun(
            self,
            d: DocInfo,
            expect_content=None,
            expect_status=200,
            user_input=None,
            **kwargs,
    ):
        if not user_input:
            user_input = {}
        return self.post_answer(
            'jsrunner',
            f'{d.id}.r',
            user_input=user_input,
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
            ('fields: []\ngroup: 0', "{'group': [['Not a valid string.'], {'_schema': ['Invalid input type.']}]}", 400),
            ('fields: []\ngroup: 1', "{'group': [['Not a valid string.'], {'_schema': ['Invalid input type.']}]}", 400),
            ('fields: []\nprogram: 1', "{'program': [['Not a valid string.'], {'_schema': ['Invalid input type.']}]}", 400),
            ('fields: []\ngroup: xxx', "The following groups were not found: xxx", 400),
            ('fields: []\ngroups: [xxx, yyy]', "The following groups were not found: xxx, yyy", 400),
            ('fields: []\ngroup: testuser1', "Attribute 'program' is required.", 400),
            # ('fields: [x.y]\ngroup: testuser1\nprogram: ""',
            #  'Invalid field access: y', 400),
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
        self.assertTrue(r['web']['fatalError']['stackTrace'].startswith('Unexpected token (1:1)\n'))

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
        self.verify_answer_content(f'{d.id}.t1', 'c', 'a', self.test_user_1, expected_count=1)

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
        self.verify_answer_content(f'{d1.id}.t1', 'c', 'al1', self.test_user_1)
        self.verify_answer_content(f'{d1.id}.t2', 'c', 'al2', self.test_user_1)
        self.verify_answer_content(f'{d2.id}.t1', 'c', 'al3', self.test_user_1)
        self.verify_answer_content(f'{d2.id}.t2', 'c', 'noalias2', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t1', 'c', 1, self.test_user_1)
        self.verify_answer_content(f'{d.id}.t2', 'c', 'noalias', self.test_user_1)
        self.do_jsrun(
            d,
        )
        self.verify_answer_content(f'{d.id}.t1', 'c', 2, self.test_user_1, expected_count=2)

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
        self.verify_answer_content(f'{d1.id}.t1', 'c', 'a', self.test_user_1)
        self.verify_answer_content(f'{d1.id}.t2', 'c', 'b', self.test_user_1)
        self.verify_answer_content(f'{d2.id}.t1', 'c', 'c', self.test_user_1)
        self.verify_answer_content(f'{d2.id}.t2', 'c', 'd', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t1', 'c', 'e', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t2', 'c', 'f', self.test_user_1)

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
        self.verify_answer_content(f'{d.id}.t01', 'c', 2, self.test_user_1)
        self.verify_answer_content(f'{d.id}.t02', 'c', 2.1, self.test_user_1)
        self.verify_answer_content(f'{d.id}.t03', 'c', 2, self.test_user_1)
        self.verify_answer_content(f'{d.id}.t04', 'c', 2.1, self.test_user_1)
        self.verify_answer_content(f'{d.id}.t08', 'c', 2, self.test_user_1)
        self.verify_answer_content(f'{d.id}.t13', 'c', 2, self.test_user_1)
        self.verify_answer_content(f'{d.id}.t17', 'c', '', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t18', 'c', 'a', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t19', 'c', '1', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t20', 'c', '1', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t21', 'c', '1.6', self.test_user_1)

    def test_rights(self):
        d = self.create_doc(initial_par="""
#- {plugin=textfield #t}
        """)
        self.test_user_2.grant_access(d, 'view')
        self.login_test2()
        d2 = self.create_jsrun(f"""
group: testuser2
fields:
- {d.id}.t
program: |!!
tools.setString("{d.id}.t", "hi");
!!
        """)
        self.do_jsrun(
            d2,
            expect_content=f'Missing teacher access for document {d.id}',
            expect_status=403,
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
        )

        # Can write own answer to another doc via jsrunner if teacher access there
        self.test_user_2.grant_access(d, 'teacher')
        self.do_jsrun(
            d2,
        )
        a = self.verify_answer_content(f'{d.id}.t', 'c', 'hi', self.test_user_2)
        self.test_user_2.remove_access(d.id, 'teacher')
        db.session.commit()

        # Can write own answer to another doc via jsrunner if view access and allow_ext
        d.document.add_setting('allow_external_jsrunner', True)
        self.test_user_2.remove_access(d.id, 'view')
        db.session.commit()
        d2 = self.create_jsrun(f"""
group: testuser2
fields: []
program: |!!
tools.setString("{d.id}.t", "hi_ext");
!!
        """)
        self.do_jsrun(
            d2,
            expect_content=f'Missing teacher access for document {d.id}',
            expect_status=403,
        )
        self.test_user_2.grant_access(d, 'view')
        self.do_jsrun(
            d2,
        )
        a = self.verify_answer_content(f'{d.id}.t', 'c', 'hi_ext', self.test_user_2, expected_count=2)

        d2 = self.create_jsrun(f"""
group: testuser1
fields: []
program: |!!
tools.setString("t", "hi");
!!
        """)
        d2.document.add_text('#- {#t plugin=textfield}')
        self.do_jsrun(
            d2,
        )
        a = self.verify_answer_content(f'{d2.id}.t', 'c', 'hi', self.test_user_2)
        self.test_user_2.groups.append(UserGroup.get_teachers_group())
        db.session.commit()
        self.do_jsrun(
            d2,
        )
        a = self.verify_answer_content(f'{d2.id}.t', 'c', 'hi', self.test_user_1, expected_count=1)
        self.assertEqual(self.test_user_2, a.saver)

        # Can use jsrunner for own group in another doc if view access and showInView: true
        self.login_test1()
        d2 = self.create_jsrun(f"""
groups: []
fields:
- t
program: |!!
tools.setString("t", "hi");
!!
        """)
        d2.document.add_text('#- {#t plugin=textfield}')
        self.test_user_2.grant_access(d2, 'view')
        d3 = self.create_jsrun(f"""
groups: []
fields:
- t
program: |!!
tools.setString("t", "hi");
!!
showInView: true
        """)
        d3.document.add_text('#- {#t plugin=textfield}')
        self.test_user_2.grant_access(d3, 'view')
        self.login_test2()
        self.do_jsrun(
            d2,
            expect_content=f'Missing teacher access for document {d2.id}',
            expect_status=403,
        )
        self.do_jsrun(
            d3,
        )
        a = self.verify_answer_content(f'{d3.id}.t', 'c', 'hi', self.test_user_2)

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

    def test_tally_fields(self):
        fd = self.create_doc(initial_par="""#- {#t plugin=textfield}""")
        self.current_user.answers.append(Answer(
            task_id=f'{fd.id}.t',
            points=2.5,
            content=json.dumps({'c': 'x'}),
            valid=True,
        ))
        db.session.commit()
        d = self.create_jsrun(f"""
fields:
 - tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:00]=total_range
 - tally:total_points[,2019-06-05 12:00:00]=total_range_end
 - tally:total_points[2018-04-06 12:00:00,]=total_range_start
 - tally:total_points=total_all
 - tally:total_points[,]=total_all_alt
 - tally:1st=a
 - tally:2nd=b
 - tally:3rd=c
 - tally:task_count
 - tally:{fd.id}.total_points=otherdoc
group: testuser1
program: |!!
tools.print(tools.getDouble("a"));
tools.print(tools.getDouble("b"));
tools.print(tools.getDouble("c"));
tools.print(tools.getDouble("total_all"));
tools.print(tools.getDouble("total_range"));
tools.print(tools.getDouble("task_count"));
tools.print(tools.getDouble("otherdoc"));
!!
                """)
        d.document.set_settings(
            {'point_sum_rule': {'groups': {
                '1st': 'a.*',
                '2nd': 'b.*',
                '3rd': 'c.*',
            },
                'count': {'best': 2}}},
        )
        d.document.add_text("""
#- {defaultplugin=textfield}
{#a01#} {#a02#} {#a03#}
{#b01#} {#b02#} {#b03#}
{#c01#} {#c02#} {#c03#}
        """)
        u = self.current_user
        for i in range(1, 4):
            for letter, num in zip(['a', 'b', 'c'], [1, 2, 3]):
                c = 10 ** (i - 1) * num
                tid = f'{d.id}.{letter}0{i}'
                a = Answer(
                    task_id=tid,
                    content=json.dumps({'c': c}),
                    points=c,
                    valid=True,
                )
                if i == 2 and letter == 'c':
                    a.answered_on = datetime(2018, 6, 20)
                u.answers.append(a)
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={'web': {'errors': [], 'outdata': {}, 'output': '111\n222\n333\n555\n30\n3\n2.5\n'}},
        )

    def test_mix_tally_and_normal_fields(self):
        c = json.dumps({'c': ''})
        f1 = self.create_doc(initial_par="#- {#t1 plugin=textfield}")
        f2 = self.create_doc(initial_par="#- {#t3 plugin=textfield}")
        curr = self.current_user
        curr.answers.append(Answer(task_id=f'{f1.id}.t1', points=1, valid=True, content=c))
        curr.answers.append(Answer(task_id=f'{f2.id}.t3', points=10, valid=True, content=c))
        db.session.commit()
        d = self.create_jsrun(f"""
groups:
 - testuser1
fields:
 - tally:{f1.id}.total_points=t11
 - {f2.id}.t3.points=d11
 - op(0,5)
 - t1
 - d1
updateFields:  
 - t1
 - d1
program: |!!
  tools.print(
    tools.getDouble("t1"),
    tools.getDouble("d1"),
  );
  tools.setDouble("t1", tools.getSum("t1", 1, 1), 0);
  tools.setDouble("d1", tools.getSum("d1", 1, 1), 0);
  tools.print(
    tools.getDouble("t1"),
    tools.getDouble("d1"),
    tools.getDouble("op0"),
    tools.getDouble("op1"),
    tools.getDouble("op2"),
    tools.getDouble("op3"),
    tools.getDouble("op4"),
  );
!!""")
        d.document.add_text("""
#- {defaultplugin=textfield}
{#op0#}
{#op1#}
{#op2#}
{#op3#}
{#op4#}
{#t1#}
{#d1#}
        """)
        for i in range(0, 5):
            self.current_user.answers.append(
                Answer(task_id=f'{d.id}.op{i}', valid=True, content=json.dumps({'c': 10 ** (i + 2)})))
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={
                'web': {
                    'errors': [],
                    'outdata': {},
                    'output': '0 0\n1 10 100 1000 10000 100000 1000000\n',
                },
            },
        )

    def test_no_points_answers_tally(self):
        d = self.create_jsrun("""
fields:
 - tally:total_points=x
group: testuser1
program: |!!
tools.print(tools.getDouble("x"));
!!
        """)
        d.document.add_text("""#- {#t plugin=textfield}""")
        # Add an answer with no points.
        self.current_user.answers.append(Answer(task_id=f'{d.id}.t', valid=True, content=json.dumps({'c': ''})))
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={'web': {'errors': [], 'outdata': {}, 'output': '0\n'}},
        )

    def test_invalid_tally_fields(self):
        invalid_yamls = [
            ('tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:00=total_range',
             "Invalid tally field format: tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:00", 400),
            ('tally:total_points[2018-04-06 12:00:00]=total_range',
             "Invalid tally field format: tally:total_points[2018-04-06 12:00:00]", 400),
            ('tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:60]=total_range',
             "Invalid tally field format: tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:60]", 400),
            ('tally:x',
             "Unknown tally field: x. Valid tally fields are: total_points, velp_points, task_points, task_count and velped_task_count.",
             400),
        ]
        d = None
        for y, e, s in invalid_yamls:
            d = self.create_jsrun(f"""
fields:
 - {y}
group: testuser1
program: |!!
!!""")
            self.do_jsrun(
                d,
                expect_content=e,
                expect_status=s,
            )
        d.document.set_settings(
            {
                'point_sum_rule': {
                    'groups': {
                        '1st': 'a.*',
                        '2nd': 'b.*',
                        '3rd': 'c.*',
                    },
                }
            }
        )
        self.do_jsrun(
            d,
            expect_content='Unknown tally field: x. Valid tally fields are: total_points, velp_points, task_points, task_count, velped_task_count, 1st, 2nd and 3rd.',
            expect_status=400,
        )

    def test_deleted_users(self):
        d = self.create_jsrun("""
fields: []
group: testusers
includeUsers: all
selectIncludeUsers: true
program: |!!
tools.print(tools.getLeaveDate());
!!""")
        ug = UserGroup.create('testusers')
        ug.current_memberships[self.test_user_1.id] = UserGroupMember(user=self.test_user_1)
        ug.current_memberships[self.test_user_2.id] = UserGroupMember(user=self.test_user_2,
                                                                      membership_end=datetime(2010, 1, 1).replace(tzinfo=timezone.utc))
        db.session.commit()
        self.do_jsrun(
            d,
            user_input={'includeUsers': 'x'},
            expect_status=400,
            expect_content={'error': "{'input': {'includeUsers': ['Invalid enum value x']}}"},
        )
        self.do_jsrun(
            d,
            user_input={'includeUsers': 'current'},
            expect_content={"web":{"output":"undefined\n","errors":[],"outdata":{}}},
        )
        self.do_jsrun(
            d,
            user_input={'includeUsers': 'all'},
            expect_content={"web":{"output":"undefined\n","errors":[],"outdata":{}}},
        )
        gd = self.create_doc()
        UserGroup.get_by_name('testusers').admin_doc = gd.block
        db.session.commit()
        self.do_jsrun(
            d,
            user_input={'includeUsers': 'all'},
            expect_content={'web': {'errors': [], 'outdata': {}, 'output': 'undefined\n1262304000\n'}},
        )
        p = d.document.get_paragraphs()[0]
        plug = Plugin.from_paragraph(p)
        plug.values['selectIncludeUsers'] = False
        plug.save()
        self.do_jsrun(
            d,
            user_input={'includeUsers': 'current'},
            expect_status=403,
            expect_content='Not allowed to select includeUsers option.',
        )
        self.do_jsrun(
            d,
            user_input={'includeUsers': 'all'},
            expect_content={'web': {'errors': [], 'outdata': {}, 'output': 'undefined\n1262304000\n'}},
        )
        plug.values.pop('selectIncludeUsers')
        plug.save()
        self.do_jsrun(
            d,
            user_input={'includeUsers': 'current'},
            expect_status=403,
            expect_content='Not allowed to select includeUsers option.',
        )
        self.do_jsrun(
            d,
            user_input={'includeUsers': 'all'},
            expect_content={'web': {'errors': [], 'outdata': {}, 'output': 'undefined\n1262304000\n'}},
        )

    def test_collaborators(self):
        d = self.create_jsrun("""
fields:
 - a
group: collabs
program: |!!
tools.print(tools.getRealName());
tools.print(tools.getString("a"));
!!""")
        d.document.add_text("""
#- {plugin=textfield #a}
        """)
        ug = UserGroup.create('collabs')
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        a = Answer(content=json.dumps({'c': 'test'}), valid=True, task_id=f'{d.id}.a')
        self.test_user_1.answers.append(a)
        self.test_user_2.answers.append(a)
        ug.admin_doc = self.create_doc().block
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={
                'web': {'errors': [],
                        'outdata': {},
                        'output': 'Test user 1\ntest\nTest user 2\ntest\n',
                        },
            },
        )


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
        self.verify_answer_content(f'{d.id}.t1', 'c', '-Test user 1', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t2', 'c', '=Test user 1', self.test_user_1)
        self.verify_answer_content(f'{d.id}.t1', 'c', '-Test user 2', self.test_user_2)
        self.verify_answer_content(f'{d.id}.t2', 'c', '=Test user 2', self.test_user_2)
        self.do_jsrun(
            d,
            expect_content={'web': {'errors': [], 'output': '', 'outdata': {}}},
        )
        self.verify_answer_content(f'{d.id}.t1', 'c', '-Test user 1-Test user 1', self.test_user_1, expected_count=2)
        self.verify_answer_content(f'{d.id}.t2', 'c', '=Test user 1=Test user 1', self.test_user_1, expected_count=2)
        self.verify_answer_content(f'{d.id}.t1', 'c', '-Test user 2-Test user 2', self.test_user_2, expected_count=2)
        self.verify_answer_content(f'{d.id}.t2', 'c', '=Test user 2=Test user 2', self.test_user_2, expected_count=2)
        self.do_jsrun(
            d,
            expect_content={'web': {'errors': [], 'output': '', 'outdata': {}}},
        )
        self.verify_answer_content(f'{d.id}.t1', 'c', '-Test user 1-Test user 1-Test user 1', self.test_user_1,
                                   expected_count=3)
        self.verify_answer_content(f'{d.id}.t2', 'c', '=Test user 1=Test user 1=Test user 1', self.test_user_1,
                                   expected_count=3)
        self.verify_answer_content(f'{d.id}.t1', 'c', '-Test user 2-Test user 2-Test user 2', self.test_user_2,
                                   expected_count=3)
        self.verify_answer_content(f'{d.id}.t2', 'c', '=Test user 2=Test user 2=Test user 2', self.test_user_2,
                                   expected_count=3)
