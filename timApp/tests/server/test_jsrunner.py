"""Server tests for jsrunner plugin."""
import json
from datetime import datetime, timezone

import requests
from sqlalchemy import func, select

from timApp.answer.answer import Answer
from timApp.auth.accesstype import AccessType
from timApp.document.docinfo import DocInfo
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.plugin import Plugin
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember


class JsRunnerTestBase(TimRouteTest):
    def create_jsrun(self, md):
        return self.create_doc(
            initial_par=rf"""
#- {{#r plugin=jsrunner}}
{md}"""
        )

    def do_jsrun(
        self,
        d: DocInfo,
        expect_content=None,
        expect_status=200,
        user_input=None,
        runner_name="r",
        **kwargs,
    ):
        if not user_input:
            user_input = {}
        return self.post_answer(
            "jsrunner",
            f"{d.id}.{runner_name}",
            user_input=user_input,
            expect_content=expect_content,
            expect_status=expect_status,
            **kwargs,
        )

    def create_group_jsrun(self, grouplist, group="tg1", method="setGroup"):
        d = self.create_jsrun(
            f"""
fields: []
group: testuser1
program: ''
postprogram: |!!
tools.{method}("{group}", {grouplist});
!!"""
        )
        return d


class JsRunnerTest(JsRunnerTestBase):
    def setUp(self):
        super().setUp()
        self.login_test1()

    def test_invalid_markup(self):
        invalid_yamls = [
            ("", "{'fields': ['Missing data for required field.']}", 400),
            (
                "fields: []",
                "{'_schema': ['Either group or groups must be given.']}",
                400,
            ),
            (
                "fields: []\ngroup: 0",
                "{'group': [['Not a valid string.'], {'_schema': ['Invalid input type.']}]}",
                400,
            ),
            (
                "fields: []\ngroup: 1",
                "{'group': [['Not a valid string.'], {'_schema': ['Invalid input type.']}]}",
                400,
            ),
            (
                "fields: []\nprogram: 1",
                "{'program': [['Not a valid string.'], {'_schema': ['Invalid input type.']}]}",
                400,
            ),
            ("fields: []\ngroup: xxx", "The following groups were not found: xxx", 400),
            (
                "fields: []\ngroups: [xxx, yyy]",
                "The following groups were not found: xxx, yyy",
                400,
            ),
            ("fields: []\ngroup: testuser1", "Attribute 'program' is required.", 400),
            # ('fields: [x.y]\ngroup: testuser1\nprogram: ""',
            #  'Invalid field access: y', 400),
            (
                'fields: []\ngroup: testuser1\nprogram: ""\ntimeout: 4500',
                {
                    "web": {
                        "error": "Invalid inputs to jsrunner answer route",
                        "invalidInputs": [
                            {
                                "actual": 4500,
                                "key": "timeout",
                                "type": "(number | <function1>)",
                            }
                        ],
                    }
                },
                200,
            ),
            (
                'fields: []\ngroup: testuser1\nprogram: ""',
                {"web": {"output": "", "errors": [], "outdata": {}}},
                200,
            ),
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
        d = self.create_jsrun(
            """
fields:
 - 999.x
program: |!!
tools.setDouble("x", 2.0)
!!
group: testuser1
        """
        )
        self.do_jsrun(
            d,
            expect_content={"error": "Document 999 not found"},
            expect_status=404,
        )

    def test_infinite_loop(self):
        d = self.create_jsrun(
            """
fields: []
group: testuser1
timeout: 100
program: |!!
while (true) {}
!!
        """
        )
        self.do_jsrun(
            d,
            expect_content={
                "web": {
                    "fatalError": {"msg": "Script execution timed out."},
                    "output": "",
                }
            },
        )

    def test_syntax_error(self):
        d = self.create_jsrun(
            """
fields: []
group: testuser1
program: |!!
{
!!
            """
        )
        r = self.do_jsrun(
            d,
        )
        self.assertEqual("Unexpected end of input", r["web"]["fatalError"]["msg"])
        self.assertTrue(
            r["web"]["fatalError"]["stackTrace"].startswith("Unexpected token (1:1)\n")
        )

    def test_invalid_field_name(self):
        d = self.create_jsrun(
            """
fields: []
group: testuser1
program: |!!
tools.setString("1", "a");
!!
        """
        )
        self.do_jsrun(
            d,
            expect_content={"error": "Invalid task name: 1"},
            expect_status=400,
        )

    def test_no_multiple_saves(self):
        d = self.create_jsrun(
            """
fields: []
group: testuser1
program: |!!
tools.setString("t1", "a");
!!
        """
        )
        d.document.add_text(
            """
#- {plugin=textfield #t1}"""
        )
        self.do_jsrun(
            d,
        )
        self.do_jsrun(
            d,
        )
        self.verify_answer_content(
            f"{d.id}.t1", "c", "a", self.test_user_1, expected_count=1
        )

    def test_print_nonascii(self):
        d = self.create_jsrun(
            """
fields: []
group: testuser1
program: |!!
tools.print("häh höh håh");
!!
        """
        )
        self.do_jsrun(
            d,
            expect_content={
                "web": {"errors": [], "output": "häh höh håh\n", "outdata": {}}
            },
        )

    def test_aliases_and_getters(self):
        d1 = self.create_doc(
            initial_par="""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}
                """
        )
        d2 = self.create_doc(
            initial_par="""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}"""
        )
        d = self.create_jsrun("")
        p = d.document.get_paragraphs()[0]
        p.set_markdown(
            f"""
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
        """
        )
        p.save()
        d.document.add_text(
            """
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}"""
        )
        self.do_jsrun(
            d,
        )
        self.verify_answer_content(f"{d1.id}.t1", "c", "al1", self.test_user_1)
        self.verify_answer_content(f"{d1.id}.t2", "c", "al2", self.test_user_1)
        self.verify_answer_content(f"{d2.id}.t1", "c", "al3", self.test_user_1)
        self.verify_answer_content(f"{d2.id}.t2", "c", "noalias2", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t1", "c", 1, self.test_user_1)
        self.verify_answer_content(f"{d.id}.t2", "c", "noalias", self.test_user_1)
        self.do_jsrun(
            d,
        )
        self.verify_answer_content(
            f"{d.id}.t1", "c", 2, self.test_user_1, expected_count=2
        )

    def test_invalid_aliases(self):
        invalid_yamls = [
            ("- a=", "Alias cannot be empty: a=", 400),
            ("- a=b\n- c=b", "Duplicate alias b in fields attribute", 400),
            ("- a==", "Invalid alias: a==", 400),
        ]
        for y, e, s in invalid_yamls:
            d = self.create_jsrun(
                f"""
fields:
{y}
group: testuser1
program: ''
            """
            )
            self.do_jsrun(
                d,
                expect_content=e,
                expect_status=s,
            )

    def test_multiple_documents(self):
        d1 = self.create_doc(
            initial_par="""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}
        """
        )
        d2 = self.create_doc(
            initial_par="""
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}"""
        )

        d = self.create_jsrun("")
        p = d.document.get_paragraphs()[0]
        p.set_markdown(
            f"""
fields: []
group: testuser1
program: |!!
tools.setString("{d1.id}.t1", "a");
tools.setString("{d1.id}.t2", "b");
tools.setString("{d2.id}.t1", "c");
tools.setString("{d2.id}.t2", "d");
tools.setString("{d.id}.t1", "e");
tools.setString("t2", "f");
!!"""
        )
        p.save()
        d.document.add_text(
            """
#- {plugin=textfield #t1}
#- {plugin=textfield #t2}
        """
        )

        self.do_jsrun(
            d,
        )
        self.verify_answer_content(f"{d1.id}.t1", "c", "a", self.test_user_1)
        self.verify_answer_content(f"{d1.id}.t2", "c", "b", self.test_user_1)
        self.verify_answer_content(f"{d2.id}.t1", "c", "c", self.test_user_1)
        self.verify_answer_content(f"{d2.id}.t2", "c", "d", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t1", "c", "e", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t2", "c", "f", self.test_user_1)

    def test_setters_and_getters(self):
        d = self.create_jsrun(
            """
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
        """
        )
        d.document.add_text(
            """
#- {defaultplugin=textfield}
{% for i in range(1, 25) %}
{#t%%'%02d'|format(i)%%#}
{% endfor %}
"""
        )
        self.do_jsrun(
            d,
            expect_content={
                "web": {
                    "output": "",
                    "errors": [],
                    "outdata": {},
                }
            },
        )
        self.verify_answer_content(f"{d.id}.t01", "c", 2, self.test_user_1)
        self.verify_answer_content(f"{d.id}.t02", "c", 2.1, self.test_user_1)
        self.verify_answer_content(f"{d.id}.t03", "c", 2, self.test_user_1)
        self.verify_answer_content(f"{d.id}.t04", "c", 2.1, self.test_user_1)
        self.verify_answer_content(f"{d.id}.t08", "c", 2, self.test_user_1)
        self.verify_answer_content(f"{d.id}.t13", "c", 2, self.test_user_1)
        self.verify_answer_content(f"{d.id}.t17", "c", "", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t18", "c", "a", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t19", "c", "1", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t20", "c", "1", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t21", "c", "1.6", self.test_user_1)

    def test_rights(self):
        d = self.create_doc(
            initial_par="""
#- {plugin=textfield #t}
        """
        )
        self.test_user_2.grant_access(d, AccessType.view)
        self.commit_db()
        self.login_test2()
        d2 = self.create_jsrun(
            f"""
group: testuser2
fields:
- {d.id}.t
program: |!!
tools.setString("{d.id}.t", "hi");
!!
        """
        )
        self.do_jsrun(
            d2,
            expect_content=f"Missing teacher access for document {d.id}",
            expect_status=403,
        )
        d2 = self.create_jsrun(
            f"""
group: testuser2
fields: []
program: |!!
tools.setString("{d.id}.t", "hi");
!!
        """
        )
        self.do_jsrun(
            d2,
            expect_content=f"Missing teacher access for document {d.id}",
            expect_status=403,
        )

        # Can write own answer to another doc via jsrunner if teacher access there
        self.test_user_2.grant_access(d, AccessType.teacher)
        db.session.commit()
        self.do_jsrun(
            d2,
        )
        a = self.verify_answer_content(f"{d.id}.t", "c", "hi", self.test_user_2)
        self.test_user_2.remove_access(d.id, "teacher")
        db.session.commit()

        # Can write own answer to another doc via jsrunner if view access and allow_ext
        d.document.add_setting("allow_external_jsrunner", True)
        self.test_user_2.remove_access(d.id, "view")
        db.session.commit()
        d2 = self.create_jsrun(
            f"""
group: testuser2
fields: []
program: |!!
tools.setString("{d.id}.t", "hi_ext");
!!
        """
        )
        self.do_jsrun(
            d2,
            expect_content=f"Missing teacher access for document {d.id}",
            expect_status=403,
        )
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.do_jsrun(
            d2,
        )
        a = self.verify_answer_content(
            f"{d.id}.t", "c", "hi_ext", self.test_user_2, expected_count=2
        )

        d2 = self.create_jsrun(
            f"""
group: testuser1
fields: []
program: |!!
tools.setString("t", "hi");
!!
        """
        )
        d2.document.add_text("#- {#t plugin=textfield}")
        self.do_jsrun(
            d2,
        )
        u2 = self.test_user_2
        a = self.verify_answer_content(f"{d2.id}.t", "c", "hi", u2)
        u2.groups.append(UserGroup.get_teachers_group())
        db.session.commit()
        self.do_jsrun(
            d2,
        )
        a = self.verify_answer_content(
            f"{d2.id}.t", "c", "hi", self.test_user_1, expected_count=1
        )
        self.assertEqual(self.test_user_2, a.saver)

        # Can use jsrunner for own group in another doc if view access and showInView: true
        self.login_test1()
        d2 = self.create_jsrun(
            f"""
groups: []
fields:
- t
program: |!!
tools.setString("t", "hi");
!!
        """
        )
        d2.document.add_text("#- {#t plugin=textfield}")
        self.test_user_2.grant_access(d2, AccessType.view)
        db.session.commit()
        d3 = self.create_jsrun(
            f"""
groups: []
fields:
- t
program: |!!
tools.setString("t", "hi");
!!
showInView: true
        """
        )
        d3.document.add_text("#- {#t plugin=textfield}")
        self.test_user_2.grant_access(d3, AccessType.view)
        db.session.commit()
        self.login_test2()
        self.do_jsrun(
            d2,
            expect_content=f"Missing teacher access for document {d2.id}",
            expect_status=403,
        )
        self.do_jsrun(
            d3,
        )
        a = self.verify_answer_content(f"{d3.id}.t", "c", "hi", self.test_user_2)

    def test_runscript(self):
        runscript_url = "http://jsrunner:5000/runScript"
        r = requests.post(runscript_url)
        self.assertEqual(400, r.status_code)
        self.assertEqual(
            {"error": "Invalid input to jsrunner runScript route."}, r.json()
        )

        r = requests.post(
            runscript_url,
            json={
                "code": """
            const result = {x: data.a + data.b, y: data.a - data.b};
            return result;
            """,
                "data": {"a": 7, "b": 11},
            },
        )
        self.assertEqual(200, r.status_code)
        self.assertEqual({"result": {"x": 18, "y": -4}, "output": ""}, r.json())

        r = requests.post(
            runscript_url, json={"timeout": 10, "code": """while(true){}""", "data": {}}
        )
        self.assertEqual(200, r.status_code)
        self.assertEqual({"error": "Script execution timed out."}, r.json())

        r = requests.post(
            runscript_url,
            json={"timeout": 10000, "code": """while(true){}""", "data": {}},
        )
        self.assertEqual(400, r.status_code)
        self.assertEqual(
            {"error": "Invalid input to jsrunner runScript route."}, r.json()
        )

        r = requests.post(runscript_url, json={"code": """{""", "data": {}})
        self.assertEqual(200, r.status_code)
        self.assertEqual(
            {
                "error": "Unexpected end of input [script.js:18:28]\n<pre>\n11: {\n</pre>\n"
            },
            r.json(),
        )

        r = requests.post(
            runscript_url,
            json={
                "code": """
                class X {}
                return X;
                """,
                "data": {},
            },
        )
        self.assertEqual(200, r.status_code)
        self.assertEqual(
            {
                "error": "Script failed to return anything (the return value must be JSON serializable)."
            },
            r.json(),
        )

    def test_tally_count_style(self):
        self.login_test1()
        fd = self.create_doc(initial_par="""#- {#t plugin=textfield}""")
        self.current_user.answers.append(
            Answer(
                task_id=f"{fd.id}.t",
                points=1,
                content=json.dumps({"c": "x"}),
                valid=True,
            )
        )
        self.current_user.answers.append(
            Answer(
                task_id=f"{fd.id}.t",
                points=2,
                content=json.dumps({"c": "y"}),
                valid=False,
            )
        )
        db.session.commit()
        d = self.create_jsrun(
            f"""
fields:
 - tally:{fd.id}.total_points=total_all
 - tally:count_only_valid:{fd.id}.total_points=total_all_valid
 - tally:count_all:{fd.id}.total_points=total_all_any
group: testuser1
program: |!!
tools.print(tools.getDouble("total_all"));
tools.print(tools.getDouble("total_all_valid"));
tools.print(tools.getDouble("total_all_any"));
!!
                        """
        )
        self.do_jsrun(
            d,
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "1\n1\n2\n",
                }
            },
        )

    def test_tally_fields(self):
        fd = self.create_doc(initial_par="""#- {#t plugin=textfield}""")
        self.current_user.answers.append(
            Answer(
                task_id=f"{fd.id}.t",
                points=2.5,
                content=json.dumps({"c": "x"}),
                valid=True,
            )
        )
        db.session.commit()
        d = self.create_jsrun(
            f"""
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
                """
        )
        d.document.set_settings(
            {
                "point_sum_rule": {
                    "groups": {
                        "1st": "a.*",
                        "2nd": "b.*",
                        "3rd": "c.*",
                    },
                    "count": {"best": 2},
                }
            },
        )
        d.document.add_text(
            """
#- {defaultplugin=textfield}
{#a01#} {#a02#} {#a03#}
{#b01#} {#b02#} {#b03#}
{#c01#} {#c02#} {#c03#}
        """
        )
        u = self.current_user
        for i in range(1, 4):
            for letter, num in zip(["a", "b", "c"], [1, 2, 3]):
                c = 10 ** (i - 1) * num
                tid = f"{d.id}.{letter}0{i}"
                a = Answer(
                    task_id=tid,
                    content=json.dumps({"c": c}),
                    points=c,
                    valid=True,
                )
                if i == 2 and letter == "c":
                    a.answered_on = datetime(2018, 6, 20)
                u.answers.append(a)
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "111\n222\n333\n555\n30\n3\n2.5\n",
                }
            },
        )

    def test_plugininfo_fields(self):
        fd = self.create_doc(
            initial_par="""
#- {#foo1 plugin=textfield}

#- {#foo2 plugin=textfield}

#- {#bar1 plugin=textfield}

#- {#bar2 plugin=textfield}

#- {#bar3 plugin=textfield}
"""
        )
        fd.document.set_settings(
            {
                "point_sum_rule": {
                    "groups": {
                        "foos": "foo.*",
                        "bars": "bar.*",
                    },
                }
            },
        )
        db.session.commit()

        d = self.create_jsrun(
            f"""
        fields:
         - plugininfo:{fd.id}.foos.count=foocount
         - plugininfo:{fd.id}.bars.count=barcount
         - plugininfo:count=thiscount
         - plugininfo:{fd.id}.count=fdcount
         - plugininfo:{fd.id}.task_names=fdnames
         - plugininfo:{fd.id}.task_ids=fdids
        group: testuser1
        program: |!!
        tools.print(tools.getInt("foocount"));
        tools.print(tools.getInt("barcount"));
        tools.print(tools.getInt("thiscount"));
        tools.print(tools.getInt("fdcount"));
        tools.print(tools.getValue("fdnames"));
        tools.print(tools.getValue("fdids"));
        !!"""
        )
        d.document.add_text(
            """
#- {defaultplugin=textfield}
{#a01#} {#a02#} {#a03#}
{#b01#} {#b02#} {#b03#}
{#c01#} {#c02#} {#c03#}
        """
        )
        db.session.commit()

        self.do_jsrun(
            d,
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "2\n"
                    "3\n"
                    "10\n"
                    "5\n"
                    '["foo1","foo2","bar1","bar2","bar3"]\n'
                    f'["{fd.id}.foo1","{fd.id}.foo2","{fd.id}.bar1","{fd.id}.bar2","{fd.id}.bar3"]\n',
                }
            },
        )

    def test_mix_tally_and_normal_fields(self):
        c = json.dumps({"c": ""})
        f1 = self.create_doc(initial_par="#- {#t1 plugin=textfield}")
        f2 = self.create_doc(initial_par="#- {#t3 plugin=textfield}")
        curr = self.current_user
        curr.answers.append(
            Answer(task_id=f"{f1.id}.t1", points=1, valid=True, content=c)
        )
        curr.answers.append(
            Answer(task_id=f"{f2.id}.t3", points=10, valid=True, content=c)
        )
        db.session.commit()
        d = self.create_jsrun(
            f"""
groups:
 - testuser1
fields:
 - tally:{f1.id}.total_points=t11
 - {f2.id}.t3.points=d11
 - op(0,5)
 - t1
 - op1.count
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
    tools.getValue("op1.count"),
  );
!!"""
        )
        d.document.add_text(
            """
#- {defaultplugin=textfield}
{#op0#}
{#op1#}
{#op2#}
{#op3#}
{#op4#}
{#t1#}
{#d1#}
        """
        )
        for i in range(0, 5):
            self.current_user.answers.append(
                Answer(
                    task_id=f"{d.id}.op{i}",
                    valid=True,
                    content=json.dumps({"c": 10 ** (i + 2)}),
                )
            )
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "0 0\n1 10 100 1000 10000 100000 1000000 1\n",
                },
            },
        )

    def test_no_points_answers_tally(self):
        d = self.create_jsrun(
            """
fields:
 - tally:total_points=x
group: testuser1
program: |!!
tools.print(tools.getDouble("x"));
!!
        """
        )
        d.document.add_text("""#- {#t plugin=textfield}""")
        # Add an answer with no points.
        self.current_user.answers.append(
            Answer(task_id=f"{d.id}.t", valid=True, content=json.dumps({"c": ""}))
        )
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={"web": {"errors": [], "outdata": {}, "output": "0\n"}},
        )

    def test_invalid_tally_fields(self):
        invalid_yamls = [
            (
                "tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:00=total_range",
                "Invalid tally field format: tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:00",
                400,
            ),
            (
                "tally:total_points[2018-04-06 12:00:00]=total_range",
                "Invalid tally field format: tally:total_points[2018-04-06 12:00:00]",
                400,
            ),
            (
                "tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:60]=total_range",
                "Invalid tally field format: tally:total_points[2018-04-06 12:00:00, 2019-06-05 12:00:60]",
                400,
            ),
            (
                "tally:x",
                "Unknown tally field: x (full name: {}.x). Valid tally fields are: total_points, velp_points, task_points, task_count, velped_task_count, first_answer_on, last_answer_on and answer_duration.",
                400,
            ),
        ]
        d = None
        for y, e, s in invalid_yamls:
            d = self.create_jsrun(
                f"""
fields:
 - {y}
group: testuser1
program: |!!
!!"""
            )
            self.do_jsrun(
                d,
                expect_content=e.format(d.id),
                expect_status=s,
            )
        d.document.set_settings(
            {
                "point_sum_rule": {
                    "groups": {
                        "1st": "a.*",
                        "2nd": "b.*",
                        "3rd": "c.*",
                    },
                }
            }
        )
        self.do_jsrun(
            d,
            expect_content=f"Unknown tally field: x (full name: {d.id}.x). Valid tally fields are: total_points, velp_points, task_points, task_count, velped_task_count, first_answer_on, last_answer_on, answer_duration, 1st, 2nd and 3rd.",
            expect_status=400,
        )

    def test_deleted_users(self):
        d = self.create_jsrun(
            """
fields: []
group: testusers
includeUsers: all
selectIncludeUsers: true
program: |!!
tools.print(tools.getLeaveDate());
!!"""
        )
        ug = UserGroup.create("testusers")
        ug.current_memberships[self.test_user_1.id] = UserGroupMember(
            user=self.test_user_1
        )
        ug.current_memberships[self.test_user_2.id] = UserGroupMember(
            user=self.test_user_2,
            membership_end=datetime(2010, 1, 1).replace(tzinfo=timezone.utc),
        )
        db.session.commit()
        self.do_jsrun(
            d,
            user_input={"includeUsers": "x"},
            expect_status=400,
            expect_content={
                "error": "{'input': {'includeUsers': [['Invalid enum value x'], {'_schema': "
                "['Invalid input type.']}]}}"
            },
        )
        self.do_jsrun(
            d,
            user_input={"includeUsers": "current"},
            expect_content={"web": {"output": "null\n", "errors": [], "outdata": {}}},
        )
        self.do_jsrun(
            d,
            user_input={"includeUsers": "all"},
            expect_content={"web": {"output": "null\n", "errors": [], "outdata": {}}},
        )
        gd = self.create_doc()
        UserGroup.get_by_name("testusers").admin_doc = gd.block
        db.session.commit()
        self.do_jsrun(
            d,
            user_input={"includeUsers": "all"},
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "null\n1262304000\n",
                }
            },
        )
        p = d.document.get_paragraphs()[0]
        plug = Plugin.from_paragraph(p, default_view_ctx)
        plug.values["selectIncludeUsers"] = False
        plug.save()
        self.do_jsrun(
            d,
            user_input={"includeUsers": "current"},
            expect_status=403,
            expect_content="Not allowed to select includeUsers option.",
        )
        self.do_jsrun(
            d,
            user_input={"includeUsers": "all"},
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "null\n1262304000\n",
                }
            },
        )
        plug.values.pop("selectIncludeUsers")
        plug.save()
        self.do_jsrun(
            d,
            user_input={"includeUsers": "current"},
            expect_status=403,
            expect_content="Not allowed to select includeUsers option.",
        )
        self.do_jsrun(
            d,
            user_input={"includeUsers": "all"},
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "null\n1262304000\n",
                }
            },
        )

    def test_add_time_log(self):
        d = self.create_jsrun(
            """
fields: []
group: testusers_addtest
includeUsers: all
selectIncludeUsers: true
program: |!!
tools.print(tools.getAddDate());
!!"""
        )

        ug = UserGroup.create("testusers_addtest")
        ug.current_memberships[self.test_user_1.id] = UserGroupMember(
            user=self.test_user_1,
            membership_added=datetime(2010, 1, 1).replace(tzinfo=timezone.utc),
        )
        ug.current_memberships[self.test_user_2.id] = UserGroupMember(
            user=self.test_user_2,
            membership_added=datetime(2020, 1, 1).replace(tzinfo=timezone.utc),
            membership_end=datetime(2020, 2, 1).replace(tzinfo=timezone.utc),
        )
        gd = self.create_doc()
        UserGroup.get_by_name("testusers_addtest").admin_doc = gd.block
        db.session.commit()
        self.do_jsrun(
            d,
            user_input={"includeUsers": "current"},
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "1262304000\n",
                }
            },
        )
        self.do_jsrun(
            d,
            user_input={"includeUsers": "all"},
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "1262304000\n1577836800\n",
                }
            },
        )

    def test_collaborators(self):
        d = self.create_jsrun(
            """
fields:
 - a
group: collabs
program: |!!
tools.print(tools.getRealName());
tools.print(tools.getString("a"));
!!"""
        )
        d.document.add_text(
            """
#- {plugin=textfield #a}
        """
        )
        self.create_test_group("collabs")
        a = Answer(content=json.dumps({"c": "test"}), valid=True, task_id=f"{d.id}.a")
        self.test_user_1.answers.append(a)
        self.test_user_2.answers.append(a)
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={
                "web": {
                    "errors": [],
                    "outdata": {},
                    "output": "Test user 1\ntest\nTest user 2\ntest\n",
                },
            },
        )

    def create_test_group(self, ugname: str):
        ug = UserGroup.create(ugname)
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        ug.admin_doc = self.create_doc().block
        return ug

    def test_global_field(self):
        d = self.create_jsrun(
            """
fields:
 - GLO_a
group: tg2
program: |!!
tools.setString("GLO_a", tools.getString("GLO_a") + "b")
!!
        """
        )
        d.document.add_text(
            """
#- {plugin=textfield #GLO_a}
                """
        )
        self.create_test_group("tg2")
        a = Answer(content=json.dumps({"c": "a"}), valid=True, task_id=f"{d.id}.GLO_a")
        self.test_user_1.answers.append(a)
        db.session.commit()
        total_answer_count_before = db.session.scalar(select(func.count(Answer.id)))
        self.do_jsrun(d)
        total_answer_count_after = db.session.scalar(select(func.count(Answer.id)))
        self.assertEqual(1, total_answer_count_after - total_answer_count_before)
        self.verify_answer_content(
            f"{d.id}.GLO_a", "c", "ab", self.test_user_1, expected_count=2
        )
        self.verify_answer_content(
            f"{d.id}.GLO_a", "c", "ab", self.test_user_2, expected_count=0
        )

    def test_styles(self):
        d = self.create_jsrun(
            """
fields: []
group: testuser1
program: |!!
tools.setString("t.styles", JSON.stringify({backgroundColor: "red"}));
!!
                """
        )
        d.document.add_text(
            """
#- {plugin=textfield #t}

#- {plugin=jsrunner #r2}
fields: []
group: testuser1
program: |!!
tools.setString("t", "x");
!!
#- {plugin=jsrunner #r3}
fields: []
group: testuser1
program: |!!
tools.setDouble("t", 0);
!!
#- {plugin=jsrunner #r4}
fields: []
group: testuser1
program: |!!
tools.setString("t.styles", JSON.stringify({backgroundColor: "green"}));
!!
#- {plugin=jsrunner #r5}
fields: []
group: testuser1
program: |!!
tools.setString("t.styles", "");
!!
"""
        )
        self.do_jsrun(d)
        self.verify_answer_content(
            f"{d.id}.t",
            None,
            {"styles": {"backgroundColor": "red"}, "c": None},
            self.test_user_1,
        )
        self.do_jsrun(d)
        self.verify_answer_content(
            f"{d.id}.t",
            None,
            {"styles": {"backgroundColor": "red"}, "c": None},
            self.test_user_1,
        )
        self.do_jsrun(d, runner_name="r2")
        self.verify_answer_content(
            f"{d.id}.t",
            None,
            {"styles": {"backgroundColor": "red"}, "c": "x"},
            self.test_user_1,
            expected_count=2,
        )
        self.do_jsrun(d)
        self.verify_answer_content(
            f"{d.id}.t",
            None,
            {"styles": {"backgroundColor": "red"}, "c": "x"},
            self.test_user_1,
            expected_count=2,
        )
        self.do_jsrun(d, runner_name="r3")
        self.verify_answer_content(
            f"{d.id}.t",
            None,
            {"styles": {"backgroundColor": "red"}, "c": 0},
            self.test_user_1,
            expected_count=3,
        )
        self.do_jsrun(d)
        self.verify_answer_content(
            f"{d.id}.t",
            None,
            {"styles": {"backgroundColor": "red"}, "c": 0},
            self.test_user_1,
            expected_count=3,
        )
        self.do_jsrun(d, runner_name="r4")
        self.verify_answer_content(
            f"{d.id}.t",
            None,
            {"styles": {"backgroundColor": "green"}, "c": 0},
            self.test_user_1,
            expected_count=4,
        )
        self.do_jsrun(d, runner_name="r5")
        self.verify_answer_content(
            f"{d.id}.t", None, {"c": 0}, self.test_user_1, expected_count=5
        )
        self.do_jsrun(d, runner_name="r5")
        self.verify_answer_content(
            f"{d.id}.t", None, {"c": 0}, self.test_user_1, expected_count=5
        )

    def test_points(self):
        d = self.create_jsrun(
            """
fields: []
group: testuser1
program: |!!
tools.setDouble("t.points", 1);
!!
        """
        )
        d.document.add_text("#- {plugin=textfield #t}")
        self.do_jsrun(d)
        a = self.verify_answer_content(f"{d.id}.t", None, {"c": None}, self.test_user_1)
        self.assertEqual(1, a.points)
        self.do_jsrun(d)
        self.verify_answer_content(f"{d.id}.t", None, {"c": None}, self.test_user_1)

    def test_group_wildcard(self):
        self.login_test1()

        def run_js(doc, output):
            self.do_jsrun(
                doc,
                expect_content={
                    "web": {
                        "errors": [],
                        "outdata": {},
                        "output": output,
                    },
                },
            )

        def make_doc(*groups):
            d = self.create_jsrun(
                f"""
fields: [mmcqt]
groups: [{', '.join(groups)}]
program: |!!
tools.println(tools.getRealName());
!!
            """
            )
            d.document.add_text(
                """
#- {plugin=mmcq #mmcqt}
stem: "foo"
choices:
    -
        correct: true
        text: "a"
    -
        correct: false
        text: "b"
"""
            )
            return d

        d1 = make_doc('"*"')

        self.add_answer(d1, "mmcqt", content=[True, False], user=self.test_user_1)
        self.add_answer(d1, "mmcqt", content=[True, True], user=self.test_user_2)
        db.session.commit()

        run_js(d1, "Test user 1\nTest user 2\n")

        ug = self.create_test_group("jsrunnertestgroup1")
        self.test_user_3.add_to_group(ug, None)
        d2 = make_doc('"*"', "jsrunnertestgroup1")

        self.add_answer(d2, "mmcqt", content=[True, False], user=self.test_user_1)
        self.add_answer(d2, "mmcqt", content=[True, True], user=self.test_user_2)
        db.session.commit()

        # Test User 3 appears in the result along with all the answered users
        expected_names = "Test user 1\nTest user 2\nTest user 3\n"
        run_js(d2, expected_names)

        self.add_answer(d2, "mmcqt", content=[True, True], user=self.test_user_3)
        db.session.commit()

        # Test User 3 is not duplicated in the list
        run_js(d2, expected_names)

        # Test that current user is not visible if he hasn't answered
        d3 = make_doc('"*"')
        run_js(d3, "")

    def test_mcq_qst_fetch(self):
        d = self.create_jsrun(
            """
fields: [t, t2]
group: testuser1
program: |!!
const t = tools.getValue("t");
tools.println(t);
const t2 = tools.getValue("t2");
tools.println(t2);
!!
        """
        )
        d.document.add_text(
            """
#- {plugin=mcq #t}
headerText: ''
stem: ""
choices:
- text: "first" 
- text: "second"

``` {#t2 plugin="qst"}
answerFieldType: radio
answerLimit: 1
expl: {}
headers: []
questionText: x
questionTitle: x
questionType: radio-vertical
rows:
- a
- b
```
        """
        )
        self.add_answer(d, "t", 1, content_key=None, user=self.test_user_1)
        self.add_answer(d, "t2", [["2"]], content_key=None, user=self.test_user_1)
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={
                "web": {"errors": [], "outdata": {}, "output": '1\n[["2"]]\n'}
            },
        )

    def test_count_field(self):
        d = self.create_jsrun(
            """
fields:
 - t1.count
 - t2.count
group: tg
program: |!!
tools.println(tools.getDouble("t1.count", 0));
tools.println(tools.getValue("t2.count"));
!!
        """
        )
        self.create_test_group("tg")
        d.document.add_text("#- {plugin=textfield #t1}\n#- {plugin=textfield #t2}")
        self.add_answer(d, "t1", "tu1.1.1", user=self.test_user_1)
        self.add_answer(d, "t1", "tu1.1.2", user=self.test_user_1)
        self.add_answer(d, "t1", "tu1.1.3", user=self.test_user_1)
        self.add_answer(d, "t2", "tu2.2.1", user=self.test_user_2)
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={
                "web": {"errors": [], "outdata": {}, "output": "3\n0\n0\n1\n"}
            },
        )

    def test_answerlimit_override(self):
        self.login_test1()
        d = self.create_jsrun(
            """
groups: []
fields:
 - counter
program: |!!
let val = tools.getDouble('counter')
tools.setDouble('counter',val+1)
!!
        """
        )
        d.document.add_text(
            """
``` {#counter plugin="numericfield"}
answerLimit: 0
```
        """
        )
        self.post_answer("numericfield", f"{d.id}.counter", user_input={"c": "3"})
        self.do_jsrun(d)
        self.post_answer("numericfield", f"{d.id}.counter", user_input={"c": "4"})
        self.do_jsrun(d)
        # Tasks with answerlimit 0 are currently used as fields where valid input can only be done via jsrunner
        self.verify_answer_content(f"{d.id}.counter", "c", 2, self.test_user_1, 4)


class JsRunnerGroupTest(JsRunnerTestBase):
    def test_jsrunner_group(self):
        self.login_test1()
        d = self.create_jsrun(
            """
fields: [t1, t2]
group: testusers
program: |!!
tools.setString("t1", tools.getString("t1", "") + "-" + tools.getRealName());
tools.setString("t2", tools.getString("t2", "") + "=" + tools.getRealName());
!!
        """
        )
        d.document.add_text(
            """
#- {#t1 plugin=textfield#}
#- {#t2 plugin=textfield#}
        """
        )
        ug = UserGroup.create("testusers")
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        ug.users.append(self.test_user_3)
        self.test_user_1.groups.append(UserGroup.get_teachers_group())
        db.session.commit()
        self.do_jsrun(
            d,
            expect_content={"web": {"errors": [], "output": "", "outdata": {}}},
        )
        self.verify_answer_content(f"{d.id}.t1", "c", "-Test user 1", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t2", "c", "=Test user 1", self.test_user_1)
        self.verify_answer_content(f"{d.id}.t1", "c", "-Test user 2", self.test_user_2)
        self.verify_answer_content(f"{d.id}.t2", "c", "=Test user 2", self.test_user_2)
        self.do_jsrun(
            d,
            expect_content={"web": {"errors": [], "output": "", "outdata": {}}},
        )
        self.verify_answer_content(
            f"{d.id}.t1",
            "c",
            "-Test user 1-Test user 1",
            self.test_user_1,
            expected_count=2,
        )
        self.verify_answer_content(
            f"{d.id}.t2",
            "c",
            "=Test user 1=Test user 1",
            self.test_user_1,
            expected_count=2,
        )
        self.verify_answer_content(
            f"{d.id}.t1",
            "c",
            "-Test user 2-Test user 2",
            self.test_user_2,
            expected_count=2,
        )
        self.verify_answer_content(
            f"{d.id}.t2",
            "c",
            "=Test user 2=Test user 2",
            self.test_user_2,
            expected_count=2,
        )
        self.do_jsrun(
            d,
            expect_content={"web": {"errors": [], "output": "", "outdata": {}}},
        )
        self.verify_answer_content(
            f"{d.id}.t1",
            "c",
            "-Test user 1-Test user 1-Test user 1",
            self.test_user_1,
            expected_count=3,
        )
        self.verify_answer_content(
            f"{d.id}.t2",
            "c",
            "=Test user 1=Test user 1=Test user 1",
            self.test_user_1,
            expected_count=3,
        )
        self.verify_answer_content(
            f"{d.id}.t1",
            "c",
            "-Test user 2-Test user 2-Test user 2",
            self.test_user_2,
            expected_count=3,
        )
        self.verify_answer_content(
            f"{d.id}.t2",
            "c",
            "=Test user 2=Test user 2=Test user 2",
            self.test_user_2,
            expected_count=3,
        )

    def test_group_create(self):
        self.login_test1()
        d = self.create_jsrun(
            """
fields: []
group: testuser1
program: |!!
tools.setGroup("tg1", []);
!!"""
        )
        # Group methods are only usable in postProgram.
        self.do_jsrun(
            d,
            expect_content={
                "web": {
                    "fatalError": {
                        "msg": "tools.setGroup is not a function",
                        "stackTrace": "Index (1:24)\n"
                        "program:\n"
                        '01: tools.setGroup("tg1", []);\n',
                    },
                    "output": "",
                }
            },
        )

        d_empty = self.create_group_jsrun([])
        self.do_jsrun(
            d_empty,
            expect_content="Creating group tg1: This action requires group administrator rights.",
            expect_status=403,
        )
        ug = UserGroup.get_groupadmin_group()
        self.test_user_1.add_to_group(ug, None)
        db.session.commit()
        self.do_jsrun(
            d_empty,
            expect_content={"web": {"errors": [], "outdata": {}, "output": ""}},
        )
        self.assertIsNotNone(UserGroup.get_by_name("tg1"))
        d = self.create_group_jsrun([self.test_user_1.id, 999])
        self.do_jsrun(
            d,
            expect_content="Users not found: {999}",
            expect_status=400,
        )
        d = self.create_group_jsrun([self.test_user_1.id, self.test_user_2.id])
        self.do_jsrun(
            d,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}}},
        )

        def expect_tg1_members(members):
            ug = UserGroup.get_by_name("tg1")
            self.assertEqual(members, set(ug.users))

        expect_tg1_members({self.test_user_1, self.test_user_2})
        self.do_jsrun(
            d_empty,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}}},
        )
        expect_tg1_members(set())
        d1 = self.create_group_jsrun([self.test_user_1.id], method="addToGroup")
        d2 = self.create_group_jsrun([self.test_user_2.id], method="addToGroup")
        self.do_jsrun(
            d1,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}}},
        )
        self.do_jsrun(
            d2,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}}},
        )
        expect_tg1_members({self.test_user_1, self.test_user_2})

        d1 = self.create_group_jsrun([self.test_user_1.id], method="removeFromGroup")
        d2 = self.create_group_jsrun([self.test_user_2.id], method="removeFromGroup")
        self.do_jsrun(
            d1,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}}},
        )
        expect_tg1_members({self.test_user_2})
        self.do_jsrun(
            d2,
            expect_content={"web": {"output": "", "errors": [], "outdata": {}}},
        )
        expect_tg1_members(set())

        d = self.create_jsrun(
            """
fields: []
group: testuser1
program: ''
postprogram: |!!
for (let i = 0; i < 11; ++i) {
    tools.setGroup("tgx" + i, []);
}
!!"""
        )
        self.do_jsrun(
            d,
            expect_content="Maximum of 10 groups can be created per one jsrunner run.",
            expect_status=400,
        )

    def test_user_filtering(self):
        # Group teacher can filter jsrunner targets but not use runner on users outside of the group
        self.login_test1()
        d = self.create_jsrun(
            """
fields: []
group: testuser3isnothere
program: |!!
tools.print(tools.getUserName());
!!"""
        )
        ug = UserGroup.create("testuser3isnothere")
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        ug.admin_doc = self.create_doc().block
        self.test_user_2.grant_access(ug.admin_doc, AccessType.teacher)
        self.test_user_2.grant_access(d, AccessType.teacher)
        db.session.commit()
        self.login_test2()
        self.do_jsrun(
            d,
            user_input={"userNames": ["testuser1"]},
            expect_content={
                "web": {"errors": [], "output": "testuser1\n", "outdata": {}}
            },
        )
        self.do_jsrun(
            d,
            user_input={"userNames": ["testuser3"]},
            expect_content={"web": {"errors": [], "output": "", "outdata": {}}},
        )
