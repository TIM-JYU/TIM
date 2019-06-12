"""Server tests for jsrunner plugin."""
from typing import List

from timApp.answer.answer import Answer
from timApp.document.docinfo import DocInfo
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.user.user import User


class JsRunnerTest(TimRouteTest):
    def setUp(self):
        super().setUp()
        self.login_test1()

    def verify_content(self, task: str, content_field: str, content, u: User, expected_count=1):
        anss: List[Answer] = u.answers.filter_by(task_id=task).order_by(Answer.answered_on.desc()).all()
        self.assertEqual(expected_count, len(anss))
        first = anss[0]
        self.assertEqual(content, first.content_as_json[content_field])

    def test_jsrunner_invalid(self):
        invalid_yamls = [
            ('', "{'fields': ['Missing data for required field.']}", 400),
            ('fields: []', "{'_schema': ['Either group or groups must be given.']}", 400),
            ('fields: []\ngroup: 0', "{'group': ['Not a valid string.']}", 400),
            ('fields: []\ngroup: 1', "{'group': ['Not a valid string.']}", 400),
            ('fields: []\nprogram: 1', "{'program': ['Not a valid string.']}", 400),
            ('fields: []\ngroup: xxx', "The following groups were not found: xxx", 404),
            ('fields: []\ngroups: [xxx, yyy]', "The following groups were not found: xxx, yyy", 404),
            ('fields: []\ngroup: testuser1', "Attribute 'program' is required.", 400),
            ('fields: []\ngroup: testuser1\nprogram: ""', {"web": {"result": "points saved", "print": "", "error": ""}},
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

    def create_jsrun(self, md):
        return self.create_doc(initial_par=rf"""
#- {{#r plugin=jsrunner}}
{md}""")

    def do_jsrun(self, d: DocInfo, expect_content, expect_status=200, **kwargs):
        self.post_answer(
            'jsrunner',
            f'{d.id}.r',
            user_input={},
            expect_content=expect_content,
            expect_status=expect_status,
            **kwargs,
        )

    def test_jsrunner_nonexistent_field(self):
        d = self.create_jsrun("""
fields:
 - y
program: |!!
tools.setDouble("x", 2.0)
!!
group: testuser1
        """)
        self.do_jsrun(
            d,
            expect_content={'web': {'error': 'Task not found: x',
                                    'print': '',
                                    'result': 'points saved'}},
        )

    def test_jsrunner_nonexistent_doc(self):
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

    def test_jsrunner_setters_and_getters(self):
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
{#t%%'%02d'|format(i)%%}
{% endfor %}
""")
        self.do_jsrun(
            d,
            expect_content={"web": {"result": "points saved", "print": "", "error": ""}},
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
