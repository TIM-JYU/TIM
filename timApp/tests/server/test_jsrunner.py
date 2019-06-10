"""Server tests for jsrunner plugin."""
from timApp.tests.server.timroutetest import TimRouteTest


class JsRunnerTest(TimRouteTest):
    def test_jsrunner_invalid(self):
        self.login_test1()
        invalid_yamls = [
            ('', "{'fields': ['Missing data for required field.']}", 400),
            ('fields: []', "{'_schema': ['Either group or groups must be given.']}", 400),
            ('fields: []\ngroup: 0', "{'group': ['Not a valid string.']}", 400),
            ('fields: []\ngroup: 1', "{'group': ['Not a valid string.']}", 400),
            ('fields: []\ngroup: xxx', "The following groups were not found: xxx", 404),
            ('fields: []\ngroups: [xxx, yyy]', "The following groups were not found: xxx, yyy", 404),
            ('fields: []\ngroup: testuser1', {"web": {"result": "points saved", "print": "", "error": ""}}, 200),
        ]
        for y, e, s in invalid_yamls:
            d = self.create_doc(initial_par=rf"""
#- {{#j plugin=jsrunner}}
{y}
        """)
            self.post_answer(
                'jsrunner',
                f'{d.id}.j',
                user_input={},
                expect_content=e,
                expect_status=s,
                json_key='error' if s >= 400 else None,
            )
