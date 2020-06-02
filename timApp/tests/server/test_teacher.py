"""Server tests for xxx."""
import json

from timApp.answer.answer import Answer
from timApp.tests.db.timdbtest import TEST_USER_2_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import get_home_organization_group
from timApp.timdb.sqa import db


class TeacherTest(TimRouteTest):
    def test_teacher(self):
        self.login_test1()
        d = self.create_doc()
        r = self.get(f'/teacher/{d.path}', query_string={'group': get_home_organization_group().name}, as_tree=True)
        self.assertEqual(
            "You don't have access to group 'jyu.fi users'.",
            r.cssselect('.alert.alert-info')[0].text_content().strip(),
        )

    def test_teacher_nonexistent_group(self):
        self.login_test1()
        d = self.create_doc()
        r = self.get(
            f'/teacher/{d.path}',
            query_string={'group': 'nonexistent'})
        self.assertIn('User group nonexistent not found', r)

    def test_teacher_single_user_in_group(self):
        self.login_test1()
        d = self.create_doc(initial_par='#- {plugin=textfield #t}')
        self.test_user_1.answers.append(Answer(content=json.dumps({'c': 'x'}), valid=True, task_id=f'{d.id}.t'))
        self.test_user_2.answers.append(Answer(content=json.dumps({'c': 'x'}), valid=True, task_id=f'{d.id}.t'))
        db.session.commit()
        r = self.get(
            f'/teacher/{d.path}',
            query_string={'group': 'testuser2'},
            as_tree=True,
        )
        self.assertEqual(0, len(r.cssselect('.alert.alert-info')))
        self.assert_js_variable(
            r,
            "users",
            [{'task_count': 1,
              'task_points': None,
              'total_points': None,
              'user': {'email': 'test2@example.com',
                       'id': TEST_USER_2_ID,
                       'name': 'testuser2',
                       'real_name': 'Test user 2'},
              'velp_points': 0.0,
              'velped_task_count': 0}],
        )
