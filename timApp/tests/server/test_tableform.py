from urllib.parse import urlencode

from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.util.utils import EXAMPLE_DOCS_PATH


class TableFormTest(TimRouteTest):
    def test_answered_users_group(self):
        self.login_test1()
        doc = self.create_doc(from_file=f'{EXAMPLE_DOCS_PATH}/multiple_mmcqs.md')
        plugin_type = 'mmcq'
        task_id = f'{doc.id}.mmcqexample'
        fetch_args = urlencode({
            'taskid': task_id,
            'fields': 'mmcqexample',
            'groups': '*'
        })
        self.post_answer(plugin_type, task_id, [True, False, False])
        self.test_user_2.grant_access(doc, AccessType.edit)
        db.session.commit()
        self.login_test2()
        self.post_answer(plugin_type, task_id, [True, True, True])

        self.get(f"/tableForm/fetchTableDataPreview?{fetch_args}", expect_status=403)
        self.login_test1()

        res = self.get(f"/tableForm/fetchTableDataPreview?{fetch_args}")
        self.assertEqual(len(res["rows"]), 2, "all users' values should be visible for teacher access")
