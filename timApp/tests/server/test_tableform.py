from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.util.utils import static_tim_doc


class TableFormTest(TimRouteTest):
    def test_answered_users_group(self):
        self.login_test1()
        doc = self.create_doc(from_file=static_tim_doc("multiple_mmcqs.md"))
        plugin_type = "mmcq"
        task_id = f"{doc.id}.mmcqexample"
        fetch_args = {"taskid": task_id, "fields": "mmcqexample", "groups": "*"}
        self.post_answer(plugin_type, task_id, [True, False, False])
        self.test_user_2.grant_access(doc, AccessType.edit)
        db.session.commit()
        self.login_test2()
        self.post_answer(plugin_type, task_id, [True, True, True])

        self.get(
            f"/tableForm/fetchTableDataPreview",
            query_string=fetch_args,
            expect_status=403,
        )
        self.login_test1()

        res = self.get(f"/tableForm/fetchTableDataPreview", query_string=fetch_args)
        self.assertEqual(
            len(res["rows"]),
            2,
            "all users' values should be visible for teacher access",
        )

        # Test that user filtering still works with the wildcard
        self.get(
            f"/tableForm/generateReport",
            query_string={
                "docId": doc.id,
                "fields": "mmcqexample",
                "groups": ["*"],
                "userFilter": [self.test_user_2.name],
                "separator": ";",
                "removeDocIds": False,
            },
            expect_status=200,
            expect_content=f"Username;{task_id}.c\r\n{self.test_user_2.name};[true, true, true]\r\n",
        )
