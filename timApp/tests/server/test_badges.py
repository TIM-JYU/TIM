from datetime import datetime
from unittest.mock import patch

from sqlalchemy import select

from timApp import tim_celery
from timApp.admin.answer_cli import delete_old_answers
from timApp.answer.answer import Answer
from timApp.answer.answers import (
    get_users_for_tasks,
    save_answer,
    get_existing_answers_info,
)
from timApp.answer.backup import get_backup_answer_file
from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.plugin.taskid import TaskId
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.utils import read_json_lines


class BadgeTest(TimRouteTest):
    def create_group(self, name: str, users: list) -> UserGroup:
        ug = UserGroup.create(name)
        for u in users:
            ug.users.append(u)
        return ug

    def test_badge(self):
        main_group = self.create_group("it_25", [self.test_user_1])
        sub_group = self.create_group("it_25-cats", [])
        d = DocEntry.create("badge_test")
        db.session.flush()
        self.test_user_1.grant_access(d.block, AccessType.teacher)
        self.commit_db()

        # TODO: Why isn't the test working without this?
        d.block

        self.login_test1()
        r1 = self.get(f"/all_badges")
        self.assertEqual([], r1)

        # TODO: Test with a user that isn't included in it_25.

        r2 = self.get(
            f"/all_badges_in_context/{self.test_user_1.id}/{d.block.id}/{main_group.name}"
        )
        self.assertEqual([], r2)
        # timestamp = datetime.now()
        r3 = self.post(
            "/create_badge",
            data={
                "created_by": self.test_user_1.id,
                "doc_id": d.block.id,
                "context_group": main_group.name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )
        self.assertEqual(
            {
                "active": True,
                "color": "blue",
                "context_group": "it_25",
                "created": r3["created"],
                "created_by": 2,
                "deleted": None,
                "deleted_by": None,
                "description": "Great coordination",
                "id": 1,
                "image": 1,
                "modified": None,
                "modified_by": None,
                "restored": None,
                "restored_by": None,
                "shape": "hexagon",
                "title": "Coordinator",
            },
            r3,
        )
        r4 = self.get(f"/all_badges")
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "blue",
                    "context_group": "it_25",
                    "created": r3["created"],
                    "created_by": 2,
                    "deleted": None,
                    "deleted_by": None,
                    "description": "Great coordination",
                    "id": 1,
                    "image": 1,
                    "modified": None,
                    "modified_by": None,
                    "restored": None,
                    "restored_by": None,
                    "shape": "hexagon",
                    "title": "Coordinator",
                }
            ],
            r4,
        )

        # d = self.create_doc(
        #     # settings={"auto_number_headings": 0},
        #     initial_par="""
        #         #- {allowangular="true"}
        #         <tim-badge-creator badgegroup-context="it_25">
        #         </tim-badge-creator>
        #         <tim-badge-viewer badgegroup-context="it_25">
        #         </tim-badge-viewer>
        #         """,
        # )
        # print("vastaus:", d)
        # url = d.get_absolute_url()
        # url = d.get_url_for_view("")
        # self.get(url, expect_status=404)
