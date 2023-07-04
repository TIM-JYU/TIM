from sqlalchemy import select

from timApp.answer.answer import Answer
from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.utils import get_current_time


class TestSelfExpire(TimRouteTest):
    def test_self_expire(self):
        """Test self-expire route basic functionality"""
        self.login_test1()
        d = self.create_doc()
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            accessible_from=get_current_time(),
            accessible_to=get_current_time(),
            block=d,
        )
        db.session.commit()
        self.login_test2()
        self.json_post("/permissions/selfExpire", {"id": d.id})

        self.test_user_2.grant_access(
            access_type=AccessType.view,
            accessible_from=get_current_time(),
            block=d,
        )
        db.session.commit()
        self.json_post("/permissions/selfExpire", {"id": d.id})

    def test_self_expire_field(self):
        """Test self-expiring with a field"""
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin="textfield" .fieldCell}
{#test #}
"""
        )

        self.test_user_2.grant_access(
            access_type=AccessType.view,
            accessible_from=get_current_time(),
            block=d,
        )
        db.session.commit()

        self.login_test2()
        self.get(d.url_relative, expect_status=200)

        self.json_post(
            "/permissions/selfExpire", {"id": d.id, "set_field": f"{d.id}.test"}
        )

        self.get(d.url_relative, expect_status=403)

        d = DocEntry.find_by_id(d.id)
        self.assertEqual(
            {
                (User.get_by_name(a.usergroup.name).id, a.type, a.expired)
                for a in d.block.accesses.values()
            },
            {
                (self.test_user_1.id, AccessType.owner.value, False),
                (self.test_user_2.id, AccessType.view.value, True),
            },
        )

        ans: list[Answer] = (
            db.session.execute(select(Answer).filter_by(task_id=f"{d.id}.test"))
            .scalars()
            .all()
        )
        self.assertEqual(len(ans), 1)
        self.assertEqual([u.name for u in ans[0].users_all], [self.test_user_2.name])
        self.assertEqual(ans[0].content, '{"c": "1"}')
