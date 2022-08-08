from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import ADMIN_GROUPNAME
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup


class SelfJoinTest(TimRouteTest):
    """Tests for groupJoin plugin routes"""

    def test_self_join(self):
        """Test self-join functionality of groupJoin plugin"""

        u1, _ = User.create_with_group(
            UserInfo(
                full_name="Self Join Test 1",
                username="selfjointestuser1",
                email="selfjointestuser1@example.com",
                password="test",
            ),
            is_admin=True,
        )
        u2, _ = User.create_with_group(
            UserInfo(
                full_name="Self Join Test 1",
                username="selfjointestuser2",
                email="selfjointestuser2@example.com",
                password="test",
            ),
        )

        def get_u1() -> User:
            return User.get_by_name(u1.name)

        def get_u2() -> User:
            return User.get_by_name(u2.name)

        db.session.commit()
        self.login(get_u1().email, "test", get_u1().name)
        self.get("/groups/create/selfjointest1")
        self.json_post("/groupJoin/joinGroups", {"groups": ["selfjointest1"]})
        db.session.refresh(get_u1())
        self.assertEqual(
            {g.name for g in get_u1().groups},
            {
                "selfjointest1",
                ADMIN_GROUPNAME,
                get_u1().get_personal_group().name,
            },
        )

        self.login(get_u2().email, "test", get_u2().name)
        r = self.json_post("/groupJoin/joinGroups", {"groups": ["selfjointest1"]})
        self.assertEqual(
            r["result"]["selfjointest1"], "Self-join not enabled for this group"
        )

        ug = UserGroup.get_or_create_group("selfjointest1")
        doc: DocEntry = ug.admin_doc.docentries[0]
        doc.document.add_setting(
            "groupSelfJoin",
            {
                "canJoin": True,
            },
        )
        db.session.commit()
        self.json_post("/groupJoin/joinGroups", {"groups": ["selfjointest1"]})
        db.session.refresh(get_u2())
        self.assertEqual(
            {g.name for g in get_u2().groups},
            {
                "selfjointest1",
                get_u2().get_personal_group().name,
            },
        )

    def test_self_leave(self):
        """Test self-leave functionality of groupJoin plugin"""

        u1, _ = User.create_with_group(
            UserInfo(
                full_name="Self Leave Test 1",
                username="selfleavetestuser1",
                email="selfleavetestuser1@example.com",
                password="test",
            ),
            is_admin=True,
        )
        u2, _ = User.create_with_group(
            UserInfo(
                full_name="Self Leave Test 1",
                username="selfleavetestuser2",
                email="selfleavetestuser2@example.com",
                password="test",
            ),
        )

        def get_u1() -> User:
            return User.get_by_name(u1.name)

        def get_u2() -> User:
            return User.get_by_name(u2.name)

        db.session.commit()

        self.login(get_u1().email, "test", get_u1().name)
        self.get("/groups/create/selfleavetest1")
        ug = UserGroup.get_or_create_group("selfleavetest1")
        get_u1().add_to_group(ug, None)
        get_u2().add_to_group(ug, None)

        db.session.commit()
        db.session.refresh(get_u1())
        db.session.refresh(get_u2())

        self.assertEqual(
            {g.name for g in get_u1().groups},
            {
                "selfleavetest1",
                ADMIN_GROUPNAME,
                get_u1().get_personal_group().name,
            },
        )
        self.assertEqual(
            {g.name for g in get_u2().groups},
            {
                "selfleavetest1",
                get_u2().get_personal_group().name,
            },
        )

        self.json_post("/groupJoin/leaveGroups", {"groups": ["selfleavetest1"]})
        db.session.refresh(get_u1())
        self.assertEqual(
            {g.name for g in get_u1().groups},
            {
                ADMIN_GROUPNAME,
                get_u1().get_personal_group().name,
            },
        )

        self.login(get_u2().email, "test", get_u2().name)
        r = self.json_post("/groupJoin/leaveGroups", {"groups": ["selfleavetest1"]})
        self.assertEqual(
            r["result"]["selfleavetest1"], "Self-leave not enabled for this group"
        )

        ug = UserGroup.get_or_create_group("selfleavetest1")
        doc: DocEntry = ug.admin_doc.docentries[0]
        doc.document.add_setting(
            "groupSelfJoin",
            {
                "canLeave": True,
            },
        )
        db.session.commit()
        self.json_post("/groupJoin/leaveGroups", {"groups": ["selfleavetest1"]})
        db.session.refresh(get_u2())
        self.assertEqual(
            {g.name for g in get_u2().groups},
            {get_u2().get_personal_group().name},
        )
