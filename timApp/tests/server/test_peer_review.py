"""Server tests for peer review."""

from timApp.peerreview.peerreview import PeerReview
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class PeerReviewTest(TimRouteTest):
    def test_peer_review_generate(self):
        self.login_test1()
        d = self.create_doc(
            settings={"peer_review": True},
            initial_par="""
#- {#t plugin=textfield}
#- {#t2 plugin=textfield}
""",
        )
        r = self.get(d.get_url_for_view("review"))
        self.assertIn(
            "Not enough users to form pairs (0 but at least 2 users needed)", r
        )
        rq = PeerReview.query.filter_by(block_id=d.id)
        self.assertEqual(0, len(rq.all()))
        self.add_answer(d, "t", "x", user=self.test_user_1)
        db.session.commit()
        r = self.get(d.get_url_for_view("review"))
        self.assertIn(
            "Not enough users to form pairs (1 but at least 2 users needed)", r
        )
        self.assertEqual(0, len(rq.all()))
        self.add_answer(d, "t2", "x", user=self.test_user_2)
        db.session.commit()
        r = self.get(d.get_url_for_view("review"))
        self.assertNotIn(
            "Not enough users to form pairs (1 but at least 2 users needed)", r
        )

        def check_peerreview_rows():
            prs: list[PeerReview] = (
                PeerReview.query.filter_by(block_id=d.id)
                .order_by(
                    PeerReview.reviewer_id,
                )
                .all()
            )
            self.assertEqual(2, len(prs))
            self.assertEqual(None, prs[0].task_name)
            self.assertEqual(self.test_user_1.id, prs[0].reviewer_id)
            self.assertEqual(self.test_user_2.id, prs[0].reviewable_id)
            self.assertEqual(None, prs[1].task_name)
            self.assertEqual(self.test_user_2.id, prs[1].reviewer_id)
            self.assertEqual(self.test_user_1.id, prs[1].reviewable_id)

        # pairing without group works using everyone who answered the document
        check_peerreview_rows()
        rq.delete()
        d.document.add_setting("group", "testusers1")
        db.session.commit()
        self.get(d.get_url_for_view("review"))
        # pairing with group ignores group members who haven't answered the document
        check_peerreview_rows()
        rq.delete()
        self.add_answer(d, "t", "x", user=self.test_user_3)
        ug = UserGroup.create("testuser3isnothere")
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        d.document.add_setting("group", "testuser3isnothere")
        db.session.commit()
        self.get(d.get_url_for_view("review"))
        # pairing with group setting ignores users who answered but aren't in the group
        check_peerreview_rows()
