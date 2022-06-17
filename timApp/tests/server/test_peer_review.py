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
        url = d.get_url_for_view("review")
        r = self.get(url)
        self.assertIn(
            "Not enough users to form pairs (0 but at least 2 users needed)", r
        )
        rq = PeerReview.query.filter_by(block_id=d.id)
        self.assertEqual(0, len(rq.all()))
        self.add_answer(d, "t", "x", user=self.test_user_1)
        db.session.commit()
        r = self.get(url)
        self.assertIn(
            "Not enough users to form pairs (1 but at least 2 users needed)", r
        )
        self.assertEqual(0, len(rq.all()))
        self.add_answer(d, "t2", "x", user=self.test_user_2)
        db.session.commit()
        r = self.get(url)
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
            self.assertEqual("t2", prs[0].task_name, "Test user 2 answered to t2")
            self.assertEqual(self.test_user_1.id, prs[0].reviewer_id)
            self.assertEqual(self.test_user_2.id, prs[0].reviewable_id)
            self.assertEqual("t", prs[1].task_name, "Test user 1 answered to t")
            self.assertEqual(self.test_user_2.id, prs[1].reviewer_id)
            self.assertEqual(self.test_user_1.id, prs[1].reviewable_id)

        # pairing without group works using everyone who answered the document
        check_peerreview_rows()
        PeerReview.query.filter_by(block_id=d.id).delete()
        d.document.add_setting("group", "testusers1")
        ug = UserGroup.create("testusers1")
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        ug.users.append(self.test_user_3)
        db.session.commit()
        self.get(url)
        # pairing with group ignores group members who haven't answered the document
        check_peerreview_rows()
        PeerReview.query.filter_by(block_id=d.id).delete()
        self.add_answer(d, "t", "x", user=self.test_user_3)
        ug = UserGroup.create("testuser3isnothere")
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        d.document.add_setting("group", "testuser3isnothere")
        db.session.commit()
        self.get(url)
        # pairing with group setting ignores users who answered but aren't in the group
        check_peerreview_rows()
        pars = d.document.get_paragraphs()
        PeerReview.query.filter_by(block_id=d.id).delete()
        db.session.commit()
        part = self.get(f"{url}?b={pars[1].id}&size=1")
        self.assertNotIn('id="t2"', part)
        # testuser2 is in pairings even though pairing generating request didn't include #t2
        check_peerreview_rows()
