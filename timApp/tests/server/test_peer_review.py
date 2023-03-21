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

#- {area="rev"}
#- {#ta1 plugin=textfield}
#- {#ta2 plugin=textfield}
#- {area_end="rev"}
""",
        )
        url = d.get_url_for_view("review")
        pars = d.document.get_paragraphs()
        b = pars[1].id
        self.get(
            url,
            expect_status=400,
            expect_content={
                "error": "A single block or an area are required for review view"
            },
        )
        self.get(
            f"{url}?b={b}",
            expect_status=400,
            expect_content={
                "error": "A single block or an area are required for review view"
            },
        )
        r = self.get(f"{url}?b={b}&size=1")
        self.assertIn(
            "Not enough users to form pairs (0 but at least 2 users needed)", r
        )
        rq = PeerReview.query.filter_by(block_id=d.id)
        self.assertEqual(0, len(rq.all()))
        self.add_answer(d, "t", "x", user=self.test_user_1)
        db.session.commit()
        r = self.get(f"{url}?b={b}&size=1")
        self.assertIn(
            "Not enough users to form pairs (1 but at least 2 users needed)", r
        )
        self.assertEqual(0, len(rq.all()))
        self.add_answer(d, "t", "x", user=self.test_user_2)
        db.session.commit()
        r = self.get(f"{url}?b={b}&size=1")
        self.assertNotIn(
            "Not enough users to form pairs (1 but at least 2 users needed)", r
        )

        def check_peerreview_rows_t():
            prs: list[PeerReview] = (
                PeerReview.query.filter_by(block_id=d.id)
                .order_by(
                    PeerReview.reviewer_id,
                )
                .all()
            )
            self.assertEqual(2, len(prs))
            self.assertEqual("t", prs[0].task_name, "Test user 2 answered to t")
            self.assertEqual(self.test_user_1.id, prs[0].reviewer_id)
            self.assertEqual(self.test_user_2.id, prs[0].reviewable_id)
            self.assertEqual("t", prs[1].task_name, "Test user 1 answered to t")
            self.assertEqual(self.test_user_2.id, prs[1].reviewer_id)
            self.assertEqual(self.test_user_1.id, prs[1].reviewable_id)

        # pairing without group works using everyone who answered the document
        check_peerreview_rows_t()
        PeerReview.query.filter_by(block_id=d.id).delete()
        d.document.add_setting("group", "testusers1")
        ug = UserGroup.create("testusers1")
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        ug.users.append(self.test_user_3)
        db.session.commit()
        self.get(f"{url}?b={b}&size=1")
        # pairing with group ignores group members who haven't answered the document
        check_peerreview_rows_t()
        PeerReview.query.filter_by(block_id=d.id).delete()
        self.add_answer(d, "t", "x", user=self.test_user_3)
        ug = UserGroup.create("testuser3isnothere")
        ug.users.append(self.test_user_1)
        ug.users.append(self.test_user_2)
        d.document.add_setting("group", "testuser3isnothere")
        db.session.commit()
        self.get(f"{url}?b={b}&size=1")
        # pairing with group setting ignores users who answered but aren't in the group
        check_peerreview_rows_t()
        PeerReview.query.filter_by(block_id=d.id).delete()
        db.session.commit()
        self.get(
            f"{url}?b={pars[3].id}&size=1",
            expect_status=400,
            expect_content={"error": "Requested block is inside an area"},
        )
        self.assertEqual(0, len(rq.all()))
        tu1_ans = self.add_answer(d, "ta1", "tu1", user=self.test_user_1)
        tu3_ans = self.add_answer(d, "ta2", "tu2", user=self.test_user_3)
        db.session.commit()
        self.get(
            f"{url}?area=revs",
            expect_status=400,
            expect_content={"error": "Area revs not found"},
        )
        self.assertEqual(0, len(rq.all()))
        d.document.add_setting("group", "testusers1")
        self.get(f"{url}?area=rev")
        prs: list[PeerReview] = (
            PeerReview.query.filter_by(block_id=d.id)
            .order_by(
                PeerReview.reviewer_id,
            )
            .all()
        )

        def check_pr_row(index, reviewer_id, reviewable_id, task_name, answer_id):
            self.assertEqual(prs[index].reviewer_id, reviewer_id)
            self.assertEqual(prs[index].reviewable_id, reviewable_id)
            self.assertEqual(prs[index].task_name, task_name)
            self.assertEqual(prs[index].answer_id, answer_id)

        # Testuser2 didn't answer to any tasks in area => not in reviewer pairs
        # Testuser1 and Testuser3 answered only to some tasks in the area => PR rows are still generated for every task
        self.assertEqual(4, len(prs))
        check_pr_row(0, 2, 4, "ta1", None)
        check_pr_row(1, 2, 4, "ta2", tu3_ans.id)
        check_pr_row(2, 4, 2, "ta1", tu1_ans.id)
        check_pr_row(3, 4, 2, "ta2", None)

        PeerReview.query.filter_by(block_id=d.id).delete()
        db.session.commit()
