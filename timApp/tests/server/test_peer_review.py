"""Server tests for peer review."""
from typing import List

from timApp.peerreview.peerreview import PeerReview
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class PeerReviewTest(TimRouteTest):
    def test_peer_review_generate(self):
        self.login_test1()
        d = self.create_doc(
            settings={'peer_review': True},
            initial_par="""
#- {#t plugin=textfield}
#- {#t2 plugin=textfield}
""",
        )
        r = self.get(d.get_url_for_view('review'))
        self.assertIn('Not enough users to form pairs (0 but at least 2 users needed)', r)
        rq = PeerReview.query.filter_by(block_id=d.id)
        self.assertEqual(0, len(rq.all()))
        self.add_answer(d, 't', 'x', user=self.test_user_1)
        db.session.commit()
        r = self.get(d.get_url_for_view('review'))
        self.assertIn('Not enough users to form pairs (1 but at least 2 users needed)', r)
        self.assertEqual(0, len(rq.all()))
        self.add_answer(d, 't', 'x', user=self.test_user_2)
        db.session.commit()
        r = self.get(d.get_url_for_view('review'))
        self.assertNotIn('Not enough users to form pairs (1 but at least 2 users needed)', r)

        def check_peerreview_rows():
            prs: List[PeerReview] = PeerReview.query.filter_by(block_id=d.id).order_by(
                PeerReview.reviewer_id,
                PeerReview.task_name,
            ).all()
            self.assertEqual(2, len(prs))
            self.assertEqual('t', prs[0].task_name)
            self.assertEqual(self.test_user_1.id, prs[0].reviewer_id)
            self.assertEqual(self.test_user_2.id, prs[0].reviewable_id)
            self.assertEqual('t', prs[1].task_name)
            self.assertEqual(self.test_user_2.id, prs[1].reviewer_id)
            self.assertEqual(self.test_user_1.id, prs[1].reviewable_id)

        check_peerreview_rows()
        rq.delete()
        db.session.commit()
        d.document.add_setting('group', 'testuser1')
        self.get(d.get_url_for_view('review'))
        check_peerreview_rows()
