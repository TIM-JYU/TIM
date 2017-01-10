from datetime import timedelta

from tests.server.timroutetest import TimRouteTest
from timdb.models.docentry import DocEntry
from timdb.tim_models import db, BlockAccess


class DurationTest(TimRouteTest):
    def test_duration_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        self.db.users.grant_access(self.get_test_user_2_group_id(), doc_id, 'view', duration=timedelta(days=1))
        d = DocEntry.find_by_id(doc_id)
        self.get('/view/' + d.path,
                 expect_status=403,
                 expect_contains=['You are about to access a time-limited document.',
                                  'After you click Unlock, your access to this document will be removed in a day.'])
        self.get('/view/' + d.path, query_string={'unlock': 'true'},
                 expect_contains=['Item was unlocked successfully.'])
        self.get('/view/' + d.path)
        ba = BlockAccess.query.filter_by(usergroup_id=self.get_test_user_2_group_id(), block_id=doc_id,
                                         type=self.db.users.get_access_type_id('view')).one()
        ba.accessible_to -= timedelta(days=2)
        db.session.commit()
        self.get('/view/' + d.path, expect_status=403,
                 expect_content={'error': 'Your access to this item has expired.'})
