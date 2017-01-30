from documentmodel.timjsonencoder import TimJsonEncoder
from tests.server.timroutetest import TimRouteTest
from timdb.tim_models import db


class TemplateTest(TimRouteTest):

    def test_templates(self):
        self.login_test1()
        db.session.expire_on_commit = False
        t1 = self.create_doc('{}/Templates/T1'.format(self.current_user.get_personal_folder().path))
        t1json = t1.to_json()
        t2 = self.create_doc('{}/Templates/T2'.format(self.current_user.get_personal_folder().path))
        t2json = t2.to_json()

        # TODO possibly move this to Item class
        for t in t1json, t2json:
            t['owner'] = TimJsonEncoder().default(t['owner'])

        d = self.create_doc()
        self.maxDiff = None
        self.get('/getTemplates',
                 query_string={'item_path': d.path},
                 expect_content=[t1json, t2json])
        self.logout()
        self.get('/getTemplates',
                 query_string={'item_path': d.path},
                 expect_status=403)
        self.get('/getTemplates',
                 query_string={'item_path': 'nonexistent'},
                 expect_status=404)
