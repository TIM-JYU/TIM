from routes.common import *
from timroutetest import TimRouteTest

class VelpTest(TimRouteTest):
    def test_velp(self):
        db = self.get_db()
        self.login_test1()
        user_folder = 'users/{}'.format(session['user_name'])
        deeper_folder = '{}/test.'.format(user_folder)
        test_doc1 = '{}/test1'.format(user_folder)
        test_doc2 = '{}/test1'.format(user_folder)
        test_doc2 = '{}/test2'.format(deeper_folder)
        root_velp_group_folder = "velp groups"
        user_velp_group_folder = "{}/velp groups".format(user_folder)
        print(user_velp_group_folder)
        db.folders.create(user_folder, 7)
        db.folders.create(deeper_folder, 7)
        doc1 = db.documents.create(test_doc1, 7)
        doc1_id = doc1.doc_id
        doc2 = db.documents.create(test_doc2, 8)
        doc2_id = doc2.doc_id

        # Try to get velp groups for document that has none
        resp = self.get('/{}/get_default_velp_group'.format(str(doc1_id)), as_json=True)
        self.assertEqual(-1, resp['id'])
        resp = self.get('/{}/get_velp_groups'.format(str(doc1_id)), as_json=True)
        self.assertEqual(len(resp), 0)

        # Create a default velp group for document
        resp = self.json_post('/{}/create_default_velp_group'.format(str(doc1_id)), {})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual('test1_default', j['name'])
        self.assertNotEqual(-1, j['id'])
        resp = self.get('/{}/get_default_velp_group'.format(str(doc1_id)), as_json=True)
        self.assertNotEqual(-1, resp['id'])

        # Create new velp group (after default group) so we should have 2 for document in total
        resp = self.json_post('/{}/create_velp_group'.format(str(doc1_id)),
                              {'name': "velp group for doc 1", "target_type": 1})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual("velp group for doc 1", j['name'])
        resp = self.get('/{}/get_velp_groups'.format(str(doc1_id)), as_json=True)
        self.assertEqual(len(resp), 2)

        # Add two documents to document's velp group folder manually
        # User should only see one of them due to lack of view right for the other
        test_group1 = '{}/test1/test_group1'.format(user_velp_group_folder)
        test_group2 = '{}/test1/test_group2'.format(user_velp_group_folder)
        db.documents.create(test_group1, 7)
        db.documents.create(test_group2, 8)
        resp = self.get('/{}/get_velp_groups'.format(str(doc1_id)), as_json=True)
        self.assertEqual(len(resp), 3)



        # Try to get (not existing) default velp group and create new default group for document without owner rights
        resp = self.get('/{}/get_default_velp_group'.format(str(doc2_id)), as_json=True)
        self.assertEqual(-1, resp['id'])
        self.assertDictResponse({'error': 'User is not owner of current document'},
                                self.json_post('/{}/create_default_velp_group'.format(str(doc2_id)), {}),
                                expect_status = 400)
