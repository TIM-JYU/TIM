from routes.common import *
from timroutetest import TimRouteTest

class VelpTest(TimRouteTest):
    def test_velp(self):
        db = self.get_db()
        self.login_test1()
        user_folder = 'users/{}'.format(session['user_name'])   # users/testuser1
        deeper_folder = '{}/test'.format(user_folder)          # users/testuser1/test
        test_doc1 = '{}/test1'.format(user_folder)
        test_doc2 = '{}/test2'.format(user_folder)
        test_doc3 = '{}/test3'.format(deeper_folder)
        root_velp_group_folder = "velp groups"
        user_velp_group_folder = "{}/velp groups".format(user_folder)   # users/testuser1/velp groups
        deep_velp_group_folder = "{}/velp groups".format(deeper_folder) # users/testuser1/test/velp groups
        db.folders.create(user_folder, 7)
        db.folders.create(deeper_folder, 7)
        db.folders.create(root_velp_group_folder, 7)
        db.folders.create(user_velp_group_folder, 7)
        db.folders.create(deep_velp_group_folder, 7)
        doc1 = db.documents.create(test_doc1, 7)    # users/testuser1/test1 #owner: testuser1
        doc1_id = doc1.doc_id
        doc2 = db.documents.create(test_doc2, 8)    # users/testuser1/test2 #owner: testuser2
        doc2_id = doc2.doc_id
        doc3 = db.documents.create(test_doc3, 7)    # users/testuser1/test/test3 #owner: testuser1
        doc3_id = doc3.doc_id

        # Try to get velp groups for document that has none
        resp = self.get('/{}/get_default_velp_group'.format(str(doc1_id)), as_json=True)
        self.assertEqual(-1, resp['id'])
        resp = self.get('/{}/get_velp_groups'.format(str(doc1_id)), as_json=True)
        self.assertEqual(len(resp), 0)

        # Create a default velp group for document
        resp = self.json_post('/{}/create_default_velp_group'.format(str(doc1_id)))
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual('test1_default', j['name'])
        self.assertNotEqual(-1, j['id'])
        resp = self.get('/{}/get_default_velp_group'.format(str(doc1_id)), as_json=True)
        self.assertNotEqual(-1, resp['id'])
        default_group_id = resp['id']   # Grab ID of newly created group for later use

        # Create new velp group (after default group) so we should have 2 for document in total
        resp = self.json_post('/{}/create_velp_group'.format(str(doc1_id)),
                              {'name': "velp group for doc 1", "target_type": 1})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual("velp group for doc 1", j['name'])
        resp = self.get('/{}/get_velp_groups'.format(str(doc1_id)), as_json=True)
        self.assertEqual(len(resp), 2)

        # Add two documents to document's velp group folder manually
        # User should only see one of them as velp group due to lack of view right for the other
        test_group1 = '{}/test1/test_group1'.format(user_velp_group_folder)
        test_group2 = '{}/test1/test_group2'.format(user_velp_group_folder)
        db.documents.create(test_group1, 7)
        db.documents.create(test_group2, 8)
        resp = self.get('/{}/get_velp_groups'.format(str(doc1_id)), as_json=True)
        self.assertEqual(len(resp), 3)

        # Create default velp group manually for test 3 file which route notices and turns that document to a velp group
        test3_default_path = '{}/test3/test3_default'.format(deep_velp_group_folder)
        test3_default = db.documents.create(test3_default_path, 7)
        test3_default_id = test3_default.doc_id
        resp = self.json_post('/{}/create_default_velp_group'.format(str(doc3_id)))
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual('test3_default', j['name'])
        self.assertEqual(test3_default_id, j['id'])

        # Test 3 should now have only default velp group assigned so length of velp group list is 1
        resp = self.get('/{}/get_velp_groups'.format(str(doc3_id)), as_json=True)
        self.assertEqual(len(resp), 1)

        # Add velp group to root velp group folder
        # Both documents test1 and test3 should now have one more velp group to use
        test_group3 = '{}/test_group3'.format(root_velp_group_folder)
        db.documents.create(test_group3, 7)
        resp = self.get('/{}/get_velp_groups'.format(str(doc1_id)), as_json=True)
        self.assertEqual(len(resp), 4)
        resp = self.get('/{}/get_velp_groups'.format(str(doc3_id)), as_json=True)
        self.assertEqual(len(resp), 2)

        # Try to get (not existing) default velp group and create new default group for document without owner rights
        resp = self.get('/{}/get_default_velp_group'.format(str(doc2_id)), as_json=True)
        self.assertEqual(-1, resp['id'])
        self.assertDictResponse({'error': 'User has no edit access to current document'},
                                self.json_post('/{}/create_default_velp_group'.format(str(doc2_id))),
                                expect_status = 403)


        # There are no velps added to any groups so getting velps for doc1 should give nothing
        resp = self.get('/{}/get_velps'.format(str(doc1_id)), as_json=True)
        self.assertEqual(len(resp), 0)

        # Add new velp to doc1 default velp group and check velps for that document after
        test_data = {'content':'test velp 1', 'velp_groups':[default_group_id], 'language_id':'FI'}
        resp = self.json_post('/add_velp', test_data)
        velp_id = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual(velp_id, 1) # Added velp's id is 1 as it was the first ever velp added
        resp = self.get('/{}/get_velps'.format(str(doc1_id)), as_json=True)
        print(resp)
        self.assertEqual(len(resp), 1)
        self.assertEqual(resp[0]['content'], "test velp 1")

        # Change just added velp's content
        test_data = {'content':'Is Zorg now', 'velp_groups':[default_group_id], 'language_id':'FI', 'id':1}
        resp = self.json_post('/{}/update_velp'.format(str(doc1_id)), test_data)
        self.assertResponseStatus(resp, return_json=True)
        resp = self.get('/{}/get_velps'.format(str(doc1_id)), as_json=True)
        self.assertEqual(resp[0]['content'], "Is Zorg now")

        # Next add velp label, attach it to a velp
        resp = self.json_post('/add_velp_label', {'content':'test label'})
        label_id = self.assertResponseStatus(resp)
        test_data = {'content':'Is Zorg now', 'velp_groups':[default_group_id], 'language_id':'FI', 'id':velp_id,
                    'labels':[label_id]}
        self.json_post('/{}/update_velp'.format(str(doc1_id)), test_data)
        resp = self.get('/{}/get_velp_labels'.format(str(doc1_id)), as_json=True)
        print(resp)
        self.assertEqual(resp[0]['content'], 'test label')

        # Add a new velp label and update previous one
        self.json_post('/add_velp_label', {'content':'test label intensifies'})
        resp = self.json_post('/update_velp_label', {'id':label_id, 'content':'Zorg label'})
        self.assertResponseStatus(resp, expect_status=200)
        resp = self.get('/{}/get_velp_labels'.format(str(doc1_id)), as_json=True)
        print(resp)
        self.assertNotEqual(resp[0]['content'], 'test label')
        self.assertEqual(len(resp), 1)  # Added velp label wasn't added to any velp and thus it can't be found
                                        # when searching velp labels for doc1





