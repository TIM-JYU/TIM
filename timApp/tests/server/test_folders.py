from timApp.tests.db.timdbtest import TEST_USER_2_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.accesstype import AccessType
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.tim_models import db, BlockAccess
from timApp.timdb.userutils import grant_access, grant_view_access, get_anon_group_id


class FolderTest(TimRouteTest):

    def test_folder_manage(self):
        self.login_test3()
        f = self.create_folder(self.get_personal_item_path('test_manage'))
        self.get(f'/manage/{f["path"]}')
        self.login_test2()
        self.get(f'/manage/{f["path"]}', expect_status=403)
        db = self.get_db()
        grant_access(self.get_test_user_2_group_id(), f['id'], 'manage')
        self.get(f'/manage/{f["path"]}')

    def test_folder_delete(self):
        self.login_test1()
        to_delete = self.get_personal_item_path('delete/this')
        f = self.create_folder(to_delete)
        f2 = Folder.find_by_path(self.get_personal_item_path('delete'))
        not_empty_error = {'error': 'The folder is not empty. Only empty folders can be deleted.'}
        self.delete(f'/folders/{f2.id}', expect_status=403,
                    expect_content=not_empty_error)
        grant_view_access(get_anon_group_id(), f['id'])
        self.delete(f'/folders/{f["id"]}', expect_content=self.ok_resp)
        doc_path = self.get_personal_item_path('delete/somedoc')
        self.create_doc(doc_path)
        self.delete(f'/folders/{f2.id}', expect_status=403,
                    expect_content=not_empty_error)
        d = DocEntry.find_by_path(doc_path)
        d.name = 'asd'
        d = DocEntry.find_by_path(self.get_personal_item_path('Bookmarks'))
        d.name = 'asd2'
        db.session.commit()
        self.delete(f'/folders/{f2.id}', expect_content=self.ok_resp)

    def test_intermediate_folders(self):
        self.login_test1()
        fname = self.get_personal_item_path('a/b/c/d')
        self.create_folder(fname)

    def test_folders(self):
        self.login_test1()
        user_folder = self.current_user.get_personal_folder().path
        fname = self.get_personal_item_path('testing')

        f = self.create_folder(fname)
        self.create_folder(fname,
                           expect_content={'error': 'Item with a same name already exists.'},
                           expect_status=403)
        new_name = fname + '1'
        f2 = self.json_put(f'/rename/{f["id"]}', {"new_name": new_name})
        self.assertEqual(new_name, f2['new_name'])
        self.json_put(f'/rename/{f["id"]}',
                      {"new_name": new_name + '/testing1'}, expect_status=403,
                      expect_content={'error': 'A folder cannot contain itself.'}),

        # Create another folder and give access to anonymous users
        fname2 = self.get_personal_item_path('testing2')
        f3 = self.create_folder(fname2)
        grant_access(get_anon_group_id(), f3['id'], 'view')
        t1g = self.get_test_user_1_group_id()
        self.get('/getItems', query_string={'folder': user_folder},
                 expect_content=[{'name': 'testing1',
                                  'title': 'foldertitle',
                                  'id': f['id'],
                                  'isFolder': True,
                                  'modified': 'just now',
                                  'path': new_name,
                                  'location': user_folder,
                                  'owner': {'id': t1g, 'name': 'testuser1'},
                                  'rights': {'browse_own_answers': True,
                                             'can_comment': True,
                                             'can_mark_as_read': True,
                                             'editable': True,
                                             'manage': True,
                                             'owner': True,
                                             'see_answers': True,
                                             'teacher': True},
                                  'unpublished': True,
                                  'public': True},
                                 {'name': 'testing2',
                                  'title': 'foldertitle',
                                  'id': f3['id'],
                                  'isFolder': True,
                                  'modified': 'just now',
                                  'path': fname2,
                                  'location': user_folder,
                                  'owner': {'id': t1g, 'name': 'testuser1'},
                                  'rights': {'browse_own_answers': True,
                                             'can_comment': True,
                                             'can_mark_as_read': True,
                                             'editable': True,
                                             'manage': True,
                                             'owner': True,
                                             'see_answers': True,
                                             'teacher': True},
                                  'unpublished': False,
                                  'public': True}])
        self.logout()
        self.get('/getItems', query_string={'folder': user_folder},
                 expect_content=[{'name': 'testing2',
                                  'title': 'foldertitle',
                                  'id': f3['id'],
                                  'isFolder': True,
                                  'modified': 'just now',
                                  'path': fname2,
                                  'location': user_folder,
                                  'owner': {'id': t1g, 'name': 'testuser1'},
                                  'rights': {'browse_own_answers': False,
                                             'can_comment': False,
                                             'can_mark_as_read': False,
                                             'editable': False,
                                             'manage': False,
                                             'owner': False,
                                             'see_answers': False,
                                             'teacher': False},
                                  'unpublished': False,
                                  'public': True}])

    def test_folders_invalid(self):
        self.login_test1()
        invalid = self.get_personal_item_path('/test')
        invalid2 = "test"
        invalid3 = "1234"
        invalid4 = ''
        self.create_folder(invalid,
                           expect_content={'error': 'The folder path cannot have empty parts.'},
                           expect_status=400)
        self.create_folder(invalid2,
                           expect_content={'error': 'You cannot create folders in this folder.'},
                           expect_status=403)
        self.create_folder(invalid3,
                           expect_content={
                               'error': 'The folder path can not be a number to avoid confusion with document id.'},
                           expect_status=400)
        self.create_folder(invalid4,
                           expect_content={'error': 'The folder path cannot have empty parts.'},
                           expect_status=400)
        for c in 'ãàáäâåẽèéëêìíïîõòóöôùúüûñç·,:;<>|^~¨"!½#¤%&()=?`.@£$€{[]}\\ ':
            self.create_folder(self.get_personal_item_path(c),
                               expect_content={
                                   'error': 'The folder path has invalid characters. Only letters, numbers, '
                                            'underscores and dashes are allowed.'},
                               expect_status=400)


class FolderCopyTest(TimRouteTest):
    def test_folder_copy(self):
        self.login_test1()
        d1 = self.create_doc(self.get_personal_item_path('a/d1'))
        d2 = self.create_doc(self.get_personal_item_path('a/d2'))
        f1d1 = self.create_doc(self.get_personal_item_path('a/f1/d1'))
        f1d2 = self.create_doc(self.get_personal_item_path('a/f1/d2'))
        f1d3 = self.create_doc(self.get_personal_item_path('a/f1/d3'),
                               initial_par="""
#- {#t1 plugin=csPlugin}
#- {#t1 plugin=csPlugin}""")
        f2d1 = self.create_doc(self.get_personal_item_path('a/f2/d1'))
        f1d1tren = self.create_translation(f1d1, 'title_en', 'en')
        f1d1trsv = self.create_translation(f1d1, 'title_sv', 'sv')
        f1d1tren.document.add_paragraph('hello_en')
        f1d1trsv.document.add_paragraph('hello_sv')

        a = Folder.find_by_path(self.get_personal_item_path('a'))
        a.title = 'oldtitle'
        f1 = Folder.find_by_path(self.get_personal_item_path('a/f1'))
        f2 = Folder.find_by_path(self.get_personal_item_path('a/f2'))

        t2g = self.get_test_user_2_group_id()
        grant_access(t2g, f1.id, 'view', commit=False)
        grant_access(t2g, f2.id, 'edit', commit=False)
        grant_access(t2g, d2.id, 'teacher', commit=False)
        db.session.commit()
        d1.document.add_paragraph('hello')
        f2d1.document.add_paragraph('hi')

        self.json_post(f'/copy/{a.id}',
                       {'destination': self.get_personal_item_path('a/b'), 'exclude': None},
                       expect_status=403,
                       expect_content={'error': 'Cannot copy folder inside of itself.'})
        self.json_post(f'/copy/{a.id}',
                       {'destination': self.get_personal_item_path('b'), 'exclude': None})
        preview = self.json_post(f'/copy/{a.id}/preview',
                                 {'destination': self.get_personal_item_path('b'), 'exclude': ''})
        self.assertEqual([{'from': 'users/test-user-1/a/d1', 'to': 'users/test-user-1/b/d1'},
                          {'from': 'users/test-user-1/a/d2', 'to': 'users/test-user-1/b/d2'},
                          {'from': 'users/test-user-1/a/f1', 'to': 'users/test-user-1/b/f1'},
                          {'from': 'users/test-user-1/a/f1/d1', 'to': 'users/test-user-1/b/d1'},
                          {'from': 'users/test-user-1/a/f1/d2', 'to': 'users/test-user-1/b/d2'},
                          {'from': 'users/test-user-1/a/f1/d3', 'to': 'users/test-user-1/b/d3'},
                          {'from': 'users/test-user-1/a/f2', 'to': 'users/test-user-1/b/f2'},
                          {'from': 'users/test-user-1/a/f2/d1', 'to': 'users/test-user-1/b/d1'}], preview)
        preview = self.json_post(f'/copy/{a.id}/preview',
                                 {'destination': self.get_personal_item_path('b'), 'exclude': 'd1'})
        self.assertEqual([{'from': 'users/test-user-1/a/d2', 'to': 'users/test-user-1/b/d2'},
                          {'from': 'users/test-user-1/a/f1', 'to': 'users/test-user-1/b/f1'},
                          {'from': 'users/test-user-1/a/f1/d2', 'to': 'users/test-user-1/b/d2'},
                          {'from': 'users/test-user-1/a/f1/d3', 'to': 'users/test-user-1/b/d3'},
                          {'from': 'users/test-user-1/a/f2', 'to': 'users/test-user-1/b/f2'}], preview)
        b = Folder.find_by_path(self.get_personal_item_path('b'))
        self.assertEqual('b', b.title)
        d1c = DocEntry.find_by_path(self.get_personal_item_path('b/d1'))
        d2c = DocEntry.find_by_path(self.get_personal_item_path('b/d2'))
        d2 = DocEntry.find_by_path(self.get_personal_item_path('a/d2'))
        f1d1c = DocEntry.find_by_path(self.get_personal_item_path('b/f1/d1'))
        f2d1c = DocEntry.find_by_path(self.get_personal_item_path('b/f2/d1'))
        f1c = Folder.find_by_path(self.get_personal_item_path('b/f1'))
        f2c = Folder.find_by_path(self.get_personal_item_path('b/f2'))
        t1g = self.get_test_user_1_group_id()
        self.assertEqual([BlockAccess(block_id=f1c.id, usergroup_id=t1g, type=AccessType.owner.value),
                          BlockAccess(block_id=f1c.id, usergroup_id=t2g, type=AccessType.view.value),
                          ], f1c.block.accesses.all())
        self.assertEqual([BlockAccess(block_id=f2c.id, usergroup_id=t1g, type=AccessType.owner.value),
                          BlockAccess(block_id=f2c.id, usergroup_id=t2g, type=AccessType.edit.value),
                          ], f2c.block.accesses.all())
        self.assertEqual([BlockAccess(block_id=d2c.id, usergroup_id=t1g, type=AccessType.owner.value),
                          BlockAccess(block_id=d2c.id, usergroup_id=t2g, type=AccessType.teacher.value),
                          ], d2c.block.accesses.all())
        self.assertEqual([BlockAccess(block_id=d2.id, usergroup_id=t1g, type=AccessType.owner.value),
                          BlockAccess(block_id=d2.id, usergroup_id=t2g, type=AccessType.teacher.value),
                          ], d2.block.accesses.all())
        trs = sorted(f1d1c.translations, key=lambda tr: tr.lang_id)
        self.assertEqual(['', 'en', 'sv'], [tr.lang_id for tr in trs])
        self.assertEqual(['document 1', 'title_en', 'title_sv'], [tr.title for tr in trs])
        self.assertEqual(['hello'], [p.get_markdown() for p in d1c.document.get_paragraphs()])
        self.assertEqual(['hi'], [p.get_markdown() for p in f2d1c.document.get_paragraphs()])
        self.assertEqual(['hello_en'], [p.get_markdown() for p in trs[1].document.get_paragraphs()])
        self.assertEqual(['hello_sv'], [p.get_markdown() for p in trs[2].document.get_paragraphs()])
        self.assertFalse(set(tr.id for tr in f1d1.translations) & set(tr.id for tr in f1d1c.translations))

        self.json_post(f'/copy/{a.id}',
                       {'destination': self.get_personal_item_path('b'), 'exclude': None},
                       expect_status=403)
