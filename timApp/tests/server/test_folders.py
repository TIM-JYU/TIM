from sqlalchemy import event

from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_access


class FolderTest(TimRouteTest):

    def test_folder_manage(self):
        self.login_test3()
        f = self.create_folder(self.get_personal_item_path('test_manage'))
        self.get(f'/manage/{f["path"]}')
        self.login_test2()
        self.get(f'/manage/{f["path"]}', expect_status=403)
        self.test_user_2.grant_access(Folder.get_by_id(f['id']), AccessType.manage)
        db.session.commit()
        self.get(f'/manage/{f["path"]}')

    def test_folder_delete(self):
        self.login_test1()
        to_delete = self.get_personal_item_path('delete/this')
        f = self.create_folder(to_delete)
        f2 = Folder.find_by_path(self.get_personal_item_path('delete'))
        self.delete(f'/folders/{f2.id}')
        self.get(f'/folders/{f2.id}', expect_status=404)
        User.get_anon().grant_access(Folder.get_by_id(f['id']), AccessType.view)
        db.session.commit()
        self.delete(f'/folders/{f["id"]}', expect_content=self.ok_resp)
        doc_path = self.get_personal_item_path('delete/somedoc')
        self.create_doc(doc_path)
        self.delete(
            f'/folders/{f2.id}',
            expect_status=400,
            expect_content='Folder is already deleted.',
        )
        self.get(f'/folders/{f2.id}', expect_status=404)
        d2 = DocEntry.find_by_path(doc_path)
        d2.name = 'asd'
        d = DocEntry.find_by_path(self.get_personal_item_path('Bookmarks'))
        d.name = 'asd2'
        db.session.commit()
        f2 = Folder.find_by_path(self.get_personal_item_path('delete'))
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
        User.get_anon().grant_access(Folder.get_by_id(f3['id']), AccessType.view)
        db.session.commit()
        t1g = self.get_test_user_1_group_id()
        self.get('/getItems', query_string={'folder': user_folder},
                 expect_content=[{'name': 'testing1',
                                  'title': 'foldertitle',
                                  'id': f['id'],
                                  'isFolder': True,
                                  'modified': 'just now',
                                  'path': new_name,
                                  'location': user_folder,
                                  'owners': [{'id': t1g, 'name': 'testuser1'}],
                                  'rights': {'browse_own_answers': True,
                                             'can_comment': True,
                                             'can_mark_as_read': True,
                                             'copy': True,
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
                                  'owners': [{'id': t1g, 'name': 'testuser1'}],
                                  'rights': {'browse_own_answers': True,
                                             'can_comment': True,
                                             'can_mark_as_read': True,
                                             'copy': True,
                                             'editable': True,
                                             'manage': True,
                                             'owner': True,
                                             'see_answers': True,
                                             'teacher': True},
                                  'unpublished': False,
                                  'public': True}])
        self.logout()
        self.get('/getItems', query_string={'folder': user_folder}, expect_status=403)
        User.get_anon().grant_access(self.test_user_1.get_personal_folder(), AccessType.view)
        db.session.commit()
        self.get('/getItems', query_string={'folder': user_folder},
                 expect_content=[{'name': 'testing2',
                                  'title': 'foldertitle',
                                  'id': f3['id'],
                                  'isFolder': True,
                                  'modified': 'just now',
                                  'path': fname2,
                                  'location': user_folder,
                                  'owners': [{'id': t1g, 'name': 'testuser1'}],
                                  'rights': {'browse_own_answers': False,
                                             'can_comment': False,
                                             'can_mark_as_read': False,
                                             'copy': False,
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

    def test_folder_view_perf(self):
        self.login_test3()
        self.create_doc(self.get_personal_item_path('perf/x'))
        d = self.create_doc(self.get_personal_item_path('perf/y'))
        self.get(d.url)
        eng = db.get_engine()

        stmts = 0
        db.session.expunge_all()

        def before_cursor_execute(conn, cursor, statement: str, parameters, context,
                                  executemany):
            stmt = statement.replace('\n', ' ')
            # print(f"{parameters}\n{stmt}")
            nonlocal stmts
            stmts += 1

        event.listen(eng, 'before_cursor_execute', before_cursor_execute)
        self.get('/view/' + self.get_personal_item_path('perf'))
        event.remove(eng, 'before_cursor_execute', before_cursor_execute)

        # TODO: Find out why there is one extra statement at the end when running all tests in this class.
        self.assertGreaterEqual(stmts, 12)
        self.assertLessEqual(stmts, 14)
        # print(stmts)


class FolderCopyTest(TimRouteTest):
    def test_folder_copy(self):
        self.login_test1()
        d1 = self.create_doc(self.get_personal_item_path('a/d1'))
        d2 = self.create_doc(self.get_personal_item_path('a/d2'))
        f1d1 = self.create_doc(self.get_personal_item_path('a/f1/d1'))
        _ = self.create_doc(self.get_personal_item_path('a/a-b/x_y'))
        _ = self.create_doc(self.get_personal_item_path('a/a-b/x-y'))
        _ = self.create_doc(self.get_personal_item_path('a/f1/d2'))
        _ = self.create_doc(self.get_personal_item_path('a/f1/d3'),
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

        t2g = self.test_user_2.get_personal_group()
        grant_access(t2g, f1, AccessType.view)
        grant_access(t2g, f2, AccessType.edit)
        grant_access(t2g, d2, AccessType.teacher)
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
        self.assertEqual({
            'dest_exists': True,
            'preview': [
                {'from': 'users/test-user-1/a/d1', 'to': 'users/test-user-1/b/d1'},
                {'from': 'users/test-user-1/a/d2', 'to': 'users/test-user-1/b/d2'},
                {'from': 'users/test-user-1/a/f1', 'to': 'users/test-user-1/b/f1'},
                {'from': 'users/test-user-1/a/f1/d1',
                 'to': 'users/test-user-1/b/f1/d1'},
                {'from': 'users/test-user-1/a/f1/d2',
                 'to': 'users/test-user-1/b/f1/d2'},
                {'from': 'users/test-user-1/a/f1/d3',
                 'to': 'users/test-user-1/b/f1/d3'},
                {'from': 'users/test-user-1/a/a-b',
                 'to': 'users/test-user-1/b/a-b'},
                {'from': 'users/test-user-1/a/a-b/x_y',
                 'to': 'users/test-user-1/b/a-b/x_y'},
                {'from': 'users/test-user-1/a/a-b/x-y',
                 'to': 'users/test-user-1/b/a-b/x-y'},
                {'from': 'users/test-user-1/a/f2', 'to': 'users/test-user-1/b/f2'},
                {'from': 'users/test-user-1/a/f2/d1',
                 'to': 'users/test-user-1/b/f2/d1'},
            ]}, preview)
        preview = self.json_post(f'/copy/{a.id}/preview',
                                 {'destination': self.get_personal_item_path('b'), 'exclude': 'd1'})
        self.assertEqual({
            'dest_exists': True,
            'preview': [
                {'from': 'users/test-user-1/a/d2', 'to': 'users/test-user-1/b/d2'},
                {'from': 'users/test-user-1/a/f1', 'to': 'users/test-user-1/b/f1'},
                {'from': 'users/test-user-1/a/f1/d2',
                 'to': 'users/test-user-1/b/f1/d2'},
                {'from': 'users/test-user-1/a/f1/d3',
                 'to': 'users/test-user-1/b/f1/d3'},
                {'from': 'users/test-user-1/a/a-b',
                 'to': 'users/test-user-1/b/a-b'},
                {'from': 'users/test-user-1/a/a-b/x_y',
                 'to': 'users/test-user-1/b/a-b/x_y'},
                {'from': 'users/test-user-1/a/a-b/x-y',
                 'to': 'users/test-user-1/b/a-b/x-y'},
                {'from': 'users/test-user-1/a/f2',
                 'to': 'users/test-user-1/b/f2'},
            ]}, preview)
        b = Folder.find_by_path(self.get_personal_item_path('b'))
        self.assertEqual('b', b.title)
        d1c = DocEntry.find_by_path(self.get_personal_item_path('b/d1'))
        d2c = DocEntry.find_by_path(self.get_personal_item_path('b/d2'))
        d2 = DocEntry.find_by_path(self.get_personal_item_path('a/d2'))
        f1d1c = DocEntry.find_by_path(self.get_personal_item_path('b/f1/d1'))
        f2d1c = DocEntry.find_by_path(self.get_personal_item_path('b/f2/d1'))
        f1c = Folder.find_by_path(self.get_personal_item_path('b/f1'))
        f2c = Folder.find_by_path(self.get_personal_item_path('b/f2'))
        t1g = self.test_user_1.get_personal_group()
        self.assertEqual([BlockAccess(block_id=f1c.id, usergroup_id=t1g.id, type=AccessType.owner.value),
                          BlockAccess(block_id=f1c.id, usergroup_id=t2g.id, type=AccessType.view.value),
                          ], list(f1c.block.accesses.values()))
        self.assertEqual([BlockAccess(block_id=f2c.id, usergroup_id=t1g.id, type=AccessType.owner.value),
                          BlockAccess(block_id=f2c.id, usergroup_id=t2g.id, type=AccessType.edit.value),
                          ], list(f2c.block.accesses.values()))
        self.assertEqual([BlockAccess(block_id=d2c.id, usergroup_id=t1g.id, type=AccessType.owner.value),
                          BlockAccess(block_id=d2c.id, usergroup_id=t2g.id, type=AccessType.teacher.value),
                          ], list(d2c.block.accesses.values()))
        self.assertEqual([BlockAccess(block_id=d2.id, usergroup_id=t1g.id, type=AccessType.owner.value),
                          BlockAccess(block_id=d2.id, usergroup_id=t2g.id, type=AccessType.teacher.value),
                          ], list(d2.block.accesses.values()))
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

    def test_copy_to_existing(self):
        self.login_test2()
        d1 = self.create_doc(self.get_personal_item_path('a/d1'))
        d2 = self.create_doc(self.get_personal_item_path('a/d2'))
        f1d1 = self.create_doc(self.get_personal_item_path('a/f1/d1'))
        a = Folder.find_by_path(self.get_personal_item_path('a'))
        preview = self.json_post(f'/copy/{a.id}/preview',
                                 {'destination': self.get_personal_item_path('b'), 'exclude': ''})
        self.assertEqual(
            {'dest_exists': False,
             'preview': [
                 {'from': 'users/test-user-2/a/d1', 'to': 'users/test-user-2/b/d1'},
                 {'from': 'users/test-user-2/a/d2', 'to': 'users/test-user-2/b/d2'},
                 {'from': 'users/test-user-2/a/f1', 'to': 'users/test-user-2/b/f1'},
                 {'from': 'users/test-user-2/a/f1/d1',
                  'to': 'users/test-user-2/b/f1/d1'},
             ],
             }, preview
        )

        d1 = self.create_doc(self.get_personal_item_path('b/x1'))
        d2 = self.create_doc(self.get_personal_item_path('b/x2'))
        f1d1 = self.create_doc(self.get_personal_item_path('b/f1/x1'))

        preview = self.json_post(f'/copy/{a.id}/preview',
                                 {'destination': self.get_personal_item_path('b'), 'exclude': ''})
        self.assertEqual(
            {'dest_exists': True,
             'preview': [
                 {'from': 'users/test-user-2/a/d1', 'to': 'users/test-user-2/b/d1'},
                 {'from': 'users/test-user-2/a/d2', 'to': 'users/test-user-2/b/d2'},
                 {'from': 'users/test-user-2/a/f1', 'to': 'users/test-user-2/b/f1'},
                 {'from': 'users/test-user-2/a/f1/d1',
                  'to': 'users/test-user-2/b/f1/d1'},
             ],
             }, preview)
        preview = self.json_post(
            f'/copy/{a.id}',
            {'destination': self.get_personal_item_path('b'), 'exclude': ''}
        )
        preview = self.json_post(
            f'/copy/{a.id}',
            {'destination': self.get_personal_item_path('b'), 'exclude': ''},
            expect_content='Document already exists at path users/test-user-2/b/d1',
            expect_status=403,
        )

    def test_copy_permission(self):
        self.login_test1()
        d = self.create_doc(self.get_personal_item_path('perm/a'))
        d = self.create_doc(self.get_personal_item_path('perm/b'))
        f = d.parent
        self.test_user_2.grant_access(f, AccessType.view)
        db.session.commit()
        self.login_test2()
        self.json_post(
            f'/copy/{f.id}',
            {'destination': self.get_personal_item_path('perm'), 'exclude': ''},
            expect_content='Missing copy access to folder users/test-user-1/perm',
            expect_status=403,
        )
        self.test_user_2.grant_access(f, AccessType.copy)
        db.session.commit()
        self.json_post(
            f'/copy/{f.id}',
            {'destination': self.get_personal_item_path('perm'), 'exclude': ''},
            expect_content='Missing copy access to document users/test-user-1/perm/a',
            expect_status=403,
        )
        self.login_test1()
        d = DocEntry.find_by_path(self.get_personal_item_path('perm/a'))
        self.test_user_2.grant_access(d, AccessType.copy)
        db.session.commit()
        d = DocEntry.find_by_path(self.get_personal_item_path('perm/b'))
        self.test_user_2.grant_access(d, AccessType.copy)
        db.session.commit()
        self.login_test2()
        self.json_post(
            f'/copy/{f.id}',
            {'destination': self.get_personal_item_path('perm'), 'exclude': ''},
        )

        # Make sure the copier is now the owner, and that the copy right has not been copied.
        d = DocEntry.find_by_path(self.get_personal_item_path('perm/b'))
        self.assertEqual(
            {(AccessType.owner, self.test_user_2.get_personal_group())},
            {(a.access_type, a.usergroup) for a in d.block.accesses.values()},
        )

        # Make sure the original rights did not change.
        self.login_test1()
        d = DocEntry.find_by_path(self.get_personal_item_path('perm/b'))
        self.assertEqual(
            {
                (AccessType.copy, self.test_user_2.get_personal_group()),
                (AccessType.owner, self.test_user_1.get_personal_group()),

            },
            {(a.access_type, a.usergroup) for a in d.block.accesses.values()},
        )

    def test_copy_regression(self):
        self.login_test1()
        _ = self.create_doc(self.get_personal_item_path('x/templates/b'))
        f = Folder.find_by_path(self.get_personal_item_path('x'))
        self.json_post(f'/copy/{f.id}',
                       {'destination': self.get_personal_item_path('c'), 'exclude': None})


class FolderParentTest(TimRouteTest):
    def test_folder_parents(self):
        self.login_test1()
        fname = self.get_personal_item_path('x/y/z')
        self.create_folder(fname)
        f = Folder.find_by_path(fname)
        self.assertEqual(['y', 'x', 'Test user 1', 'users', 'All documents'], [p.title for p in f.parents_to_root()])
        f = Folder.find_by_path('users')
        self.assertEqual(['All documents'], [p.title for p in f.parents_to_root()])

    def test_root_parents(self):
        f = Folder.get_root()
        self.assertEqual([], [p.title for p in f.parents_to_root()])


class FolderContentTest(TimRouteTest):
    def test_folder_content(self):
        self.login_test1()
        d = self.create_doc()
        d_id = d.id
        docpath = d.path
        doctitle = d.title
        docname = d.short_name
        folder = self.test_user_1.get_personal_folder()
        folderpath = folder.path
        User.get_anon().grant_access(folder, AccessType.view)
        db.session.commit()
        self.login_test2()
        self.get('/getItems', query_string={'folder': folderpath},
                 expect_content=[])
        User.get_anon().grant_access(d, AccessType.view)
        db.session.commit()
        self.get('/getItems', query_string={'folder': folderpath},
                 expect_content=[{
                     'id': d_id,
                     'isFolder': False,
                     'location': folderpath,
                     'modified': 'just now',
                     'name': docname,
                     'owners': [{'id': self.get_test_user_1_group_id(), 'name': self.test_user_1.name}],
                     'path': docpath,
                     'public': True,
                     'rights': {'browse_own_answers': True,
                                'can_comment': True,
                                'can_mark_as_read': True,
                                'copy': False,
                                'editable': False,
                                'manage': False,
                                'owner': False,
                                'see_answers': False,
                                'teacher': False},
                     'title': doctitle,
                     'unpublished': False
                 }])
