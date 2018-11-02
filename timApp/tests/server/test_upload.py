import io

from timApp.document.docentry import DocEntry
from timApp.document.document import Document
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.user.usergroup import UserGroup
from timApp.timdb.sqa import db
from timApp.user.userutils import grant_access


class UploadTest(TimRouteTest):

    def test_upload_permissions(self):
        u = self.test_user_3
        u.groups.append(UserGroup.get_admin_group())
        db.session.commit()
        self.login_test1()
        j = self.post('/upload/',
                      data={'folder': self.current_user.get_personal_folder().path,
                            'file': (io.BytesIO(b'test file'), 'test.md')})
        self.post('/upload/',
                  data={'folder': self.current_user.get_personal_folder().path,
                        'file': (io.BytesIO(b'test file'), 'test2')})
        format_error = {"error": "Only markdown files are allowed. This file appears to be application/octet-stream."}
        self.post('/upload/',
                  data={'folder': self.current_user.get_personal_folder().path,
                        'file': (io.BytesIO(b'\x00'),
                                 'test2.md')},
                  expect_status=400,
                  expect_content=format_error)
        doc = Document(j['id'])
        self.post(f'/update/{doc.doc_id}',
                  data={'file': (io.BytesIO(b'testing'), 'test.md'), 'original': doc.export_markdown()})
        self.post(f'/update/{j["id"]}',
                  data={'file': (io.BytesIO(b'\x00'),
                                 'test.md')},
                  expect_status=400,
                  expect_content=format_error)
        fname = 'custom'
        self.post('/upload/',
                  data={'folder': fname,
                        'file': (io.BytesIO(b'test file'), 'test.md')},
                  expect_status=403,
                  expect_content={'error': 'You cannot create documents in this folder.'})
        test1_group = self.current_group().id
        self.login_test3()
        j = self.create_folder(fname)
        grant_access(test1_group, j['id'], 'edit')

        self.login_test1()
        self.post('/upload/',
                  data={'folder': fname,
                        'file': (io.BytesIO(b'test file'), 'test.md')},
                  expect_status=200)

    def test_upload_file(self):
        self.login_test1()
        d = self.create_doc()
        d_id = d.id
        j = self.upload_file(d, b'test file', 'test.md')
        d = DocEntry.find_by_id(d_id)
        up_id = d.block.children[0].id
        self.assertEqual(f'{up_id}/test.md', j['file'])
        self.get_no_warn(f'/files/{j["file"]}', expect_content='test file')
        self.get(f'/files/1{j["file"]}', expect_status=404)
        self.get(f'/files/{j["file"]}x', expect_status=404)

        self.login_test2()
        self.get(f'/files/{j["file"]}', expect_status=403)
        grant_access(self.get_test_user_2_group_id(), d_id, 'view')
        self.get_no_warn(f'/files/{j["file"]}', expect_content='test file')

    def test_upload_image(self):
        self.login_test1()
        d_id, j = self.create_doc_with_image()
        self.get_no_warn(f'/images/{j}', expect_content='GIF87a')
        self.get(f'/images/1{j}', expect_status=404)
        self.get(f'/images/{j}x', expect_status=404)

        self.login_test2()
        self.get(f'/images/{j}', expect_status=403)
        grant_access(self.get_test_user_2_group_id(), d_id, 'view')
        self.get_no_warn(f'/images/{j}', expect_content='GIF87a')

    def test_upload_copy_doc(self):
        self.login_test1()
        d_id, j = self.create_doc_with_image()
        d = self.create_doc(copy_from=d_id)
        copy_id = d.id
        grant_access(self.get_test_user_2_group_id(), d.id, 'view')
        self.login_test2()
        self.get_no_warn(f'/images/{j}', expect_content='GIF87a')
        grant_access(self.get_test_user_2_group_id(), d_id, 'view')
        self.test_user_2.remove_access(copy_id, 'view')
        db.session.commit()
        self.get_no_warn(f'/images/{j}', expect_content='GIF87a')
        self.test_user_2.remove_access(d_id, 'view')
        db.session.commit()
        self.get(f'/images/{j}', expect_status=403)

    def create_doc_with_image(self):
        d = self.create_doc()
        d_id = d.id
        j = self.upload_file(d, b'GIF87a', 'test.jpg')
        return d_id, j['image']
