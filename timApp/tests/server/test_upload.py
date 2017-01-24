import io

from documentmodel.document import Document
from tests.server.timroutetest import TimRouteTest


class UploadTest(TimRouteTest):
    def test_upload_permissions(self):
        db = self.get_db()
        db.users.addUserToAdmins(db.users.get_user_by_name('testuser3').id)
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
        self.post('/update/{}'.format(doc.doc_id),
                  data={'file': (io.BytesIO(b'testing'), 'test.md'), 'original': doc.export_markdown()})
        self.post('/update/{}'.format(j['id']),
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
        test1_group = db.users.get_personal_usergroup_by_id(self.current_user_id())
        self.login_test3()
        j = self.create_folder(fname)
        db.users.grant_access(test1_group, j['id'], 'edit')

        self.login_test1()
        self.post('/upload/',
                  data={'folder': fname,
                        'file': (io.BytesIO(b'test file'), 'test.md')},
                  expect_status=200)

