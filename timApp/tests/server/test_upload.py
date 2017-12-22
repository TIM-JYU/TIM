import io

from timApp.documentmodel.document import Document
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.tim_models import db
from timApp.timdb.userutils import grant_access


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
