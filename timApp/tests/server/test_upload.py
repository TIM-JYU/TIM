import io
import json

from timApp.document.docentry import DocEntry
from timApp.document.document import Document
from timApp.folder.folder import Folder
from timApp.item.block import BlockType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.upload.uploadedfile import StampedPDF, UploadedFile
from timApp.user.usergroup import UserGroup
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
        self.login_test3()
        j = self.create_folder(fname)
        grant_access(self.test_user_1.get_personal_group(), Folder.get_by_id(j['id']), 'edit')
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
        self.get_no_warn(f'/files/{j["file"]}', expect_content='test file', expect_mimetype='text/plain')
        self.get(f'/files/1{j["file"]}', expect_status=404)
        self.get(f'/files/{j["file"]}x', expect_status=404)

        self.login_test2()
        self.get(f'/files/{j["file"]}', expect_status=403)
        grant_access(self.test_user_2.get_personal_group(), d, 'view')
        self.get_no_warn(f'/files/{j["file"]}', expect_content='test file')

    def test_upload_image(self):
        self.login_test1()
        di, j = self.create_doc_with_image()
        self.get_no_warn(f'/images/{j}', expect_content='GIF87a')
        self.get(f'/images/1{j}', expect_status=404)
        self.get(f'/images/{j}x', expect_status=404)

        self.login_test2()
        self.get(f'/images/{j}', expect_status=403)
        grant_access(self.test_user_2.get_personal_group(), di, 'view')
        self.get_no_warn(f'/images/{j}', expect_content='GIF87a', expect_mimetype='image/gif')

    def test_upload_copy_doc(self):
        self.login_test1()
        di, j = self.create_doc_with_image()
        d = self.create_doc(copy_from=di.id)
        copy_id = d.id
        grant_access(self.test_user_2.get_personal_group(), d, 'view')
        self.login_test2()
        self.get_no_warn(f'/images/{j}', expect_content='GIF87a')
        grant_access(self.test_user_2.get_personal_group(), di, 'view')
        self.test_user_2.remove_access(copy_id, 'view')
        db.session.commit()
        self.get_no_warn(f'/images/{j}', expect_content='GIF87a')
        self.test_user_2.remove_access(di.id, 'view')
        db.session.commit()
        self.get(f'/images/{j}', expect_status=403)

    def create_doc_with_image(self):
        d = self.create_doc()
        d_id = d.id
        j = self.upload_file(d, b'GIF87a', 'test.jpg')
        return DocEntry.find_by_id(d_id), j['image']

    def test_upload_and_stamp(self):
        self.login_test1()
        # Document does not really need any macros for stamping to work, but might in the future.
        d = self.create_doc(initial_par=r"""
{% macro liite(selitys,liiteNro,lista,linkki) -%}
{% set server = "https://tim.jyu.fi" %}
{% if linkki.startswith('http') %}
{% set server = "" %}
{% endif %}
iframe: true
open: false
videoicon: false
xdocicon: false
stem: "%%selitys%%"
hidetext: Piilota liite
type: list
videoname: "(LIITE %%liiteNro%% / lista %%lista%%,"
text: Kokous %%dates[knro][0]%% \newline LIITE %%liiteNro%% / lista %%lista%%
doctext: ")"
doclink: %%linkki%%
stamped-file:
width: 800
height: 600
file: %%linkki%%
texprint: "- %%selitys%% ([LIITE %%liiteNro%% / lista %%lista%%](%%server+linkki%%))"
{%- endmacro %}

%%liite("Tutkinnot 2018", "A", "3", "")%%
        """)
        with open('tests/server/expected/printing/hello_1_2.pdf', 'rb') as f:
            content = f.read()
            file_len = len(content)
            r = self.upload_file(
                d,
                content,
                'test.pdf',
                attachmentParams=json.dumps(["30.1.2019", "", "Tutkinnot 2018", "A", "3", "", True]),
            )
        file_id = int(r['file'].split('/')[0])
        pdf = UploadedFile.find_by_id_and_type(file_id, BlockType.File)
        pdf = StampedPDF(pdf.block)
        self.assertEqual(15985, len(pdf.data))
        self.assertGreater(len(pdf.data), file_len)
        self.check_mime(pdf.relative_filesystem_path, 'application/pdf')

    def check_mime(self, f, expected):
        self.get_no_warn(f'/files/{f}', expect_mimetype=expected)

    def test_upload_mimetype(self):
        self.login_test1()
        d = self.create_doc()
        r = self.upload_file(d, b"""
<!doctype html>
<html>
<head>
<title>Surprise</title>
</head>
<body>
<script>alert('hi')</script>
</body>
</html>
        """, 'test.html')
        self.check_mime(r['file'], 'text/plain')
        r = self.upload_file(d, b"""<svg xmlns="http://www.w3.org/2000/svg"></svg>""", 'test.svg')
        self.check_mime(r['file'], 'image/svg+xml')
