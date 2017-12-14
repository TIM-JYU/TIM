from timApp.documentmodel.specialnames import TEMPLATE_FOLDER_NAME
from timApp.documentmodel.timjsonencoder import TimJsonEncoder
from timApp.routes.view import get_templates_for_folder
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.tim_models import db


class TemplateTest(TimRouteTest):

    def test_templates(self):
        self.login_test1()
        folder = self.current_user.get_personal_folder().path
        db.session.expire_on_commit = False
        t1 = self.create_doc(f'{folder}/a/{TEMPLATE_FOLDER_NAME}/T1')
        t1json = t1.to_json()
        t2 = self.create_doc(f'{folder}/a/{TEMPLATE_FOLDER_NAME}/T2')
        t2json = t2.to_json()

        # TODO possibly move this to Item class
        for t in t1json, t2json:
            t['owner'] = TimJsonEncoder().default(t['owner'])

        d = self.create_doc(f'{folder}/a/test')
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

    def test_automatic_template(self):
        self.login_test1()
        folder = self.current_user.get_personal_folder().path
        t1 = self.create_doc(f'{folder}/b/{TEMPLATE_FOLDER_NAME}/T1', initial_par='t1')
        t2 = self.create_doc(f'{folder}/b/{TEMPLATE_FOLDER_NAME}/force', initial_par='forced')
        d = self.create_doc(f'{folder}/b/new')
        self.assertEqual('forced', d.document.get_paragraphs()[0].get_markdown())
        d = self.create_doc(f'{folder}/b/new2', template='T1')
        self.assertEqual('t1', d.document.get_paragraphs()[0].get_markdown())
        d = self.create_doc(f'{folder}/b/new3', template='nonexistent')
        self.assertEqual('forced', d.document.get_paragraphs()[0].get_markdown())

        # Should be created directly from URL
        self.get(f'/view/{folder}/b/new-from-url')

        # Make sure folder creation does not trigger template loading code.
        self.create_folder(f'{folder}/b/newfolder')

    def test_templates_of_templates(self):
        """Templates are not templates of themselves."""
        self.login_test1()
        folder = self.current_user.get_personal_folder().path
        t1 = self.create_doc(f'{folder}/c/{TEMPLATE_FOLDER_NAME}/T1')
        t2 = self.create_doc(f'{folder}/c/{TEMPLATE_FOLDER_NAME}/T2')
        template_folder = t1.parent
        templates = get_templates_for_folder(template_folder)
        self.assertEqual(0, len(templates))
        templates = get_templates_for_folder(template_folder.parent)
        self.assertEqual(2, len(templates))
