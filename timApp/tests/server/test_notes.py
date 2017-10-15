"""Server tests for notes."""
from timApp.tests.server.timroutetest import TimRouteTest


class NotesTest(TimRouteTest):
    def test_nonexistent_note(self):
        self.get('/note/999', expect_status=404)

    def test_nonexistent_note_post(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post('/postNote', {'text': 'a', 'access': 'everyone', 'docId': d.id, 'par': 'x'},
                       expect_status=404,
                       expect_content=f'Document {d.id}: Paragraph not found: x',
                       json_key='error')
