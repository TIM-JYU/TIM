from tests.server.timroutetest import TimRouteTest


class EditTest(TimRouteTest):

    def test_nonexistent_edit(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        par_id = d.document.get_paragraphs()[0].get_id()
        invalid_par = 'nonexistent'
        self.json_post('/deleteParagraph/{}'.format(d.id), {'par': invalid_par},
                       expect_status=400,
                       expect_content={'error': 'Paragraph {} does not exist'.format(invalid_par)})
        self.json_post('/deleteParagraph/{}'.format(d.id), {'area_start': invalid_par, 'area_end': par_id},
                       expect_status=400,
                       expect_content={'error': 'Paragraph {} does not exist'.format(invalid_par)})
        self.json_post('/deleteParagraph/{}'.format(d.id), {'area_start': par_id, 'area_end': invalid_par},
                       expect_status=400,
                       expect_content={'error': 'Paragraph {} does not exist'.format(invalid_par)})
