from timApp.tests.server.timroutetest import TimRouteTest


class ItemCreateTest(TimRouteTest):
    def test_no_alias_under_doc(self):
        """It's not possible to create an alias whose path starts with another alias."""
        self.login_test1()
        d = self.create_doc()
        self.json_put(f'/alias/{d.id}/{d.path}/x',
                      {'public': True},
                      expect_status=403,
                      expect_content={'error': f'A document already exists at path {d.path}'})

    def test_no_folder_under_translation(self):
        self.login_test1()
        d = self.create_doc()
        tr = self.create_translation(d)
        self.create_folder(f'{tr.path}/x',
                           expect_status=403,
                           expect_content={'error': f'A document already exists at path {d.path}'})
