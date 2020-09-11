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
                           expect_content={'error': f'A document already exists at path {tr.path}'})

    def test_doc_name_invalid(self):
        self.login_test1()
        self.create_doc(
            self.get_personal_item_path('x/.a'),
            expect_status=400,
            expect_content='Document short name cannot start or end with a dot.',
        )
        self.create_doc(
            self.get_personal_item_path('x/a.txt'),
        )
        self.create_doc(
            self.get_personal_item_path('x/a.'),
            expect_status=400,
            expect_content='Document short name cannot start or end with a dot.',
        )
        self.create_doc(
            self.get_personal_item_path('x.b/a.txt'),
            expect_status=400,
            expect_content='Item path segment cannot have dots.',
        )

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
        for c in 'ãàáäâåẽèéëêìíïîõòóöôùúüûñç·,:;<>|^~¨"!½#¤%&()=?`@£$€{[]}\\ ':
            self.create_folder(self.get_personal_item_path(c),
                               expect_content={
                                   'error': 'The folder path has invalid characters. Only letters, numbers, '
                                            'underscores and dashes are allowed.'},
                               expect_status=400)
        for c in ['.', '..', '....']:
            self.create_folder(
                self.get_personal_item_path(c),
                expect_content={
                    'error': 'Folder short name cannot have dots.'},
                expect_status=400)
        self.create_folder(
            self.get_personal_item_path('a.txt'),
            expect_content={
                'error': 'Folder short name cannot have dots.'},
            expect_status=400)
