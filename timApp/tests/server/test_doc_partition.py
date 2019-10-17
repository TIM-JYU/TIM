from timApp.tests.server.timroutetest import TimRouteTest
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME, PREAMBLE_FOLDER_NAME

class DocPartitionTest(TimRouteTest):

    def test_set_and_unset_view_range_cookie(self):
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 10},
                       expect_cookie=('r','10'),
                       expect_status=200)
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': -10},
                       expect_content="Invalid piece size",
                       expect_cookie=('r', None),
                       expect_status=400)
        self.get(url=f'/viewrange/unset/piecesize',
                 expect_cookie=('r', None),
                 expect_status=200)


    def test_calculating_part_indices(self):
        self.make_admin(self.test_user_1)
        self.login_test1()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)
        forwards = 1
        backwards = 0
        d1 = self.create_doc()
        d2 = self.create_doc(initial_par=["1","2","3","4","5","6","7","8","9","10"])
        # Empty document.
        self.get(f'/viewrange/get/{d1.id}/0/1', expect_content={'b': 0, 'e': 0})
        # Begin index at the doc beginning.
        self.get(f'/viewrange/get/{d2.id}/0/{forwards}', expect_content={'b': 0, 'e': 5})
        # Begin index is at the doc end, and is rounded to avoid a too short part.
        self.get(f'/viewrange/get/{d2.id}/10/{forwards}', expect_content={'b': 8, 'e': 10})
        # Begin index is 5 and moving backwards.
        self.get(f'/viewrange/get/{d2.id}/5/{backwards}', expect_content={'b': 0, 'e': 5})
        # Begin index is at doc beginning and moving backwards, the is rounded to avoid a too short part.
        self.get(f'/viewrange/get/{d2.id}/0/{backwards}', expect_content={'b': 0, 'e': 2})
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 8},
                       expect_cookie=('r','8'),
                       expect_status=200)
        # Test rounding when remaining pars are shorter than half the piece size.
        self.get(f'/viewrange/get/{d2.id}/0/{forwards}', expect_content={'b': 0, 'e': 10})


    def test_partitioning_document(self):
        self.make_admin(self.test_user_1)
        self.login_test1()
        d = self.create_doc(initial_par=["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"])

        # No partitioning.
        tree = self.get(f"view/{d.path}", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(10, len(pars))

        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)

        # Partitioning on, no URL parameters.
        tree = self.get(f"view/{d.path}", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(5, len(pars))
        self.assertIn('2', pars[1].text)

        # Partitioning with URL parameters, mid-document range.
        tree = self.get(f"view/{d.path}?b=2&e=6", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(4, len(pars))
        self.assertIn('3', pars[0].text)
        self.assertIn('6', pars[3].text)

        # Partitioning with URL parameters, whole document range.
        tree = self.get(f"view/{d.path}?b=0&e=10", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(10, len(pars))


    def test_partitioning_document_with_overflowing_range(self):
        self.make_admin(self.test_user_1)
        self.login_test1()
        d = self.create_doc(initial_par=["1","Kissa","3","4","5","6","Koira","8","9","10"])
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)

        # Overflowing range end.
        tree = self.get(f"view/{d.path}?b=6&e=100", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(4, len(pars))
        self.assertIn(pars[0].text, 'Koira')

        # Negative range begin.
        tree = self.get(f"view/{d.path}?b=-100&e=4", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(4, len(pars))
        self.assertIn(pars[1].text, 'Kissa')


    def test_partitioning_empty_document(self):
        self.make_admin(self.test_user_1)
        self.login_test1()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r', '5'),
                       expect_status=200)
        d = self.create_doc()

        tree = self.get(f"view/{d.path}?b=3&e=14", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(0, len(pars))


    def test_partitioning_with_preambles(self):
        self.make_admin(self.test_user_1)
        self.login_test1()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)
        d = self.create_doc(
            initial_par=["1","Kissa","3","4","5","6","Koira","8","9","10"])
        d_preamble = self.create_doc(
            path=self.get_personal_item_path(f'{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/preamble'),
            initial_par=["Preamble par 1", "Preamble par 2"])

        # Partitioning on, no URL parameters.
        tree = self.get(f"view/{d.path}", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(7, len(pars))
        self.assertIn('Preamble paragraph 1', pars[0].text)
        self.assertIn('Kissa', pars[3].text)

        # Partitioning with URL parameters, starting from doc beginning.
        tree = self.get(f"view/{d.path}?b=0&e=6&preamble=true", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(8, len(pars))
        self.assertIn('Preamble par 1', pars[0].text)
        self.assertIn('Kissa', pars[3].text)

        # Partitioning with URL parameters, starting from mid-document.
        tree = self.get(f"view/{d.path}?b=6&e=10&preamble=true", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(4, len(pars))
        self.assertIn(pars[0].text, 'Koira')


    def test_partitioning_with_special_class_preambles(self):
        self.make_admin(self.test_user_1)
        self.login_test1()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)
        d = self.create_doc(initial_par=["1","Kissa","3","4","5","6","Koira","8","9","10"])
        d_preamble = self.create_doc(
            path=self.get_personal_item_path(f'{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/preamble'),
            initial_par=["Preamble par 1", "Preamble par 2", """
#- {.includeInParts}
Preamble par 3
"""])

        # Partitioning on, no URL parameters; all pars should be included.
        tree = self.get(f"view/{d.path}", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(8, len(pars))
        self.assertIn('Preamble par 1', pars[0].text)
        self.assertIn('Kissa', pars[4].text)

        # Partitioning with URL parameters, starting from mid-document; special par should be included.
        tree = self.get(f"view/{d.path}?b=6&e=10&preamble=true", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        self.assertEqual(5, len(pars))
        self.assertIn(pars[0].text, 'Preamble par 3')

    def test_partitioning_range(self):
        # TODO: Range check not implemented yet.
        pass