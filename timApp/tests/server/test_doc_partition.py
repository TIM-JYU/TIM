from timApp.tests.server.timroutetest import TimRouteTest


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
        # Begin index is at the doc end, and is rounded to avoid a short part.
        self.get(f'/viewrange/get/{d2.id}/10/{forwards}', expect_content={'b': 5, 'e': 10})
        # Begin index is 5 and moving backwards.
        self.get(f'/viewrange/get/{d2.id}/5/{backwards}', expect_content={'b': 0, 'e': 5})
        # Begin index is at doc beginning and moving backwards, the is rounded to avoid a short part.
        self.get(f'/viewrange/get/{d2.id}/0/{backwards}', expect_content={'b': 0, 'e': 5})
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 8},
                       expect_cookie=('r','8'),
                       expect_status=200)
        # Test rounding when remaining pars are shorter than half the piece size.
        self.get(f'/viewrange/get/{d2.id}/0/{forwards}', expect_content={'b': 0, 'e': 10})

    def test_partitioning_document(self):
        self.make_admin(self.test_user_1)
        self.login_test1()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)
        d = self.create_doc(initial_par=["1","Kissa","3","4","5","Koira","7","8","9","10"])
        tree = self.get(f"view/{d.path}", as_tree=True)
        pars = tree.cssselect('.parContent > p')
        # Check if there are right amount of pars.
        self.assertEqual(len(pars), 5)
        # Check that correct pars were loaded.
        self.assertIn('Kissa', pars[1].text)
