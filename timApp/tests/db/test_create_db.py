from timApp.tests.server.timroutetest import TimRouteTest


class CreateDbTest(TimRouteTest):
    create_docs = True

    @classmethod
    def setUpClass(cls):
        super().setUpClass()

    def test_create_db(self):
        """Initial documents are created."""
        self.get('/view/testaus-1')
        self.get('/view/testaus-2')
