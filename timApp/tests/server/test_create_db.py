from timApp.tests.server.timroutetest import TimRouteTest


class CreateDbTest(TimRouteTest):
    create_docs = True

    def test_initial_docs_exist(self):
        """Initial documents are created."""
        self.login_test1()
        self.get('/view/tim/Eri-ohjelmointikielia')
        self.get('/view/tim/mmcq-example')
