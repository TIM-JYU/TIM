from flask_testing import LiveServerTestCase


class TimLiveServer(LiveServerTestCase):
    def create_app(self):
        from timApp.tim_app import app
        return app

    def setUp(self):
        super().setUp()
        self.client = self.app.test_client()
        self.client.__enter__()

    def tearDown(self):
        self.client.__exit__(None, None, None)
