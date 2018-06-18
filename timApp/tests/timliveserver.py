import types

from flask_testing import LiveServerTestCase


class TimLiveServer(LiveServerTestCase):
    def create_app(self):
        from timApp.tim_app import app
        old_run = app.run

        # Flask-Testing package does not allow run parameters to be configured, so we do it here.
        # Threading is needed because qst plugin lives in the same server process.
        def threaded_run(_, **kwargs):
            kwargs['threaded'] = True
            old_run(**kwargs)

        # noinspection PyArgumentList
        app.run = types.MethodType(threaded_run, app)
        return app

    def setUp(self):
        super().setUp()
        self.client = self.app.test_client()
        self.client.__enter__()

    def tearDown(self):
        self.client.__exit__(None, None, None)
