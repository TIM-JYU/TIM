from timApp.tests.server.timroutetest import TimRouteTest


class ShowfileTest(TimRouteTest):
    def test_show_file_basic(self):
        from flask import Flask
        from werkzeug.serving import make_server
        import threading

        app = Flask(__name__)

        @app.get("/ping")
        def ping():
            return {"status": "ok"}

        server = make_server("", 8080, app)

        def run_server():
            server.serve_forever()

        t = threading.Thread(target=run_server)
        t.start()

        try:
            self.login_test1()
            d = self.create_doc(
                initial_par=f"""
#- {{#t plugin=showCode}}
file: http://tests:8080/ping
"""
            )
            self.assert_content(
                self.get(d.url, as_tree=True), ['{"status":"ok"}\nping']
            )
        finally:
            server.shutdown()
            t.join()

    def test_no_local_file_access(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {#t plugin=showCode}
file: file:///tmp/cache/something
        """
        )
        self.assert_content(
            self.get(d.url, as_tree=True),
            ["URL scheme must be http or https, got 'file'something"],
        )
