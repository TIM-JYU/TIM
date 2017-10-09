"""Server tests for race conditions."""
from threading import Thread

from timApp.tests.server.timroutetest import TimRouteTest


class RaceTest(TimRouteTest):
    def test_race_bookmark(self):
        self.login_test1()
        d1 = self.create_doc()
        d2 = self.create_doc()
        def mark_read():
            for i in range(0, 100):
                self.json_post(f'/bookmarks/markLastRead/{d1.id}')
                self.json_post(f'/bookmarks/markLastRead/{d2.id}')
        t1 = Thread(target=mark_read)
        t2 = Thread(target=mark_read)
        t1.start()
        t2.start()
        t1.join()
        t2.join()
