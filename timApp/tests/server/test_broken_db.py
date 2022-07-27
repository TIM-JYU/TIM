from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class BrokenDbTest(TimRouteTest):
    def test_broken_db(self):
        db.drop_all()
        db.create_all()

        # Expire all because otherwise User.query.get(0) would still return anonymous user.
        db.session.expire_all()

        with self.assertRaises(Exception) as e:
            self.get("/")
        self.assertEqual(
            """
Database has no users; you need to re-initialize it:
./tim dc down -v
delete tim_files folder
./tim up""".strip(),
            str(e.exception),
        )
