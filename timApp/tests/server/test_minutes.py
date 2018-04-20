from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.models.docentry import DocEntry


class MinutesHandlingTest(TimRouteTest):

    def test_minutes_creation(self):
        # Tests creation of minutes based on a meeting invitation document
        self.login_test1()
        nr = 3
        d = self.create_doc(settings={"macros": {"nr": nr}})
        minutes_document_path = f"{d.location}/PK/PK{nr}"
        self.json_post("/minutes/createMinutes",
                       json_data={"item_path": minutes_document_path,
                                  "item_title": f"PK{nr}",
                                  "copy": d.id},
                       expect_status=200)
        d2 = DocEntry.find_by_path(minutes_document_path, try_translation=False)
        self.assertIsNotNone(d2)
        self.assertTrue(d2.document.get_settings().is_minutes())

