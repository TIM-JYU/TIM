import datetime

from timApp.tests.browser.browsertest import BrowserTest
from timApp.util.utils import get_current_time, static_tim_doc


class LectureTest(BrowserTest):
    def test_lecture_qst_visible(self):
        self.login_test1()
        current_time = get_current_time()
        start_time = current_time - datetime.timedelta(minutes=15)
        end_time = current_time + datetime.timedelta(hours=2)
        lecture_code = "test lecture"
        doc = self.create_doc(from_file=static_tim_doc("questions.md"))
        j = self.json_post(
            "/createLecture",
            json_data=dict(
                doc_id=doc.id,
                end_time=end_time,
                lecture_code=lecture_code,
                max_students=50,
                start_time=start_time,
            ),
        )
        self.login_browser_quick_test1()
        self.goto_document(doc, view="lecture")
        self.wait_until_present_and_vis("tim-qst")
        qst_text = self.find_element_avoid_staleness("tim-qst p")
        self.assertEqual("1) Today", qst_text.text)
