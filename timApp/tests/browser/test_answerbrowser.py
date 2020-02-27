from timApp.tests.browser.browsertest import BrowserTest, PREV_ANSWER
from timApp.document.docentry import DocEntry
from timApp.user.usergroup import UserGroup
from timApp.timdb.sqa import db
from timApp.util.utils import EXAMPLE_DOCS_PATH


class AnswerBrowserTest(BrowserTest):

    def test_referenced_area_plugin(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(from_file=f'{EXAMPLE_DOCS_PATH}/multiple_mmcqs.md')
        d2 = self.create_doc(initial_par=f'#- {{rd={d.id} ra=a1}}')
        self.check_reference_answerbrowser_ok(d2)

        # even if the original document is not accessible, browsing answers should work in the other document
        d.block.set_owner(UserGroup.get_admin_group())
        db.session.commit()
        self.check_reference_answerbrowser_ok(d2)

    def check_reference_answerbrowser_ok(self, d: DocEntry):
        self.goto_document(d)
        selector = '#mmcqexample mmcq button'
        self.find_element_avoid_staleness(selector, click=True)
        self.wait_and_click(PREV_ANSWER)
        self.should_not_exist('answerbrowser .alert-danger')
