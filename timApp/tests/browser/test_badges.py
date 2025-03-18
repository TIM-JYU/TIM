from timApp.tests.browser.browsertest import BrowserTest
from timApp.static.scripts.tim.gamification.badge import *


class TestBadges(BrowserTest):
    def test_badge_creator(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
```
auto_number_headings: 2
form_mode: true
```   
"""
        )
        dt = self.create_translation(d)
        tr_par = dt.document.get_paragraphs()[1]
        tr_par.set_markdown(
            """
#- {allowangular=true}
<tim-badge-creator></tim-badge-creator>
        """
        )
        tr_par.save()
        self.goto_document(d)
        self.wait_until_present_and_vis("#showBadgeForm")
        open_creator = self.find_element_and_move_to("#showBadgeForm")
        open_creator.click()
