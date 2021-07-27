from selenium.webdriver.common.by import By

from timApp.tests.browser.browsertest import BrowserTest
from timApp.tests.db.timdbtest import TEST_USER_1_NAME


class LoginTest(BrowserTest):

    def test_login(self):
        self.login_browser_test1()
        self.assertIn(TEST_USER_1_NAME, self.drv.find_element(by=By.XPATH, value=self.login_dropdown_path).text)
