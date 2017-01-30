import os

from flask_testing import LiveServerTestCase
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver import DesiredCapabilities
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as ec
from selenium.webdriver.support.wait import WebDriverWait

from tests.db.timdbtest import TEST_USER_1_NAME, TEST_USER_2_NAME, TEST_USER_3_NAME
from tests.server.timroutetest import TimRouteTest


class RemoteControls:
    PHANTOMJS = 'http://phantomjs:4444'  # Fastest - login test ~ 2 sec, but inaccurate emulation of browser
    CHROME = 'http://chrome:4444/wd/hub'  # Slower - login test ~ 8 sec
    FIREFOX = 'http://firefox:4444/wd/hub'  # Very slow - login test ~ 20 sec


class BrowserTest(LiveServerTestCase, TimRouteTest):
    login_dropdown_path = '//login-menu/div/button'

    def setUp(self):
        super(BrowserTest, self).setUp()
        self.drv = webdriver.Remote(command_executor=self.app.config['SELENIUM_REMOTE_URL'] + ':4444/wd/hub',
                                    desired_capabilities=DesiredCapabilities.CHROME.copy())
        self.drv.implicitly_wait(10)
        self.client = self.app.test_client()
        self.client.__enter__()
        self.wait = WebDriverWait(self.drv, 10)

    def login_browser_as(self, email: str, password: str, name: str):
        self.client.__exit__(None, None, None)
        self.goto('')
        elem = self.drv.find_element_by_xpath('//login-menu/button')
        elem.click()
        elem.find_element_by_xpath("//input[@type='email']").send_keys(email)
        elem.find_element_by_xpath("//input[@type='password']").send_keys(password)
        elem.find_element_by_xpath("//button[@type='submit']").click()
        self.wait.until(ec.text_to_be_present_in_element((By.XPATH, self.login_dropdown_path), name))
        self.client.__enter__()

    def login_browser_test1(self):
        """Logs in as Test user 1."""
        self.login_browser_as('test1@example.com', 'test1pass', TEST_USER_1_NAME)

    def login_browser_test2(self):
        """Logs in as Test user 2."""
        self.login_browser_as('test2@example.com', 'test2pass', TEST_USER_2_NAME)

    def login_browser_test3(self):
        """Logs in as Test user 3."""
        self.login_browser_as('test3@example.com', 'test3pass', TEST_USER_3_NAME)

    def goto(self, url: str):
        """Navigates to a new URL using the browser.

        :param url: The URL to which to navigate. This must be relative.

        """
        self.drv.get("{}:{}{}".format(self.app.config['SELENIUM_BROWSER_URL'], self.app.config['LIVESERVER_PORT'], url))

    def save_screenshot(self, filename: str):
        """Saves the current browser screen to a PNG file in screenshots directory.

        :param filename: The file name of the PNG file.

        """
        screenshot_dir = '/service/screenshots/'
        os.makedirs(screenshot_dir, exist_ok=True)
        if not self.drv.save_screenshot(screenshot_dir + filename + '.png'):
            raise Exception('Screenshot failed')

    def should_not_exist(self, css_selector: str):
        """Asserts that the current document should not contain any elements that match the specified CSS selector.

        :param css_selector: The CSS selector to test.

        """
        self.drv.implicitly_wait(0.5)
        try:
            self.drv.find_element_by_css_selector(css_selector)
        except NoSuchElementException:
            pass
        else:
            self.assertTrue(False, 'Selector "{}" matched something.'.format(css_selector))
        finally:
            self.drv.implicitly_wait(10)

    def create_app(self):
        from tim_app import app
        return app

    def tearDown(self):
        self.drv.quit()
        self.client.__exit__(None, None, None)
