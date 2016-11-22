import os

from flask_testing import LiveServerTestCase
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver import DesiredCapabilities
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as ec
from selenium.webdriver.support.wait import WebDriverWait

from tests.db.timdbtest import TEST_USER_1_NAME
from tests.server.timroutetest import TimRouteTest


class RemoteControls:
    PHANTOMJS = 'http://phantomjs:4444'  # Fastest - login test ~ 2 sec, but inaccurate emulation of browser
    CHROME = 'http://chrome:4444/wd/hub'  # Slower - login test ~ 8 sec
    FIREFOX = 'http://firefox:4444/wd/hub'  # Very slow - login test ~ 20 sec


class BrowserTest(LiveServerTestCase, TimRouteTest):
    login_dropdown_path = '//login-menu/div/button'

    def setUp(self):
        self.drv = webdriver.Remote(command_executor=RemoteControls.CHROME,
                                    desired_capabilities=DesiredCapabilities.CHROME.copy())
        self.drv.implicitly_wait(10)
        self.client = self.app.test_client()
        self.client.__enter__()
        self.wait = WebDriverWait(self.drv, 10)

    def login_browser_test1(self):
        self.client.__exit__(None, None, None)
        self.goto('')
        elem = self.drv.find_element_by_xpath('//login-menu/button')
        elem.click()
        elem.find_element_by_xpath("//input[@type='email']").send_keys("test1@example.com")
        elem.find_element_by_xpath("//input[@type='password']").send_keys("test1pass")
        elem.find_element_by_xpath("//button[@type='submit']").click()
        self.wait.until(ec.text_to_be_present_in_element((By.XPATH, self.login_dropdown_path), TEST_USER_1_NAME))
        self.client.__enter__()

    def goto(self, url):
        self.drv.get("{}:{}{}".format(self.app.config['SELENIUM_URL'], self.app.config['LIVESERVER_PORT'], url))

    def save_screenshot(self, filename):
        screenshot_dir = '/service/screenshots/'
        os.makedirs(screenshot_dir, exist_ok=True)
        if not self.drv.save_screenshot(screenshot_dir + filename + '.png'):
            raise Exception('Screenshot failed')

    def should_not_exist(self, css_selector):
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
