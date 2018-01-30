import math
import os
from base64 import b64decode
from io import BytesIO
from pprint import pprint
from typing import Union, List

from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support import expected_conditions as ec
from selenium.webdriver.support.wait import WebDriverWait
from wand.exceptions import BaseError
from wand.image import Image

from timApp.tests.db.timdbtest import TEST_USER_1_NAME, TEST_USER_2_NAME, TEST_USER_3_NAME
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tests.timliveserver import TimLiveServer
from timApp.timdb.docinfo import DocInfo

PREV_ANSWER = 'answerbrowser .prevAnswer'


class BrowserTest(TimLiveServer, TimRouteTest):
    login_dropdown_path = '//login-menu/div/button'
    screenshot_dir = '/service/screenshots'

    def get_screenshot_tolerance(self) -> float:
        return 0.001

    def setUp(self):
        TimLiveServer.setUp(self)
        options = webdriver.ChromeOptions()

        options.set_headless()
        options.add_argument('--window-size=1024x768')
        self.drv = webdriver.Remote(command_executor=self.app.config['SELENIUM_REMOTE_URL'] + ':4444/wd/hub',
                                    desired_capabilities=options.to_capabilities())
        self.drv.implicitly_wait(10)
        self.wait = WebDriverWait(self.drv, 10)

    def login_browser_as(self, email: str, password: str, name: str):
        self.client.__exit__(None, None, None)
        self.goto('')
        # self.save_screenshot('adsasd')
        elem = self.drv.find_element_by_xpath('//login-menu/button')
        elem.click()
        elem.find_element_by_xpath("//input[@type='email']").send_keys(email)
        elem.find_element_by_xpath("//input[@type='password']").send_keys(password)
        elem.find_element_by_xpath("//button[@type='submit']").click()
        self.wait.until(ec.text_to_be_present_in_element((By.XPATH, self.login_dropdown_path), name))
        self.client.__enter__()

    def login_browser_quick_test1(self):
        """Logs testuser 1 in quickly by directly adding the session cookie to the browser."""
        self.goto("/empty")
        self.drv.delete_all_cookies()
        self.drv.add_cookie(
            {'class': 'org.openqa.selenium.Cookie',
             'domain': 'nginx',
             'expiry': 7544144177,
             'hCode': 1984987798,
             'httpOnly': True,
             'name': 'session',
             'path': '/',
             'secure': False,
             'value': '.eJwtjEEKwzAMBL8S9mwaDD351EfkboS7pQHLCYoDhdK_V4bcZpiVvsg7TaWxdaRuJwOklfdmSEBAEWV-2aausztV1urcefT44Ed0r7wV7wFGqbn5gffF-3QetCl6GZDXJ9L94ms1vgyP-P0BRcctlw.DBglMQ.vvva2NpZfDBi7dlJAJoKfg5uGQo'})

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
        url_ = f"{self.app.config['SELENIUM_BROWSER_URL']}{url}"
        # raise Exception(url_)
        self.drv.get(url_)

    def print_console(self):
        logs = self.drv.get_log("browser")
        pprint(logs)

    def save_screenshot(self, filename: str='screenshot'):
        """Saves the current browser screen to a PNG file in screenshots directory.

        :param filename: The file name of the PNG file.

        """

        os.makedirs(self.screenshot_dir, exist_ok=True)
        if not self.drv.save_screenshot(f'{self.screenshot_dir}/{filename}.png'):
            raise Exception('Screenshot failed')

    def save_element_screenshot(self, element: WebElement, filename_or_file: Union[str, BytesIO, None]=None,
                                move_to_element: bool = False) -> Image:
        """Saves the screenshot of an element to a PNG file.

        :return: The image object.
        :param element: The element to save.
        :param filename_or_file: Filename for the image without extension, a file object or None. If None, the image
         exists only in memory.
        :param move_to_element: Whether to move to the element before taking the screenshot. Use this if there is a
         possibility that the element is not in viewport.
        """
        if move_to_element:
            ActionChains(self.drv).move_to_element(element).perform()
        src_base64 = self.drv.get_screenshot_as_base64()
        im = Image(blob=b64decode(src_base64))

        x = element.location["x"]
        y = element.location["y"]
        w = element.size["width"]
        h = element.size["height"]
        offset = int(self.drv.execute_script('return window.pageYOffset;'))
        y -= offset

        im.crop(
            left=math.floor(x),
            top=math.floor(y),
            width=math.ceil(w),
            height=math.ceil(h),
        )
        if isinstance(filename_or_file, str):
            os.makedirs(os.path.dirname(os.path.join(self.screenshot_dir, filename_or_file)), exist_ok=True)
            im.save(filename=f'{self.screenshot_dir}/{filename_or_file}.png')
        elif isinstance(filename_or_file, BytesIO):
            im.save(file=filename_or_file)
        return im

    def assert_same_screenshot(self, element: WebElement, filename: str, move_to_element: bool = False, try_again=True):
        """Asserts that the provided element looks the same as in the provided screenshot.
        :param try_again: Whether to try again it the first comparison attempt fails.
        :param element: The element to check.
        :param filename: The filename of the expected screenshot.
        :param move_to_element: Whether to move to the element before taking the screenshot.
        """
        im = self.save_element_screenshot(element, move_to_element=move_to_element)
        try:
            ref = Image(filename=f'tests/browser/expected_screenshots/{filename}.png')
        except BaseError:
            print(f'Expected screenshot not found, saving image to {filename}.png')
            im.save(filename=f'{self.screenshot_dir}/{filename}.png')
            return
        diff, result = im.compare(ref, metric='peak_signal_to_noise_ratio')
        if result > self.get_screenshot_tolerance():
            if try_again:
                self.assert_same_screenshot(element, filename, move_to_element, try_again=False)
            else:
                self.save_element_screenshot(element, f'{filename}_FAIL', move_to_element)
                diff.save(filename=f'{self.screenshot_dir}/{filename}_FAIL_DIFF.png')
                self.assertTrue(False,
                                msg=f'Screenshots did not match (diff value is {result}); '
                                    f'failed screenshot saved to screenshots/{filename}_FAIL '
                                    f'and difference to screenshots/{filename}_FAIL_DIFF')

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
            self.assertTrue(False, f'Selector "{css_selector}" matched something.')
        finally:
            self.drv.implicitly_wait(10)

    def tearDown(self):
        TimLiveServer.tearDown(self)
        self.drv.quit()

    def goto_document(self, d: DocInfo, view='view'):
        self.goto(f'/{view}/{d.path}')

    def wait_until_hidden(self, selector):
        self.drv.implicitly_wait(0.1)
        self.wait.until(ec.invisibility_of_element_located((By.CSS_SELECTOR, selector)))
        self.drv.implicitly_wait(10)

    def wait_until_present(self, selector):
        self.wait.until(ec.presence_of_element_located((By.CSS_SELECTOR, selector)))
        self.wait.until(ec.visibility_of_element_located((By.CSS_SELECTOR, selector)))

    def wait_until_text_present(self, selector: str, text: str):
        self.wait.until(ec.text_to_be_present_in_element((By.CSS_SELECTOR, selector), text))

    def select_text(self, selector: str, start_offset: int, end_offset: int):
        self.drv.execute_script(f"""
        var range = document.createRange();
        var element = document.querySelector('{selector}').childNodes[0];
        range.setStart(element, {start_offset});
        range.setEnd(element, {end_offset});
        window.getSelection().addRange(range);
        """)

    def find_element_and_move_to(self, selector: str, times=1) -> WebElement:
        e = None
        for i in range(0, times):
            e = self.drv.find_element_by_css_selector(selector)
            ActionChains(self.drv).move_to_element(e).perform()
        return e

    def find_element(self, selector: str) -> WebElement:
        return self.drv.find_element_by_css_selector(selector)

    def touch(self, e: WebElement):
        ActionChains(self.drv).move_to_element(e).perform()

    def wait_and_click(self, selector: str):
        self.wait.until(ec.element_to_be_clickable((By.CSS_SELECTOR, selector)))
        self.drv.find_element_by_css_selector(selector).click()


def find_button_by_text(root: WebElement, text: str):
    return find_element_by_text(root, text, 'button')


def find_element_by_text(root: WebElement, text: str, element: str='*') -> WebElement:
    return root.find_element_by_xpath(f"//{element}[contains(text(),'{text}')]")


def find_by_ngmodel(element: WebElement, model: str, tagname='*') -> WebElement:
    return element.find_element_by_css_selector(f'{tagname}[ng-model="{model}"]')


def find_by_ngclick(element: WebElement, value: str, tagname='*') -> WebElement:
    return element.find_element_by_css_selector(f'{tagname}[ng-click="{value}"]')


def find_all_by_ngmodel(element: WebElement, model: str, tagname='*') -> List[WebElement]:
    return element.find_elements_by_css_selector(f'{tagname}[ng-model="{model}"]')
