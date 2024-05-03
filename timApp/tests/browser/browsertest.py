import math
import os
import warnings
from base64 import b64decode
from contextlib import contextmanager
from io import BytesIO
from pprint import pprint
from time import sleep
from typing import Any
from urllib.parse import urlencode

import requests
from flask.testing import FlaskClient
from flask_testing import LiveServerTestCase
from selenium import webdriver
from selenium.common.exceptions import (
    NoSuchElementException,
    StaleElementReferenceException,
    ScreenshotException,
    ElementNotInteractableException,
)
from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support import expected_conditions as ec
from selenium.webdriver.support.wait import WebDriverWait
from wand.exceptions import BaseError
from wand.image import Image

from timApp.document.docinfo import DocInfo
from timApp.tests.db.timdbtest import (
    TEST_USER_1_NAME,
    TEST_USER_2_NAME,
    TEST_USER_3_NAME,
)
from timApp.tests.server.timroutetest import TimRouteTestBase
from timApp.timdb.sqa import db
from timApp.user.user import Consent

PREV_ANSWER = "answerbrowser .prevAnswer"


options = webdriver.ChromeOptions()
# Uncomment to print console.log calls in print_console
# options.capabilities["goog:loggingPrefs"] = {"browser": "ALL"}
options.headless = True
options.add_argument("--window-size=1024x768")
# We run unit tests in CI environment, so we can generally skip sandboxing to achieve better stability
options.add_argument("--no-sandbox")
# This may slow down unit tests but generally is more stable when running browser tests in Docker
options.add_argument("--disable-dev-shm-usage")


class BrowserTest(LiveServerTestCase, TimRouteTestBase):
    login_dropdown_path = "//tim-login-menu/tim-user-menu/div/button"
    screenshot_dir = "/service/screenshots"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def get_screenshot_tolerance(self) -> float:
        return 5

    def create_app(self):
        from timApp.tim_app import app

        return app

    def _init_client(self) -> FlaskClient:
        return self.app.test_client()

    def setUp(self):
        TimRouteTestBase.setUp(self)
        self.drv = webdriver.Chrome(options=options)
        # Some CI browser tests run slower and can cause render timeouts without a longer script timeout
        self.drv.implicitly_wait(10)
        self.drv.set_page_load_timeout(60)
        self.drv.set_script_timeout(60)
        self.wait = WebDriverWait(self.drv, 30)

    def login_browser_as(self, email: str, password: str, name: str) -> None:
        """Logs in as a user with the given email and password and tests that the user is logged in.

        :param email: User email
        :param password: User password
        :param name: User's full name. Used to test that the user is logged in properly.
        """
        # self.client.__exit__(None, None, None)
        self.goto("")
        elem = self.drv.find_element(By.XPATH, "//tim-login-menu/button")
        elem.click()
        elem = self.find_element("tim-login-dialog")
        email_login_btn = elem.find_element(
            By.CSS_SELECTOR, ".login-flex-col button:first-child"
        )
        email_login_btn.click()
        elem.find_element(By.XPATH, "//input[@type='text']").send_keys(email)
        elem.find_element(By.XPATH, "//input[@type='password']").send_keys(password)
        login = elem.find_element(
            By.CSS_SELECTOR, ".flex > button:first-child"
        )  # Log in button
        login.click()
        self.wait.until(
            ec.text_to_be_present_in_element((By.XPATH, self.login_dropdown_path), name)
        )
        # self.client.__enter__()

    @contextmanager
    def temp_config(self, settings: dict[str, Any]):
        old_settings = {k: self.app.config[k] for k in settings.keys()}
        for k, v in settings.items():
            self.app.config[k] = v
        requests.post(f"{self.get_browser_url()}/testing/config", json=settings)
        try:
            yield
        finally:
            for k, v in old_settings.items():
                self.app.config[k] = v
            requests.post(f"{self.get_browser_url()}/testing/config", json=old_settings)

    @property
    def sent_emails(self):
        res = requests.get(f"{self.get_browser_url()}/testing/sentEmails")
        return res.json()

    def login_browser_quick_test1(self):
        """Logs testuser 1 in quickly by directly adding the session cookie to the browser."""
        self.goto("/empty")
        self.drv.delete_all_cookies()
        self.drv.add_cookie(
            {
                "expiry": 7544144177,
                "hCode": 1984987798,
                "httpOnly": True,
                "name": "session",
                "path": "/",
                "secure": False,
                "value": "eyJfcGVybWFuZW50Ijp0cnVlLCJhbmNob3IiOiIiLCJjYW1lX2Zyb20iOiIvIiwidXNlcl9pZCI6Mn0.DowETw.cyvyDZcvHWr2aKC5agfIW5sUVrU",
            }
        )

    def login_browser_quick_test2(self):
        """Logs testuser 2 in quickly by directly adding the session cookie to the browser."""
        self.goto("/empty")
        self.drv.delete_all_cookies()
        self.drv.add_cookie(
            {
                "expiry": 7648167488,
                "httpOnly": True,
                "name": "session",
                "path": "/",
                "secure": False,
                "value": ".eJwVy0EOhCAMQNG7dG0yahGFy5BaS2ZigEnFlfHu4vL95F8Q_qKJsuQKvuopHVDmb1HwAB0wJQlRS2r8vD40hlp2yS2YwTGamceerWXkaR0nIiTroiFaNtsPy-oitu88RMNvA4_3A1Q0I10.X2nYwA.BJgc6wYpvsB9yFWybREPHUKbCeU",
            }
        )

    def login_browser_test1(self):
        """Logs in as Test user 1."""
        self.drv.delete_all_cookies()
        self.login_browser_as("test1@example.com", "test1pass", TEST_USER_1_NAME)

    def login_browser_test2(self):
        """Logs in as Test user 2."""
        self.drv.delete_all_cookies()
        self.login_browser_as("test2@example.com", "test2pass", TEST_USER_2_NAME)

    def login_browser_test3(self):
        """Logs in as Test user 3."""
        self.drv.delete_all_cookies()
        self.login_browser_as("test3@example.com", "test3pass", TEST_USER_3_NAME)

    def get_browser_url(self):
        return self.app.config["SELENIUM_BROWSER_URL"]

    def goto(self, url: str):
        """Navigates to a new URL using the browser.

        :param url: The URL to which to navigate. This must be relative.

        """
        url_ = f"{self.get_browser_url()}{url}"
        # raise Exception(url_)
        self.drv.get(url_)

    def print_console(self):
        logs = self.drv.get_log("browser")
        pprint(logs)

    def refresh(self):
        """Refreshes the current browser page."""
        self.drv.refresh()

    def save_screenshot(self, filename: str = "screenshot"):
        """Saves the current browser screen to a PNG file in screenshots directory.

        :param filename: The file name of the PNG file.

        """

        os.makedirs(self.screenshot_dir, exist_ok=True)
        if not self.drv.save_screenshot(f"{self.screenshot_dir}/{filename}.png"):
            raise Exception("Screenshot failed")

    def save_element_screenshot(
        self,
        element: WebElement,
        filename_or_file: str | BytesIO | None = None,
        move_to_element: bool = False,
    ) -> Image:
        """Saves the screenshot of an element to a PNG file.

        :return: The image object.
        :param element: The element to save.
        :param filename_or_file: Filename for the image without extension, a file object or None. If None, the image
         exists only in memory.
        :param move_to_element: Whether to move to the element before taking the screenshot. Use this if there is a
         possibility that the element is not in viewport.
        """
        if move_to_element:
            # It seems like move_to_element is no longer enough (at least in some cases)
            # to get the element fully visible, so we have to use JS.
            self.scroll_into_view(element)
            # ActionChains(self.drv).move_to_element(element).perform()
        src_base64 = self.drv.get_screenshot_as_base64()
        im = Image(blob=b64decode(src_base64))

        x = element.location["x"]
        y = element.location["y"]
        w = element.size["width"]
        h = element.size["height"]
        if w == 0 or h == 0:
            raise ScreenshotException("Element width and height must not be 0")
        offset = int(self.drv.execute_script("return window.pageYOffset;"))
        y -= offset

        im.crop(
            left=math.floor(x),
            top=math.floor(y),
            width=math.ceil(w),
            height=math.ceil(h),
        )
        if isinstance(filename_or_file, str):
            self.save_im(im, filename_or_file)
        elif isinstance(filename_or_file, BytesIO):
            im.save(file=filename_or_file)
        return im

    def scroll_into_view(self, element: WebElement):
        self.drv.execute_script("arguments[0].scrollIntoView();", element)

    def save_im(self, im, filename_or_file):
        os.makedirs(
            os.path.dirname(os.path.join(self.screenshot_dir, filename_or_file)),
            exist_ok=True,
        )
        im.save(filename=f"{self.screenshot_dir}/{filename_or_file}.png")

    def assert_same_screenshot(
        self,
        element: WebElement,
        filename: str | list[str],
        move_to_element: bool = False,
        attempts=1,
    ):
        """Asserts that the provided element looks the same as in the provided screenshot.

        :param attempts: Number of comparison attempts.
        :param element: The element to check.
        :param filename: The filename of the expected screenshot.
        :param move_to_element: Whether to move to the element before taking the screenshot.
        """
        filenames = filename if isinstance(filename, list) else [filename]
        diff = None
        result = None
        fail_suffix = ""
        im = None
        f = None
        for i in range(attempts):
            if im:
                im.close()
            try:
                im = self.save_element_screenshot(
                    element, move_to_element=move_to_element
                )
            except ScreenshotException:
                continue
            for f in filenames:
                try:
                    ref = Image(filename=f"tests/browser/expected_screenshots/{f}.png")
                except BaseError:
                    print(f"Expected screenshot not found, saving image to {f}.png")
                    self.save_im(im, f)
                    im.close()
                    return
                im.fuzz = 300  # 250 is too low
                diff, result = im.compare(ref, metric="absolute")
                if result <= self.get_screenshot_tolerance():
                    im.close()
                    return
        if not f:
            raise Exception("Failed to get screenshot of element")
        self.save_im(im, f"{f}{fail_suffix}")
        im.close()
        self.save_im(diff, f"{f}{fail_suffix}_DIFF")
        diff.close()
        assert_msg = (
            f"Screenshots did not match (diff value is {result}); "
            f"failed screenshot saved to screenshots/{f}{fail_suffix} "
            f"and difference to screenshots/{f}{fail_suffix}_DIFF"
        )
        new_screenshots = os.environ.get("NEW_SCREENSHOTS")
        if new_screenshots != "1":
            self.assertTrue(False, msg=assert_msg)

    def should_not_exist(self, css_selector: str):
        """Asserts that the current document should not contain any elements that match the specified CSS selector.

        :param css_selector: The CSS selector to test.

        """
        self.drv.implicitly_wait(0.5)
        try:
            self.find_element(css_selector)
        except NoSuchElementException:
            pass
        else:
            self.assertTrue(False, f'Selector "{css_selector}" matched something.')
        finally:
            self.drv.implicitly_wait(10)

    def use_left_menu(self):
        # TODO: remove once tests have been updated to use the new edit menu format
        self.post("/settings/save/parmenupos/0")
        self.drv.implicitly_wait(0.5)
        self.drv.refresh()
        self.drv.implicitly_wait(5)

    def tearDown(self):
        scn_path = f"post_test/{self.id()}"
        try:
            os.makedirs(f"{self.screenshot_dir}/post_test", exist_ok=True)
            self.save_screenshot(scn_path)
        except Exception as e:
            warnings.warn(f"Failed to save screenshot to {scn_path}: {e}")
        TimRouteTestBase.tearDown(self)
        self.drv.quit()

    def goto_document(self, d: DocInfo, view="view", query=None):
        params = ""
        if query:
            params = "?" + urlencode(query)
        self.goto(f"/{view}/{d.path}{params}")

    def wait_until_hidden(self, selector):
        self.drv.implicitly_wait(0.1)
        self.wait.until(ec.invisibility_of_element_located((By.CSS_SELECTOR, selector)))
        self.drv.implicitly_wait(10)

    def wait_until_present_and_vis(self, selector):
        self.wait_until_present(selector)
        self.wait.until(ec.visibility_of_element_located((By.CSS_SELECTOR, selector)))

    def wait_until_present(self, selector):
        self.wait.until(ec.presence_of_element_located((By.CSS_SELECTOR, selector)))

    def wait_until_text_present(self, selector: str, text: str):
        self.wait.until(
            ec.text_to_be_present_in_element((By.CSS_SELECTOR, selector), text)
        )

    def wait_until_val_present(self, selector: str, text: str):
        self.wait.until(
            ec.text_to_be_present_in_element_value((By.CSS_SELECTOR, selector), text)
        )

    def select_text(self, selector: str, start_offset: int, end_offset: int):
        self.drv.execute_script(
            f"""
        var range = document.createRange();
        var element = document.querySelector('{selector}').childNodes[0];
        range.setStart(element, {start_offset});
        range.setEnd(element, {end_offset});
        window.getSelection().addRange(range);
        """
        )

    def find_element_and_move_to(
        self, selector: str, times=1, parent: WebElement | None = None
    ) -> WebElement:
        e = None
        for i in range(0, times):
            e = self.find_element(selector, parent=parent)
            ActionChains(self.drv).move_to_element(e).perform()
        return e

    def find_element(
        self,
        selector: str | None = None,
        xpath: str | None = None,
        parent: WebElement | None = None,
    ) -> WebElement:
        if selector and xpath:
            raise Exception("Only one of selector and xpath must be given")
        if not selector and not xpath:
            raise Exception("selector or xpath must be given")
        root = parent or self.drv
        if selector:
            return root.find_element(By.CSS_SELECTOR, selector)
        else:
            return root.find_element(By.XPATH, xpath)

    def find_element_avoid_staleness(
        self,
        selector: str | None = None,
        xpath: str | None = None,
        tries: int = 20,
        click=False,
        parent=None,
        poll_rate=0.5,
    ) -> WebElement:
        while True:
            try:
                e = self.find_element(selector=selector, xpath=xpath, parent=parent)
                if click:
                    e.click()
                else:
                    self.touch(e)
            except (
                StaleElementReferenceException,
                ElementNotInteractableException,
                NoSuchElementException,
            ):
                tries -= 1
                sleep(poll_rate)
                if tries == 0:
                    raise
                continue
            else:
                return e

    def find_element_by_text(
        self,
        text: str,
        element: str = "*",
        staleness_attempts=1,
        parent: WebElement | None = None,
    ) -> WebElement:
        node_scope = "." if parent else ""
        return self.find_element_avoid_staleness(
            xpath=f"{node_scope}//{element}[contains(text(),'{text}')]",
            parent=parent,
            tries=staleness_attempts,
        )

    def touch(self, e: WebElement):
        ActionChains(self.drv).move_to_element(e).perform()

    def wait_and_click(self, selector: str):
        self.wait.until(ec.element_to_be_clickable((By.CSS_SELECTOR, selector)))
        self.find_element(selector).click()

    def accept_consent(self):
        self.current_user.consent = Consent.CookieOnly
        db.session.commit()

    def login(
        self,
        email: str | None = None,
        passw: str | None = None,
        username: str | None = None,
        add: bool = False,
        force: bool = True,
        **kwargs,
    ):
        """Logs a user in.

        :param force: Unused parameter; must be defined so that it is not in kwargs.
        :param username: The username of the user.
        :param email: The email of the user.
        :param passw: The password of the user.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.

        """
        with self.client.session_transaction() as s:
            s.pop("last_doc", None)
            s.pop("came_from", None)
        return self.post(
            "/emailLogin",
            data={"email": email, "password": passw, "add_user": add},
            follow_redirects=True,
            **kwargs,
        )

    def get_uninteractable_element(self):
        return self.find_element("footer")

    def wait_for_editor_load(self):
        self.wait_until_present("pareditor")
        self.wait_until_hidden(".editor-loading")

    def set_network_state(self, online: bool):
        """
        Blocks or allows all network activity
        :param online: If false block everything, otherwise allow everything
        """
        self.drv.execute_cdp_cmd(
            "Network.setBlockedURLs", {"urls": [] if online else ["*"]}
        )
        self.drv.execute_cdp_cmd("Network.enable", {})
        pass


def find_button_by_text(root: WebElement, text: str):
    return find_element_by_text(root, text, "button")


def find_element_by_text(root: WebElement, text: str, element: str = "*") -> WebElement:
    return root.find_element(By.XPATH, f"//{element}[contains(text(),'{text}')]")


def find_by_attr_name(element: WebElement, model: str, tagname="*") -> WebElement:
    return element.find_element(By.CSS_SELECTOR, f'{tagname}[name="{model}"]')


def find_by_ngclick(element: WebElement, value: str, tagname="*") -> WebElement:
    return element.find_element(By.CSS_SELECTOR, f'{tagname}[ng-click="{value}"]')


def find_all_by_ngmodel(
    element: WebElement, model: str, tagname="*"
) -> list[WebElement]:
    return element.find_elements(By.CSS_SELECTOR, f'{tagname}[ng-model="{model}"]')
