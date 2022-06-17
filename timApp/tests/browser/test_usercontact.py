from selenium.webdriver.common.by import By

from timApp.tests.browser.browsertest import BrowserTest


class UserContactTest(BrowserTest):
    def get_screenshot_tolerance(self):
        return 8000

    def test_email_add(self):
        def screenshot(name: str | list[str]):
            self.wait_until_present(
                "bootstrap-form-panel[title='Your account information']"
            )
            el = self.drv.find_element(
                By.CSS_SELECTOR,
                "bootstrap-form-panel[title='Your account information']",
            )
            self.assert_same_screenshot(el, name, move_to_element=True)

        self.login_browser_quick_test1()
        self.goto("/settings")
        screenshot("usercontact/add_new_initial")
        self.drv.find_element(
            By.CSS_SELECTOR, "settings-button-panel > .timButton:nth-child(2)"
        ).click()
        self.drv.find_element(By.NAME, "channel-select").click()
        dropdown = self.drv.find_element(By.NAME, "channel-select")
        dropdown.find_element(By.XPATH, "//option[. = 'Email']").click()
        self.drv.find_element(By.CSS_SELECTOR, ".ng-pristine > option").click()
        self.drv.find_element(By.NAME, "contact-info-text").click()
        self.drv.find_element(By.NAME, "contact-info-text").send_keys(
            "test1other@example.com"
        )
        self.drv.find_element(
            By.CSS_SELECTOR, ".modal-footer > .timButton:nth-child(1)"
        ).click()
        self.drv.find_element(By.CSS_SELECTOR, ".timButton:nth-child(2)").click()

        screenshot(
            ["usercontact/add_new_unverified", "usercontact/add_new_unverified_alt"]
        )

        emails = self.sent_emails
        last_email_msg = emails[-1]
        verify_link = last_email_msg["msg"].removeprefix("http://localhost")

        self.goto(verify_link)
        self.drv.find_element(By.CSS_SELECTOR, ".timButton").click()

        self.goto("/settings")
        screenshot("usercontact/add_new_verified")
