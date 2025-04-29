import time
from selenium.webdriver.common.by import By
from selenium.webdriver import ActionChains
from selenium.common.exceptions import (
    NoSuchElementException,
    ElementClickInterceptedException,
    ElementNotInteractableException,
)
from selenium.webdriver.remote.webelement import WebElement
from timApp.tests.browser.browsertest import BrowserTest


class TestBadges(BrowserTest):
    def setUp(self):
        super().setUp()
        # Log in as teacher
        self.login_browser_quick_test1()
        self.login_test1()
        # Embed the badge components in a new document
        d = self.create_doc(
            initial_par="""
#- {allowangular=true}
<tim-badge-creator badgegroup-context="newgroup1"></tim-badge-creator>

"""
        )
        self.goto_document(d)

    def tearDown(self):
        super().tearDown()

    def _create_default_badge(self, title: str):
        self.open_badge_form()
        self.fill_badge_form(title=title, description=title)
        # Submit the form
        self.submit_badge_form()
        # Wait for it in the list
        self.wait_until_present_and_vis("div.badge-card tim-badge")

    def test_badge_creation_valid(self):
        """Test that a valid badge is created successfully."""
        self._create_default_badge("Test Badge")
        badge = self.find_element("div.badge-card tim-badge")
        self.assertIn("Test Badge", badge.get_attribute("title"))

    def test_badge_creation_missing_required(self):
        """Test that required-field validation works."""
        self.wait_until_present_and_vis("#button showBadgeForm")
        self.find_element("#button showBadgeForm").click()
        # Only description
        self.wait_until_present_and_vis("#textarea description")
        self.find_element("#textarea description").send_keys("Only desc")
        # Force validation
        t = self.find_element("#input title")
        t.click()
        t.send_keys(" ")
        t.clear()
        self.wait_until_present_and_vis("div.error-message")
        self.assertIn("Title is required", self.find_element("div.error-message").text)
        self.assertFalse(self.find_element("#button createButton").is_enabled())

    def test_cancel_badge_creation(self):
        """Test that cancelling does not create a badge."""
        self.wait_until_present_and_vis("#button showBadgeForm")
        self.find_element("#button showBadgeForm").click()
        self.wait_until_present_and_vis("#input title")
        self.find_element("#input title").send_keys("Will Cancel")
        self.find_element("#textarea description").send_keys("Cancel me")
        self.find_element("#button cancelButton").click()
        self.wait_until_hidden("#form badgeForm")

        cards = self.drv.find_elements(By.CSS_SELECTOR, "div.badge-card")
        for c in cards:
            try:
                title = c.find_element(By.CSS_SELECTOR, "tim-badge").get_attribute(
                    "title"
                )
                self.assertNotIn("Will Cancel", title)
            except NoSuchElementException:
                continue

    def test_badge_editing(self):
        """Test editing an existing badge."""
        self._create_default_badge("Editable")
        # Select
        elm = self.find_element("div.badge-card tim-badge")
        ActionChains(self.drv).move_to_element(elm).click().perform()
        # Edit
        self.wait_until_present_and_vis("#button editButton")
        self.find_element("#button editButton").click()
        self.wait_until_present_and_vis("#input title")
        f = self.find_element("#input title")
        f.clear()
        f.send_keys("Edited")
        self.find_element("#button createButton").click()
        # Verify
        upd = self.find_element("div.badge-card tim-badge").get_attribute("title")
        self.assertIn("Edited", upd)

    def test_alert_dismissal(self):
        """Test that validation alerts can be closed."""
        self.wait_until_present_and_vis("#button showBadgeForm")
        self.find_element("#button showBadgeForm").click()
        # Trigger alert
        t = self.find_element("#input title")
        t.click()
        t.send_keys(" ")
        t.clear()
        self.wait_until_present_and_vis("tim-alert")
        alert = self.find_element("tim-alert")
        self.assertTrue(alert.text)
        # Dismiss
        try:
            alert.find_element(By.CSS_SELECTOR, ".close").click()
        except (ElementClickInterceptedException, ElementNotInteractableException):
            pass
        self.should_not_exist("tim-alert")

    def test_give_badge(self):
        """Test the badge assignment interface."""
        self._create_default_badge("Assignable")
        # Select
        elm = self.find_element("div.badge-card tim-badge")
        ActionChains(self.drv).move_to_element(elm).click().perform()
        # Assign
        self.wait_until_present_and_vis("button[title='Assign Badge']")
        self.find_element("button[title='Assign Badge']").click()
        self.wait_until_present_and_vis("timBadgeGiver")
        # Cancel
        self.find_element("timBadgeGiver button[title='Cancel']").click()
        self.wait_until_hidden("timBadgeGiver")

    def test_withdraw_badge(self):
        """Test the badge withdrawal interface."""
        self._create_default_badge("Withdrawable")
        # Select
        elm = self.find_element("div.badge-card tim-badge")
        ActionChains(self.drv).move_to_element(elm).click().perform()
        # Withdraw
        self.wait_until_present_and_vis("button[title='Withdraw Badge']")
        self.find_element("button[title='Withdraw Badge']").click()
        self.wait_until_present_and_vis("tim-badge-selected-withdraw")
        # Cancel
        self.find_element("tim-badge-selected-withdraw button[title='Cancel']").click()
        self.wait_until_hidden("tim-badge-selected-withdraw")

    def find_elements_by_tag_name(self, tagname: str, parent: WebElement | None = None):
        """Helper method to find elements by tag name."""
        root = parent if parent is not None else self.drv
        return root.find_elements(By.TAG_NAME, tagname)

    def open_badge_form(self):
        """Opens the badge creation form by clicking the showBadgeForm button."""
        self.wait_until_present_and_vis("#button showBadgeForm")
        open_button = self.find_element_and_move_to("#button showBadgeForm")
        open_button.click()
        # Wait for the form to appear by checking the title input.
        self.wait_until_present_and_vis("#input title")

    def fill_badge_form(
        self,
        title: str,
        description: str,
        image_index: int = 0,
        color_index: int = 0,
        shape: str = "hexagon",
    ):
        """Fills in the badge form using provided data."""
        # Fill in the title.
        title_input = self.find_element("#input title")
        title_input.clear()
        title_input.send_keys(title)

        # Fill in the description.
        desc_input = self.find_element("#textarea description")
        desc_input.clear()
        desc_input.send_keys(description)

        # Choose an image from the select.
        image_select = self.find_element("#select image")
        image_options = self.find_elements_by_tag_name("option", parent=image_select)
        if len(image_options) > image_index:
            image_options[image_index].click()

        # Choose a color from the select.
        color_select = self.find_element("#select color")
        color_options = self.find_elements_by_tag_name("option", parent=color_select)
        if len(color_options) > color_index:
            color_options[color_index].click()

        # Choose the shape radio button.
        shape_radio = self.find_element(f"input[type='radio'][value='{shape}']")
        shape_radio.click()

        # Wait for the badge preview to update.
        self.wait_until_present(".preview tim-badge")

    def submit_badge_form(self):
        """Submits the badge form by clicking the create/save button."""
        create_button = self.find_element("#createButton")
        self.assertTrue(
            create_button.is_enabled(),
            "Submit button should be enabled for a valid form",
        )
        create_button.click()
