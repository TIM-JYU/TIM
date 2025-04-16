from timApp.tests.browser.browsertest import BrowserTest
from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webelement import WebElement
from selenium.common.exceptions import (
    ElementClickInterceptedException,
    ElementNotInteractableException,
    NoSuchElementException,
)


class TestBadges(BrowserTest):
    def setUp(self):
        super().setUp()
        # Log in quickly as Test User 1.
        self.login_browser_quick_test1()

        # Create a document that includes our badge components.
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
        <tim-badge-creator badgegroup-context="newgroup1"></tim-badge-creator>
        <tim-badge-viewer badgegroup-context="newgroup1" badgeuser-context="%%username%%"></tim-badge-viewer>
        """
        )
        tr_par.save()
        self.goto_document(d)

    def tearDown(self):
        super().tearDown()

    # --- Helper Methods ---
    def find_elements_by_tag_name(self, tagname: str, parent: WebElement | None = None):
        """Helper method to find elements by tag name."""
        root = parent if parent is not None else self.drv
        return root.find_elements(By.TAG_NAME, tagname)

    def open_badge_form(self):
        """Opens the badge creation form by clicking the showBadgeForm button."""
        self.wait_until_present_and_vis("button#showBadgeForm")
        open_button = self.find_element_and_move_to("button#showBadgeForm")
        open_button.click()
        # Wait for the form to appear by checking the title input.
        self.wait_until_present_and_vis("input#title")

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
        title_input = self.find_element("input#title")
        title_input.clear()
        title_input.send_keys(title)

        # Fill in the description.
        desc_input = self.find_element("textarea#description")
        desc_input.clear()
        desc_input.send_keys(description)

        # Choose an image from the select.
        image_select = self.find_element("select#image")
        image_options = self.find_elements_by_tag_name("option", parent=image_select)
        if len(image_options) > image_index:
            image_options[image_index].click()

        # Choose a color from the select.
        color_select = self.find_element("select#color")
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

    def get_latest_badge_card(self):
        """Waits for and returns a badge card element.
        Since the badge title is contained in the tim-badge element inside the card,
        we retrieve the first badge card and then its contained tim-badge's title attribute.
        """
        self.wait_until_present("div.badge-card")
        badge_card = self.find_element("div.badge-card")
        # Optionally, retrieve child tim-badge if needed:
        badge_component = badge_card.find_element(By.CSS_SELECTOR, "tim-badge")
        return badge_component

    def dismiss_alerts(self):
        """Dismiss any visible alerts by clicking their close buttons.
        The alert uses a tim-alert component with a close button.
        """
        alert_close_buttons = self.drv.find_elements(
            By.CSS_SELECTOR, "tim-alert .close"
        )
        for btn in alert_close_buttons:
            try:
                btn.click()
            except (
                ElementClickInterceptedException,
                ElementNotInteractableException,
            ) as _:
                # Optionally log the error or take alternative action
                continue

    # --- Test Methods ---
    def test_badge_creation_valid(self):
        """Test that a valid badge is created successfully."""
        self.open_badge_form()
        self.fill_badge_form(
            title="Test Badge", description="This is a valid test badge description."
        )
        self.submit_badge_form()

        # Verify that the newly created badge appears.
        badge_component = self.get_latest_badge_card()
        badge_title = badge_component.get_attribute("title") or badge_component.text
        self.assertIn(
            "Test Badge",
            badge_title,
            "The badge should display the created badge with the title 'Test Badge'.",
        )

    def test_badge_creation_missing_required(self):
        """Test that form validation prevents submitting the badge form without required data."""
        self.open_badge_form()
        # Leave title empty to trigger required-field validation.
        self.fill_badge_form(
            title="", description="Badge description provided but title is missing."
        )

        # Trigger blur to force validation.
        title_input = self.find_element("input#title")
        title_input.click()
        title_input.send_keys(" ")
        title_input.clear()

        # Wait for a visible error message.
        self.wait_until_present("div.error-message")
        error_text = self.find_element("div.error-message").text
        self.assertIn(
            "Title is required",
            error_text,
            "Error message should indicate that title is required.",
        )

        # Verify that the submit button is disabled.
        create_button = self.find_element("#createButton")
        self.assertFalse(
            create_button.is_enabled(),
            "Submit button should be disabled when required fields are missing.",
        )

    def test_cancel_badge_creation(self):
        """Test that cancelling badge creation does not add the badge."""
        self.open_badge_form()
        self.fill_badge_form(
            title="Unwanted Badge", description="This badge creation will be cancelled."
        )
        cancel_button = self.find_element("#cancelButton")
        cancel_button.click()
        # Wait for the form (id="badgeForm") to disappear.
        self.wait_until_hidden("form#badgeForm")

        # Verify that no badge card contains the title "Unwanted Badge".
        badge_cards = self.drv.find_elements(By.CSS_SELECTOR, "div.badge-card")
        for card in badge_cards:
            try:
                # Look inside for the tim-badge element that has a title attribute.
                badge_component = card.find_element(By.CSS_SELECTOR, "tim-badge")
                card_title = (
                    badge_component.get_attribute("title") or badge_component.text
                )
                self.assertNotIn(
                    "Unwanted Badge",
                    card_title,
                    "A cancelled badge creation should not appear in the badge list.",
                )
            except NoSuchElementException:
                # If the tim-badge element isn't found, skip this card.
                continue

    def test_badge_editing(self):
        """Test that an existing badge can be edited."""
        # Create a badge to be edited.
        self.open_badge_form()
        self.fill_badge_form(
            title="Badge To Edit", description="Initial badge description."
        )
        self.submit_badge_form()

        # Select the badge by clicking its tim-badge element.
        badge_component = self.get_latest_badge_card()
        ActionChains(self.drv).move_to_element(badge_component).click().perform()

        # Click the Edit Badge button.
        self.wait_until_present_and_vis("button#editButton")
        edit_button = self.find_element("button#editButton")
        edit_button.click()

        # Wait until the form loads with current badge data.
        self.wait_until_present_and_vis("input#title")

        # Update the badge details.
        self.fill_badge_form(
            title="Edited Badge Title", description="Updated badge description."
        )
        self.submit_badge_form()

        # Verify that the badge card now reflects the updated badge title.
        updated_component = self.get_latest_badge_card()
        updated_title = (
            updated_component.get_attribute("title") or updated_component.text
        )
        self.assertIn(
            "Edited Badge Title",
            updated_title,
            "The badge list should display the updated badge title after editing.",
        )

    def test_alert_dismissal(self):
        """Test that alerts can be dismissed by the user."""
        # Trigger a validation error by leaving the title blank.
        self.open_badge_form()
        self.fill_badge_form(title="", description="Missing title to trigger alert.")
        title_input = self.find_element("input#title")
        title_input.click()
        title_input.send_keys(" ")
        title_input.clear()

        # Wait for the error alert to appear (using the tim-alert component).
        self.wait_until_present("tim-alert")
        alert_elem = self.find_element("tim-alert")
        alert_text = alert_elem.text
        self.assertTrue(alert_text, "An alert message should be displayed.")

        # Dismiss alerts.
        self.dismiss_alerts()
        # Verify that no tim-alert element is present.
        self.should_not_exist("tim-alert")

    def test_give_badge(self):
        """Test the badge assignment interface.

        This simulates selecting a badge and clicking the 'Assign Badge' button,
        then verifies that the badge giver component appears.
        """
        # Create a badge for assignment.
        self.open_badge_form()
        self.fill_badge_form(
            title="Badge For Assignment", description="Badge to be assigned."
        )
        self.submit_badge_form()

        # Select the created badge.
        badge_component = self.get_latest_badge_card()
        ActionChains(self.drv).move_to_element(badge_component).click().perform()

        # Click the 'Assign Badge' button using its title attribute.
        self.wait_until_present_and_vis("button[title='Assign Badge']")
        assign_button = self.find_element("button[title='Assign Badge']")
        assign_button.click()

        # Wait for the badge giver component to appear.
        self.wait_until_present("timBadgeGiver")
        giver_component = self.find_element("timBadgeGiver")
        self.assertIsNotNone(
            giver_component,
            "Badge giver component should be visible after clicking assign.",
        )

    def test_withdraw_badge(self):
        """Test the badge withdrawal interface.

        This simulates selecting a badge and clicking the 'Withdraw Badge' button,
        then verifies that the withdrawal component appears.
        """
        # Create a badge for withdrawal.
        self.open_badge_form()
        self.fill_badge_form(
            title="Badge For Withdrawal", description="Badge to be withdrawn."
        )
        self.submit_badge_form()

        # Select the created badge.
        badge_component = self.get_latest_badge_card()
        ActionChains(self.drv).move_to_element(badge_component).click().perform()

        # Click the 'Withdraw Badge' button using its title attribute.
        self.wait_until_present_and_vis("button[title='Withdraw Badge']")
        withdraw_button = self.find_element("button[title='Withdraw Badge']")
        withdraw_button.click()

        # Wait for the withdrawal component to appear.
        self.wait_until_present("tim-badge-selected-withdraw")
        withdraw_component = self.find_element("tim-badge-selected-withdraw")
        self.assertIsNotNone(
            withdraw_component,
            "Badge withdrawal component should be visible after clicking withdraw.",
        )
