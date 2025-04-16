from timApp.tests.browser.browsertest import BrowserTest
from timApp.static.scripts.tim.gamification.badge import *


class TestBadges(BrowserTest):
    def setUp(self):
        # Login steps common for all tests:
        self.login_browser_quick_test1()
        self.login_test1()

        # Create a document including the badge components.
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

    def open_badge_form(self):
        # Open the badge creation form by clicking the button.
        self.wait_until_present_and_vis("#showBadgeForm")
        open_creator = self.find_element_and_move_to("#showBadgeForm")
        open_creator.click()
        # Wait until the title input is visible to confirm the form is open.
        self.wait_until_present_and_vis("input#title")

    def fill_badge_form(
        self, title, description, image_index=0, color_index=0, shape="hexagon"
    ):
        # Enter text into title and description fields.
        title_input = self.find_element("input#title")
        title_input.clear()
        title_input.send_keys(title)

        description_input = self.find_element("textarea#description")
        description_input.clear()
        description_input.send_keys(description)

        # For select elements, choose an option by index.
        image_select = self.find_element("select#image")
        image_options = image_select.find_elements_by_tag_name("option")
        if len(image_options) > image_index:
            image_options[image_index].click()

        color_select = self.find_element("select#color")
        color_options = color_select.find_elements_by_tag_name("option")
        if len(color_options) > color_index:
            color_options[color_index].click()

        # For shape, select the radio button. Adjust selector if necessary.
        shape_radio = self.find_element(f"input[type='radio'][value='{shape}']")
        shape_radio.click()

        # Wait for the preview to update.
        self.wait_until_present(".preview tim-badge")

    def submit_badge_form(self):
        # Ensure the submit button is enabled before clicking.
        create_button = self.find_element("#createButton")
        self.assertTrue(
            create_button.is_enabled(),
            "Submit button should be enabled when form is valid.",
        )
        create_button.click()

    def get_latest_badge_card(self):
        # Wait for and return the first badge card element.
        self.wait_until_present(".badge-card")
        return self.find_element(".badge-card")

    def test_badge_creation_valid(self):
        """Test creating a valid badge."""
        self.open_badge_form()
        self.fill_badge_form(
            title="Test Badge", description="This is a test badge description."
        )
        self.submit_badge_form()

        # Verify that a badge card appears containing the badge title.
        created_badge = self.get_latest_badge_card()
        badge_title = created_badge.get_attribute("title") or created_badge.text
        self.assertIn(
            "Test Badge",
            badge_title,
            "Badge list should include the created badge titled 'Test Badge'.",
        )

    def test_badge_creation_missing_required(self):
        """Test form validation by attempting to submit without providing required information."""
        self.open_badge_form()

        # Fill only description leaving title blank.
        # NOTE: This assumes that the form marks the title as required.
        self.fill_badge_form(
            title="", description="Description provided but title is missing."
        )
        # Instead of submitting, check for visible error messages.
        # You might need to trigger blur events to make Angular show validation errors.
        title_input = self.find_element("input#title")
        title_input.click()
        title_input.send_keys(" ")
        title_input.clear()  # Trigger a change event

        # Check for an error message under the title field.
        self.wait_until_present(".error-message")
        error_msg = self.find_element(".error-message").text
        self.assertIn("Title is required", error_msg)

        # Confirm that the submit button is disabled.
        create_button = self.find_element("#createButton")
        self.assertFalse(
            create_button.is_enabled(),
            "Submit button should be disabled when required fields are missing.",
        )

    def test_cancel_badge_creation(self):
        """Test that cancelling badge creation does not create a badge."""
        self.open_badge_form()
        self.fill_badge_form(
            title="Unwanted Badge", description="Badge creation will be cancelled."
        )
        # Instead of submitting, click the cancel button.
        cancel_button = self.find_element("#cancelButton")
        cancel_button.click()

        # Wait a short moment for the form to close.
        self.wait_until_not_present("form#badgeForm", timeout=5)

        # Verify that no new badge card with the title "Unwanted Badge" exists.
        # Depending on how your app refreshes, you might need to search for all badge cards.
        badge_cards = self.find_elements(".badge-card")
        for card in badge_cards:
            card_title = card.get_attribute("title") or card.text
            self.assertNotIn(
                "Unwanted Badge",
                card_title,
                "Cancelled badge should not be present in the badge list.",
            )

    def test_badge_editing(self):
        """Test editing an existing badge.

        This example assumes that once a badge is created, it can be selected
        and then edited using an 'Edit Badge' button.
        """
        # Create an initial badge.
        self.open_badge_form()
        self.fill_badge_form(title="Badge To Edit", description="Initial description.")
        self.submit_badge_form()

        # Select the badge from the list
        self.wait_until_present(".badge-card")
        badge_card = self.get_latest_badge_card()
        badge_card.click()  # This should mark it as selected.

        # Click the Edit button
        self.wait_until_present_and_vis("#editButton")
        edit_button = self.find_element("#editButton")
        edit_button.click()

        # Wait until the editing form appears with existing badge data.
        self.wait_until_present_and_vis("input#title")

        # Update the title and description.
        self.fill_badge_form(
            title="Edited Badge Title", description="Edited description."
        )

        # Submit the changes.
        # This button might be the same as the 'create' button but now with different text.
        create_button = self.find_element("#createButton")
        self.assertTrue(
            create_button.is_enabled(),
            "Submit button should be enabled when editing a valid badge.",
        )
        create_button.click()

        # Verify that the badge card displays the updated title.
        self.wait_until_present(".badge-card")
        updated_badge = self.get_latest_badge_card()
        updated_title = updated_badge.get_attribute("title") or updated_badge.text
        self.assertIn(
            "Edited Badge Title",
            updated_title,
            "Badge list should display the updated badge title.",
        )
