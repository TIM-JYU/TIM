from time import sleep

from selenium.webdriver.support import expected_conditions as ec
from selenium.webdriver.common.by import By

from timApp.folder.folder import Folder
from timApp.messaging.messagelist.listinfo import ArchiveType
from timApp.messaging.messagelist.messagelist_utils import (
    MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX,
    MESSAGE_LIST_DOC_PREFIX,
)
from timApp.tests.browser.browsertest import BrowserTest
from timApp.tests.server.timroutetest import TimMessageListTest
from timApp.timdb.sqa import db
from timApp.util.utils import static_tim_doc


class MessageListBrowserTest(BrowserTest, TimMessageListTest):
    def test_admin_page_and_archive(self):
        """The message list admin component and the archived message components work in the browser."""
        self.login_browser_quick_test1()
        self.login_test1()
        self.make_admin(self.test_user_1)
        # The preamble of the message list documents shows the admin component.
        self.create_doc(
            f"{MESSAGE_LIST_DOC_PREFIX}/templates/preambles/preamble",
            from_file=static_tim_doc("initial/messagelist_preamble.md"),
        )
        list_name = "browser_list_test1"
        _, message_list = self.create_list(list_name, ArchiveType.PUBLIC)
        self.add_list_member(list_name, [self.test_user_2.name])
        db.session.commit()

        # The management page shows the options of the list.
        self.goto(f"/view/{MESSAGE_LIST_DOC_PREFIX}/{list_name}")
        self.wait_until_present_and_vis("tim-message-list-admin #list-name")
        self.assertEqual(
            list_name, self.find_element("#list-name").get_attribute("value")
        )
        self.wait.until(
            ec.text_to_be_present_in_element_value(
                (By.CSS_SELECTOR, "#list-email-address"),
                message_list.email_address,
            )
        )
        self.wait.until(
            ec.text_to_be_present_in_element(
                (By.CSS_SELECTOR, "#current-members-tab"), self.test_user_2.email
            )
        )

        # Changing an option and saving it stores it on the server:
        # the new value is shown when the page is loaded again.
        description = self.find_element("#list-description")
        description.send_keys("Browser test description")
        self.find_element_by_text("Save options", "button").click()
        sleep(1)
        self.goto(f"/view/{MESSAGE_LIST_DOC_PREFIX}/{list_name}")
        self.wait_until_present_and_vis("tim-message-list-admin #list-description")
        self.wait.until(
            ec.text_to_be_present_in_element_value(
                (By.CSS_SELECTOR, "#list-description"), "Browser test description"
            )
        )

        # An archived message shows its header and footer.
        # Leave the admin page first so that it does not make requests while the message is processed.
        self.goto("/empty")
        self.trigger_message_send(
            message_list, self.test_user_1, "Browser test message", "Message body"
        )
        folder = Folder.find_by_path(
            f"{MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX}/{message_list.name}"
        )
        doc = next(
            d
            for d in folder.get_all_documents()
            if d.short_name.startswith("browser-test-message")
        )
        self.goto_document(doc)
        self.wait_until_present_and_vis("tim-archive-header")
        self.wait.until(
            ec.text_to_be_present_in_element(
                (By.CSS_SELECTOR, "tim-archive-header"), "Browser test message"
            )
        )
        self.assertIn(
            self.test_user_1.email, self.find_element("tim-archive-header").text
        )
        self.wait_until_present("tim-archive-footer")
