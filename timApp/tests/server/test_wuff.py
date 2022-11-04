from time import sleep

from timApp.notification.send_email import sent_mails_in_testing
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db


class TestWuff(TimRouteTest):
    def setUp(self):
        super().setUp()
        self.test_user_1.make_admin()
        db.session.commit()
        sent_mails_in_testing.clear()
        self.login_test1()

    def tigger_error(self, count: int, expect_mail_count: int):
        for i in range(count):
            self.get("/exception", expect_status=500)
        self.assertEqual(expect_mail_count, len(sent_mails_in_testing))

    def test_wuff_send(self):
        with self.temp_config(
            {
                "TRAP_HTTP_EXCEPTIONS": False,
                "PROPAGATE_EXCEPTIONS": False,
            }
        ):
            self.tigger_error(count=1, expect_mail_count=1)
            m = sent_mails_in_testing[0]
            self.assertEqual(m["rcpt"], app.config["ERROR_EMAIL"])
            self.assertEqual(m["mail_from"], app.config["WUFF_EMAIL"])

    def test_wuff_send_mute_interval(self):
        with self.temp_config(
            {
                "TRAP_HTTP_EXCEPTIONS": False,
                "PROPAGATE_EXCEPTIONS": False,
                "WUFF_MAX_SAME_COUNT": 2,
                "WUFF_MAX_SAME_INTERVAL": 1,
                "WUFF_MAX_SAME_MUTE_DURATION": 5,
            }
        ):
            for i in range(2):
                # Sleeping beyond the WUFF_MAX_SAME_INTERVAL should not prevent the next mail
                sleep(1)
                self.tigger_error(count=1, expect_mail_count=i + 1)

    def test_wuff_send_mute_cooldown(self):
        with self.temp_config(
            {
                "TRAP_HTTP_EXCEPTIONS": False,
                "PROPAGATE_EXCEPTIONS": False,
                "WUFF_MAX_SAME_COUNT": 2,
                "WUFF_MAX_SAME_INTERVAL": 10,
                "WUFF_MAX_SAME_MUTE_DURATION": 1,
            }
        ):
            self.tigger_error(count=2, expect_mail_count=2)

            # This should not trigger a mail
            self.tigger_error(count=1, expect_mail_count=2)

            # Waiting for WUFF_MAX_SAME_MUTE_DURATION should reset the mute cooldown
            sleep(1)
            self.tigger_error(count=2, expect_mail_count=4)
