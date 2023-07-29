from sqlalchemy import select

from timApp.messaging.messagelist.listinfo import Channel
from timApp.notification.send_email import sent_mails_in_testing
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db, run_sql
from timApp.user.usercontact import UserContact
from timApp.user.verification.verification import ContactAddVerification


class VerificationTest(TimRouteTest):
    """Test user verification"""

    def test_add_email_contacts(self):
        """Test adding email"""
        u = self.test_user_1
        self.login_test1()

        def add_email(email: str) -> tuple[dict, str]:
            prev = len(sent_mails_in_testing)
            self.post(
                "/contacts/add",
                data={
                    "contact_info": email,
                    "contact_info_type": "email",
                },
                expect_status=200,
            )

            self.assertIsNotNone(u.get_contact(Channel.EMAIL, email))

            verification = (
                run_sql(
                    select(ContactAddVerification)
                    .join(UserContact)
                    .filter(
                        (ContactAddVerification.user == u)
                        & (UserContact.contact == email)
                    )
                )
                .scalars()
                .one()
            )

            self.assertEqual(
                len(sent_mails_in_testing),
                prev + 1,
                "Verification email should have been sent",
            )

            return sent_mails_in_testing[-1], f"/verify/contact/{verification.token}"

        self.post(
            "/contacts/add",
            data={
                "contact_info": u.email,
                "contact_info_type": "email",
            },
            expect_status=400,
        )

        mail, verify_route = add_email("test1alt@example.com")

        # Send verification to new email since we need to check its existence
        self.assertEqual(mail["rcpt"], "test1alt@example.com")
        self.assertEqual(mail["subject"], "Verify contact")
        self.assertEqual(mail["msg"], f"{app.config['TIM_HOST']}{verify_route}")

        self.login_test2()
        self.post(
            verify_route,
            data={"verify": True},
            expect_status=400,
            expect_content={"error": "You are not authorized to verify this action"},
        )

        self.login_test1()
        self.post(verify_route, data={"verify": True}, expect_status=200)
        self.post(
            verify_route,
            data={"verify": True},
            expect_status=400,
            expect_content={"error": "This verification request is no longer valid"},
        )

        c = u.get_contact(Channel.EMAIL, "test1alt@example.com")
        self.assertTrue(c.verified)

        mail, verify_route = add_email("test1altother@example.com")
        self.post(verify_route, data={"verify": False}, expect_status=200)
        # Verification should be deleted
        self.post(
            verify_route,
            data={"verify": True},
            expect_status=400,
            expect_content={"error": "No verification found for the token and type"},
        )

        c = u.get_contact(Channel.EMAIL, "test1altother@example.com")
        self.assertIsNone(c)

    def test_custom_verify_template(self):
        """Test custom verify templates"""
        u = self.test_user_1
        self.make_admin(u)
        db.session.refresh(self.test_user_1)
        self.login_test1()

        self.create_doc(
            "settings/verify-templates/contact",
            initial_par="""
This is custom contact template!

Variables:
TIM_HOST = {{ config['TIM_HOST'] }}
user.name = {{ user.name }}
verification.type = {{ verification.type }}
verify_url = {{ verify_url }}
""",
            settings={"subject": "Custom verify contact"},
        )

        self.post(
            "/contacts/add",
            data={
                "contact_info": "someotheremail1@example.com",
                "contact_info_type": "email",
            },
            expect_status=200,
        )

        verification = (
            run_sql(
                select(ContactAddVerification)
                .join(UserContact)
                .filter(
                    (ContactAddVerification.user == self.test_user_1)
                    & (UserContact.contact == "someotheremail1@example.com")
                )
            )
            .scalars()
            .one()
        )

        last_mail = sent_mails_in_testing[-1]
        self.assertEqual(last_mail["subject"], "Custom verify contact")
        self.assertEqual(
            last_mail["msg"],
            f"""
This is custom contact template!

Variables:
TIM_HOST = http://localhost
user.name = {self.test_user_1.name}
verification.type = VerificationType.CONTACT_OWNERSHIP
verify_url = {app.config['TIM_HOST']}/verify/contact/{verification.token}
""".strip(),
        )
