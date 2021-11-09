from timApp.messaging.messagelist.listinfo import Channel
from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo, UserOrigin
from timApp.user.usercontact import ContactOrigin


class ContactsTest(TimDbTest):
    """Tests for adding and manipulating contacts"""

    def verify_contacts(
        self, user: User, channel: Channel, emails: list[tuple[ContactOrigin, str]]
    ):
        self.assertEqual(
            {
                (uc.contact_origin, uc.contact)
                for uc in user.contacts
                if uc.channel == channel
            },
            set(emails),
            "User's contacts must match",
        )

    def verify_primary_contact(
        self, user: User, channel: Channel, origin: ContactOrigin, contact: str
    ):
        primary = next(
            (uc for uc in user.contacts if uc.channel == channel and uc.primary), None
        )
        self.assertIsNotNone(
            primary, f"User must have a primary contact for channel {channel}"
        )
        self.assertEqual(primary.contact_origin, origin)
        self.assertEqual(primary.contact, contact)
        self.assertEqual(primary.contact, user.email)

    def test_new_user_emails(self):
        """Test that new users get contacts correctly created"""

        def test_contact(
            user: str, user_origin: UserOrigin, contact_origin: ContactOrigin
        ):
            email = f"{user}@example.com"
            u, _ = User.create_with_group(
                UserInfo(
                    username=user,
                    full_name=f"Some {user}",
                    email=email,
                    origin=user_origin,
                )
            )
            db.session.commit()
            db.session.refresh(u)

            self.verify_contacts(u, Channel.EMAIL, [(contact_origin, email)])
            self.verify_primary_contact(u, Channel.EMAIL, contact_origin, email)

        test_contact("someemailuser", UserOrigin.Email, ContactOrigin.Custom)
        test_contact("somehakauser", UserOrigin.Haka, ContactOrigin.Haka)
        test_contact("somesisuuser", UserOrigin.Sisu, ContactOrigin.Sisu)
        # Korppi origins get upgraded to Haka emails
        test_contact("somekorppiuser", UserOrigin.Korppi, ContactOrigin.Haka)
        # Old/unused origins are not managed and thus become custom
        test_contact("someotheruser", UserOrigin.Facebook, ContactOrigin.Custom)

        u, _ = User.create_with_group(
            UserInfo(
                username="noemailsuser",
                full_name="Some No Emails User",
            )
        )
        db.session.commit()
        db.session.refresh(u)

        self.assertEqual(u.contacts, [])
        self.assertIsNone(u.primary_email_contact)

    def test_email_contacts_set(self):
        """Test various common cases for adding and changing email contacts"""

        u, _ = User.create_with_group(
            UserInfo(
                username="someuser1",
                full_name="Some User",
                email="someuser1@example.com",
            )
        )
        db.session.commit()
        db.session.refresh(u)

        self.verify_contacts(
            u, Channel.EMAIL, [(ContactOrigin.Custom, "someuser1@example.com")]
        )

        # Adding new custom email
        # The primary email must not change after adding a new email
        u.set_emails(["someuser1alt@example.com"], ContactOrigin.Custom, remove=False)
        db.session.commit()
        db.session.refresh(u)
        self.verify_contacts(
            u,
            Channel.EMAIL,
            [
                (ContactOrigin.Custom, "someuser1@example.com"),
                (ContactOrigin.Custom, "someuser1alt@example.com"),
            ],
        )
        self.verify_primary_contact(
            u, Channel.EMAIL, ContactOrigin.Custom, "someuser1@example.com"
        )

        # Adding email via integration and allowing to update primary address
        # The primary email should be upgraded from custom because new integration was added
        # Previous custom email should become managed
        u.set_emails(
            ["someuser1work@example.com", "someuser1@example.com"],
            ContactOrigin.Sisu,
            can_update_primary=True,
        )
        db.session.commit()
        db.session.refresh(u)
        self.verify_primary_contact(
            u, Channel.EMAIL, ContactOrigin.Sisu, "someuser1work@example.com"
        )
        self.verify_contacts(
            u,
            Channel.EMAIL,
            [
                (ContactOrigin.Custom, "someuser1alt@example.com"),
                (ContactOrigin.Sisu, "someuser1@example.com"),
                (ContactOrigin.Sisu, "someuser1work@example.com"),
            ],
        )

        # Adding existing email via different integration
        # Primary email is the same but the email is now managed by different integration
        u.set_emails(
            ["someuser1work@example.com"], ContactOrigin.Haka, can_update_primary=True
        )
        db.session.commit()
        db.session.refresh(u)
        self.verify_primary_contact(
            u, Channel.EMAIL, ContactOrigin.Haka, "someuser1work@example.com"
        )
        self.verify_contacts(
            u,
            Channel.EMAIL,
            [
                (ContactOrigin.Custom, "someuser1alt@example.com"),
                (ContactOrigin.Sisu, "someuser1@example.com"),
                (ContactOrigin.Haka, "someuser1work@example.com"),
            ],
        )

        # Changing primary email to custom directly should work
        u.email = "someuser1alt@example.com"
        db.session.commit()
        db.session.refresh(u)
        self.verify_primary_contact(
            u, Channel.EMAIL, ContactOrigin.Custom, "someuser1alt@example.com"
        )

        # Upgrading primary email via integration must not work because primary email was changed to custom
        u.set_emails(
            ["someuser1@example.com"], ContactOrigin.Sisu, can_update_primary=True
        )
        db.session.commit()
        db.session.refresh(u)
        self.verify_primary_contact(
            u, Channel.EMAIL, ContactOrigin.Custom, "someuser1alt@example.com"
        )

        # Changing back to managed email should resume automatic email management
        # One email changes its integration back to Sisu
        u.email = "someuser1work@example.com"
        db.session.commit()
        db.session.refresh(u)
        u.set_emails(
            ["someuser1new@example.com", "someuser1work@example.com"],
            ContactOrigin.Sisu,
            can_update_primary=True,
        )
        db.session.commit()
        db.session.refresh(u)
        self.verify_primary_contact(
            u, Channel.EMAIL, ContactOrigin.Sisu, "someuser1new@example.com"
        )
        self.verify_contacts(
            u,
            Channel.EMAIL,
            [
                (ContactOrigin.Custom, "someuser1alt@example.com"),
                (ContactOrigin.Sisu, "someuser1new@example.com"),
                (ContactOrigin.Sisu, "someuser1work@example.com"),
            ],
        )

        # Changing email directly still works like before
        # In that case email becomes verified and primary
        u.email = "someuser1differentemail@example.com"
        db.session.commit()
        db.session.refresh(u)
        self.verify_primary_contact(
            u,
            Channel.EMAIL,
            ContactOrigin.Custom,
            "someuser1differentemail@example.com",
        )
        self.verify_contacts(
            u,
            Channel.EMAIL,
            [
                (ContactOrigin.Custom, "someuser1differentemail@example.com"),
                (ContactOrigin.Custom, "someuser1alt@example.com"),
                (ContactOrigin.Sisu, "someuser1new@example.com"),
                (ContactOrigin.Sisu, "someuser1work@example.com"),
            ],
        )
