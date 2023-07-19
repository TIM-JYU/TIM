"""Server tests for TIM-calendar"""
from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.notification.send_email import sent_mails_in_testing
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


class CalendarTest(TimRouteTest):
    def post_event(self, user: User, booker_group: str | None = None):
        """Helper function to post a basic event"""

        r = self.json_post(
            f"/calendar/events",
            {
                "events": [
                    {
                        "title": "Otsake",
                        "location": "sijainti",
                        "description": "kuvaus",
                        "start": "2022-05-18T07:20:00+00:00",
                        "end": "2022-05-18T07:40:00+00:00",
                        "signup_before": "2022-05-18T07:20:00+00:00",
                        "booker_groups": [booker_group or user.name],
                        "setter_groups": [user.name],
                        "tags": [],
                        "max_size": 1,
                    }
                ]
            },
            False,
        )
        return r[0]


class CalendarModificationTest(CalendarTest):
    def test_event_add_and_delete(self):
        """Events are queried, an event is created by Test user 1 and then deleted"""

        self.login_test1()
        self.get(
            f"/calendar/events",
            expect_status=200,
            expect_content=[],
        )

        event = self.post_event(self.test_user_1)
        self.get(
            f"calendar/events",
            expect_status=200,
            expect_content=[
                {
                    "id": event["id"],
                    "title": "Otsake",
                    "start": "2022-05-18T07:20:00+00:00",
                    "end": "2022-05-18T07:40:00+00:00",
                    "meta": {
                        "booker_groups": [],
                        "description": "kuvaus",
                        "enrollments": 0,
                        "location": "sijainti",
                        "maxSize": 1,
                        "important": False,
                        "extraEnrollments": None,
                        "isExtra": False,
                        "send_notifications": True,
                        "signup_before": "2022-05-18T07:20:00+00:00",
                        "owner": {
                            "name": self.test_user_1.pretty_full_name,
                            "email": self.test_user_1.email,
                        },
                    },
                }
            ],
        )

        self.delete(
            f"calendar/events/{event['id']}",
            expect_status=200,
        )
        self.get(f"/calendar/events", expect_status=200, expect_content=[])
        self.logout()

    def test_event_modification(self):
        """An event is created and then modified by user"""

        self.login_test1()
        self.get(f"/calendar/events", expect_status=200, expect_content=[])
        event = self.post_event(self.test_user_1)
        self.json_put(
            f"/calendar/events/{event['id']}",
            {
                "event": {
                    "title": "ihauus Otsake",
                    "start": "2022-05-18T07:20:00+00:00",
                    "description": "toinen kuvaus",
                    "location": "eri sijainti",
                    "end": "2022-05-18T07:40:00+00:00",
                    "signup_before": "2022-05-18T07:20:00+00:00",
                    "max_size": 1,
                }
            },
            expect_status=200,
            as_tree=False,
        )
        self.get(
            f"calendar/events",
            expect_status=200,
            expect_content=[
                {
                    "id": event["id"],
                    "title": "ihauus Otsake",
                    "start": "2022-05-18T07:20:00+00:00",
                    "end": "2022-05-18T07:40:00+00:00",
                    "meta": {
                        "booker_groups": [],
                        "description": "toinen kuvaus",
                        "enrollments": 0,
                        "extraEnrollments": None,
                        "isExtra": False,
                        "send_notifications": True,
                        "location": "eri sijainti",
                        "important": False,
                        "maxSize": 1,
                        "owner": {
                            "name": self.test_user_1.pretty_full_name,
                            "email": self.test_user_1.email,
                        },
                        "signup_before": "2022-05-18T07:20:00+00:00",
                    },
                }
            ],
        )
        self.delete(
            f"calendar/events/{event['id']}",
            expect_status=200,
        )
        self.logout()


class CalendarBookMessageTest(CalendarTest):
    def test_event_message_send(self):
        d = DocEntry.create("event_message_send_test1")
        db.session.flush()
        ug = UserGroup.create("event_message_send_test1")
        self.test_user_2.add_to_group(ug, None)
        ug.admin_doc = d.block
        self.test_user_1.grant_access(d.block, AccessType.manage)
        self.commit_db()

        self.login_test1()
        event = self.post_event(self.test_user_1, "event_message_send_test1")
        event_id = event["id"]

        self.login_test2()
        self.json_post(
            "/calendar/bookings", json_data={"event_id": event_id, "booker_msg": ""}
        )
        self.json_put(
            "/calendar/bookings",
            json_data={
                "event_id": event_id,
                "booker_msg": "test",
                "booker_group": self.test_user_2.name,
            },
        )
        self.assertEqual(
            len(sent_mails_in_testing),
            3,
            "Booking notification and two message notifications should be sent",
        )
        self.assertEqual(
            sent_mails_in_testing[-1]["msg"],
            f"{self.test_user_2.real_name} ({self.test_user_2.name}) posted a message to TIM calendar event Otsake 18.05.2022 10:20-10:40 (UTC +0300):\n\ntest",
        )


class CalendarBookTest(CalendarTest):
    def test_booking(self):
        """Event is created by Test user 2 and booked by Test user 1."""
        ug = UserGroup.get_or_create_group("testbooking1")
        self.test_user_1.add_to_group(ug, None)
        d = DocEntry.create("testbooking1", self.test_user_2.get_personal_group())
        ug.admin_doc = d.block
        db.session.commit()
        self.login_test2()

        event = self.post_event_to_book(ug)

        self.get(
            f"/calendar/events?includeOwned=true",
            expect_status=200,
            expect_content=[
                {
                    "id": event["id"],
                    "title": "Bookattava",
                    "start": "2023-05-18T07:20:00+00:00",
                    "end": "2023-05-18T07:40:00+00:00",
                    "meta": {
                        "booker_groups": [],
                        "description": "kerhon kokous",
                        "enrollments": 0,
                        "location": "kerhohuone",
                        "maxSize": 2,
                        "extraEnrollments": None,
                        "isExtra": False,
                        "important": False,
                        "send_notifications": True,
                        "signup_before": "2023-05-18T07:20:00+00:00",
                        "owner": {
                            "name": self.test_user_2.pretty_full_name,
                            "email": self.test_user_2.email,
                        },
                    },
                }
            ],
        )
        self.logout()
        self.login_test1()
        self.get(
            f"/calendar/events/{event['id']}/bookers",
            expect_status=403,
            expect_content={"error": "No permission to see event bookers"},
        )
        self.json_post(
            f"/calendar/bookings",
            {"event_id": event["id"], "booker_msg": ""},
            expect_status=200,
        )

        self.logout()
        self.login_test2()

        self.get(f"/calendar/events/{event['id']}/bookers", expect_status=200)

        self.delete(
            f"calendar/events/{event['id']}",
            expect_status=200,
        )
        self.logout()

    def post_event_to_book(self, user):
        """Helper function to post a basic event to book"""
        r = self.json_post(
            f"/calendar/events",
            {
                "events": [
                    {
                        "title": "Bookattava",
                        "location": "kerhohuone",
                        "description": "kerhon kokous",
                        "start": "2023-05-18T07:20:00+00:00",
                        "end": "2023-05-18T07:40:00+00:00",
                        "signup_before": "2023-05-18T07:20:00+00:00",
                        "booker_groups": [user.name],
                        "setter_groups": [],
                        "tags": [],
                        "max_size": 2,
                    }
                ]
            },
            False,
        )
        return r[0]
