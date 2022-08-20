"""Server tests for TIM-calendar"""
from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


class CalendarTest(TimRouteTest):
    def post_event(self, user: User):
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
                        "booker_groups": [user.name],
                        "setter_groups": [user.name],
                        "tags": [],
                        "max_size": 1,
                    }
                ]
            },
            False,
        )
        return r[0]

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
                        "extraEnrollments": None,
                        "isExtra": False,
                        "send_notifications": True,
                        "signup_before": "2022-05-18T07:20:00+00:00",
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
                        "maxSize": 1,
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


class CalendarBookTest(TimRouteTest):
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
                        "send_notifications": True,
                        "signup_before": "2023-05-18T07:20:00+00:00",
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
