"""Server tests for TIM-calendar"""
from timApp.plugin.calendar.calendar import initialize_db
from timApp.tests.server.timroutetest import TimRouteTest


class CalendarTest(TimRouteTest):
    def post_event(self, event_id: int):
        """Helper function to post a basic event"""

        self.json_post(
            f"/calendar/events",
            {
                "events": [
                    {
                        "id": event_id,
                        "title": "Otsake",
                        "location": "sijainti",
                        "description": "kuvaus",
                        "start": "2022-05-18T07:20:00+00:00",
                        "end": "2022-05-18T07:40:00+00:00",
                        "signup_before": "2022-05-18T07:20:00+00:00",
                        "booker_groups": [""],
                        "setter_groups": [""],
                        "event_groups": ["", ""],
                        "max_size": 1,
                    }
                ]
            },
            False,
        )

    def test_event_add_and_delete(self):
        """Events are queried, an event is created by Test user 1 and then deleted"""

        self.login_test1()
        self.get(
            f"/calendar/events",
            expect_status=200,
            expect_content=[],
        )

        event_id = 1
        self.post_event(event_id)
        self.get(
            f"calendar/events",
            expect_status=200,
            expect_content=[
                {
                    "id": event_id,
                    "title": "Otsake",
                    "start": "2022-05-18T07:20:00+00:00",
                    "end": "2022-05-18T07:40:00+00:00",
                    "meta": {
                        "booker_groups": [],
                        "description": "kuvaus",
                        "enrollments": 0,
                        "location": "sijainti",
                        "maxSize": 1,
                        "signup_before": "2022-05-18T07:20:00+00:00",
                    },
                }
            ],
        )

        self.delete(
            f"calendar/events/{event_id}",
            expect_status=200,
        )
        self.get(f"/calendar/events", expect_status=200, expect_content=[])
        self.logout()

    def test_event_modification(self):
        """An event is created and then modified by user"""

        self.login_test1()
        self.get(f"/calendar/events", expect_status=200, expect_content=[])
        event_id = 2
        self.post_event(event_id)
        self.json_put(
            f"/calendar/events/{event_id}",
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
                    "id": event_id,
                    "title": "ihauus Otsake",
                    "start": "2022-05-18T07:20:00+00:00",
                    "end": "2022-05-18T07:40:00+00:00",
                    "meta": {
                        "booker_groups": [],
                        "description": "toinen kuvaus",
                        "enrollments": 0,
                        "location": "eri sijainti",
                        "maxSize": 1,
                        "signup_before": "2022-05-18T07:20:00+00:00",
                    },
                }
            ],
        )
        self.delete(
            f"calendar/events/{event_id}",
            expect_status=200,
        )
        self.logout()


class CalendarBookTest(TimRouteTest):
    def test_booking(self):
        """Event is created by Test user 2 and booked by Test user 1."""
        initialize_db()
        self.login_test2()
        event_id = 1
        self.post_event_to_book(event_id)

        self.get(
            f"/calendar/events",
            expect_status=200,
            expect_content=[
                {
                    "id": event_id,
                    "title": "Bookattava",
                    "start": "2023-05-18T07:20:00+00:00",
                    "end": "2023-05-18T07:40:00+00:00",
                    "meta": {
                        "booker_groups": [],
                        "description": "kerhon kokous",
                        "enrollments": 0,
                        "location": "kerhohuone",
                        "maxSize": 2,
                        "signup_before": "2023-05-18T07:20:00+00:00",
                    },
                }
            ],
        )
        self.logout()
        self.login_test1()
        self.get(
            f"/calendar/events/{event_id}/bookers",
            expect_status=403,
            expect_content={"error": "No permission to see event bookers"},
        )
        self.json_post(
            f"/calendar/bookings",
            {"event_id": event_id, "booker_msg": ""},
            expect_status=200,
        )

        self.logout()
        self.login_test2()

        self.get(f"/calendar/events/{event_id}/bookers", expect_status=200)

        self.delete(
            f"calendar/events/{event_id}",
            expect_status=200,
        )
        self.logout()

    def post_event_to_book(self, event_id):
        """Helper function to post a basic event to book"""
        self.json_post(
            f"/calendar/events",
            {
                "events": [
                    {
                        "id": event_id,
                        "title": "Bookattava",
                        "location": "kerhohuone",
                        "description": "kerhon kokous",
                        "start": "2023-05-18T07:20:00+00:00",
                        "end": "2023-05-18T07:40:00+00:00",
                        "signup_before": "2023-05-18T07:20:00+00:00",
                        "booker_groups": [""],
                        "setter_groups": [""],
                        "event_groups": ["", ""],
                        "max_size": 2,
                    }
                ]
            },
            False,
        )
