from timApp.tests.server.timroutetest import TimRouteTest


class CalendarTest(TimRouteTest):
    def testEvents(self):
        self.login_test1()

        self.get(
            f"/calendar/events",
            expect_status=200,
            expect_content=[],
        )

        self.json_post(
            f"/calendar/events",
            {
                "events": [
                    {
                        "id": 1,
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

        self.get(
            f"calendar/events",
            expect_status=200,
            expect_content=[
                {
                    "id": 1,
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
