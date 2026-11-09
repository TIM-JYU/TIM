"""Server tests for answer tracking:

- /trackEvents route (csplugin edit tracking log file)
- imagex markup option `track` (drag tracking sessions stored in the answer)
- csplugin markup option `track` (edit tracking summary stored in the answer)
"""

import json
from pathlib import Path

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app

LOG_NAME = "test_edit_track.log"


def session(sid: str, t: float, n_events: int, init=None) -> dict:
    return {
        "id": sid,
        "t": t,
        "init": init or [],
        "e": [["a", 10 + i, 10, 1.0] for i in range(n_events)],
    }


class TrackEventsRouteTest(TimRouteTest):
    def setUp(self):
        super().setUp()
        self.log_path = Path(app.config["FILES_PATH"]) / LOG_NAME
        self.log_path.unlink(missing_ok=True)

    def tearDown(self):
        self.log_path.unlink(missing_ok=True)
        super().tearDown()

    def post_events(self, ev: list, **kwargs):
        return self.json_post(
            "/trackEvents",
            {"task": "1.t", "sid": "abc123", "seq": 0, "t0": 1762700000.3, "ev": ev}
            | kwargs,
        )

    def read_log(self) -> list[dict]:
        if not self.log_path.exists():
            return []
        return [json.loads(l) for l in self.log_path.read_text().splitlines()]

    def test_disabled_by_default(self):
        self.login_test1()
        with self.temp_config({"EDIT_TRACK_LOG_FILE": None}):
            self.post_events([["k", 1.0, 5, 0]])
        self.assertEqual([], self.read_log())

    def test_logs_batch(self):
        self.login_test1()
        ev = [
            ["init", 0, 0, "811c9dc5"],
            ["k", 2.5, 12, 1],
            ["p", 0.4, 30, "deadbeef"],
            ["away", 3.0],
        ]
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            self.post_events(ev)
            self.post_events([["back", 20.1]], seq=1)
        lines = self.read_log()
        self.assertEqual(2, len(lines))
        first = lines[0]
        self.assertEqual(self.test_user_1.name, first["user"])
        self.assertEqual(self.test_user_1.id, first["uid"])
        self.assertEqual("1.t", first["task"])
        self.assertEqual("abc123", first["sid"])
        self.assertEqual(0, first["seq"])
        self.assertEqual(1762700000.3, first["t0"])
        self.assertEqual(ev, first["ev"])
        self.assertIn("time", first)
        self.assertEqual(1, lines[1]["seq"])

    def test_seq_is_optional(self):
        self.login_test1()
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            self.json_post(
                "/trackEvents",
                {"task": "1.t", "sid": "s", "t0": 1, "ev": [["u", 0.1]]},
            )
        self.assertEqual(0, self.read_log()[0]["seq"])

    def test_anonymous_not_logged(self):
        self.logout()
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            self.post_events([["k", 1.0, 5, 0]])
        self.assertEqual([], self.read_log())

    def test_too_many_events_ignored(self):
        self.login_test1()
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            self.post_events([["k", 0.1, 1, 0]] * 2001)
            self.post_events([["k", 0.1, 1, 0]] * 2000)
        lines = self.read_log()
        self.assertEqual(1, len(lines))
        self.assertEqual(2000, len(lines[0]["ev"]))

    def test_missing_fields_rejected(self):
        self.login_test1()
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            self.json_post("/trackEvents", {"task": "1.t", "ev": []}, expect_status=422)
        self.assertEqual([], self.read_log())


class ImagexTrackTest(TimRouteTest):
    MARKUP = """
``` {{#t plugin="imagex"}}
track: {track}
objects:
  - id: apple
    type: rectangle
    position: [10, 10]
    size: [20, 20]
targets:
  - id: box
    type: rectangle
    position: [100, 100]
    size: [50, 50]
    points:
      apple: 1
```"""

    def setUp(self):
        super().setUp()
        self.wait_for_url("http://imagex:5000/reqs")
        self.login_test1()

    def answer(self, d, track=None, x=10, y=10):
        inp = {"drags": [{"did": "obj1", "id": "apple", "position": [x, y]}]}
        if track is not None:
            inp["track"] = track
        return self.post_answer("imagex", f"{d.id}.t", user_input=inp)

    def latest(self, d) -> dict:
        return (
            self.test_user_1.get_answers_for_task(f"{d.id}.t").first().content_as_json
        )

    def test_track_saved_and_merged(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))

        s1 = session("s1", 100, 2)
        self.answer(d, [s1], x=11)
        self.assertEqual([s1], self.latest(d)["track"])

        # Second window saves: knows only its own session -> union with previous answer
        s2 = session("s2", 50, 1, init=[["apple", 11, 10]])
        self.answer(d, [s2], x=12)
        self.assertEqual([s2, s1], self.latest(d)["track"])  # sorted by t

        # First window saves again with more events in s1 and a stale copy of nothing else
        s1_longer = session("s1", 100, 5)
        self.answer(d, [s1_longer], x=13)
        self.assertEqual([s2, s1_longer], self.latest(d)["track"])

        # A stale, shorter copy never overwrites a longer one
        self.answer(d, [session("s1", 100, 1)], x=14)
        self.assertEqual([s2, s1_longer], self.latest(d)["track"])

    def test_unchanged_board_creates_no_new_answer(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))
        s1 = session("s1", 100, 2)
        self.answer(d, [s1], x=11)
        # Same board, longer log (e.g. another save marker) -> redundant, log not updated
        r = self.answer(d, [session("s1", 100, 3)], x=11)
        self.assertIsNone(r["savedNew"])
        answers = self.test_user_1.get_answers_for_task(f"{d.id}.t").all()
        self.assertEqual(1, len(answers))
        self.assertEqual([s1], answers[0].content_as_json["track"])
        # Changed board -> new answer with the longer log
        s1_longer = session("s1", 100, 4)
        r = self.answer(d, [s1_longer], x=12)
        self.assertIsNotNone(r["savedNew"])
        self.assertEqual([s1_longer], self.latest(d)["track"])

    def test_track_kept_when_input_missing(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))
        s1 = session("s1", 100, 2)
        self.answer(d, [s1], x=11)
        self.answer(d, None, x=12)
        self.assertEqual([s1], self.latest(d)["track"])

    def test_invalid_track_entries_dropped(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))
        s1 = session("s1", 100, 1)
        self.answer(d, [s1, "x", {"id": 5}, ["a", 1, 2, 3]], x=11)
        self.assertEqual([s1], self.latest(d)["track"])
        self.answer(d, "not a list", x=12)
        self.assertEqual([s1], self.latest(d)["track"])

    def test_track_disabled(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="false"))
        self.answer(d, [session("s1", 100, 2)])
        self.assertNotIn("track", self.latest(d))

    def test_track_missing_from_markup(self):
        d = self.create_doc(
            initial_par=self.MARKUP.format(track="true").replace("track: true\n", "")
        )
        self.answer(d, [session("s1", 100, 2)])
        self.assertNotIn("track", self.latest(d))

    def test_points_unaffected(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))
        r = self.answer(d, [session("s1", 100, 1)], x=110, y=110)
        self.assertEqual(
            1, self.test_user_1.get_answers_for_task(f"{d.id}.t").first().points
        )
        self.assertEqual("saved", r["web"]["result"])


class CsPluginTrackTest(TimRouteTest):
    MARKUP = """
``` {{#t plugin="csPlugin"}}
type: text
track: {track}
```"""

    def setUp(self):
        super().setUp()
        self.wait_for_url("http://csplugin:5000/cs/reqs")
        self.login_test1()

    def answer(self, d, code: str, summary=None):
        inp = {"usercode": code, "type": "text"}
        if summary is not None:
            inp["trackSummary"] = summary
        return self.post_answer("csPlugin", f"{d.id}.t", user_input=inp)

    def answers(self, d):
        return self.test_user_1.get_answers_for_task(f"{d.id}.t").all()

    def test_summary_saved_and_merged(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))
        a = {"s1": {"t": 100, "n": 3, "typed": 5, "paste": 0}}
        self.answer(d, "hello", a)
        self.assertEqual(a, self.answers(d)[0].content_as_json["trackSummary"])

        # New session from another page load + longer copy of s1
        b = {
            "s1": {"t": 100, "n": 6, "typed": 9, "paste": 1},
            "s2": {"t": 200, "n": 2, "typed": 3, "paste": 0},
        }
        self.answer(d, "hello world", b)
        self.assertEqual(b, self.answers(d)[0].content_as_json["trackSummary"])

        # A window that only knows s2 doesn't drop s1
        self.answer(d, "hello world!", {"s2": {"t": 200, "n": 4, "typed": 4}})
        merged = self.answers(d)[0].content_as_json["trackSummary"]
        self.assertEqual(b["s1"], merged["s1"])
        self.assertEqual(4, merged["s2"]["n"])

    def test_unchanged_code_creates_no_new_answer(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))
        a = {"s1": {"t": 100, "n": 3, "typed": 5}}
        self.answer(d, "hello", a)
        self.assertEqual(1, len(self.answers(d)))
        # Same code, newer summary -> summary not updated, answer is redundant
        self.answer(d, "hello", {"s1": {"t": 100, "n": 9, "typed": 5, "away": 2}})
        answers = self.answers(d)
        self.assertEqual(1, len(answers))
        self.assertEqual(a, answers[0].content_as_json["trackSummary"])
        # Changed code -> new answer with the newer summary
        self.answer(d, "hello2", {"s1": {"t": 100, "n": 10, "typed": 6}})
        answers = self.answers(d)
        self.assertEqual(2, len(answers))
        self.assertEqual(10, answers[0].content_as_json["trackSummary"]["s1"]["n"])

    def test_summary_kept_when_input_missing(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))
        a = {"s1": {"t": 100, "n": 3}}
        self.answer(d, "one", a)
        self.answer(d, "two")
        self.assertEqual(a, self.answers(d)[0].content_as_json["trackSummary"])

    def test_track_disabled(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="false"))
        self.answer(d, "hello", {"s1": {"t": 100, "n": 3}})
        self.assertNotIn("trackSummary", self.answers(d)[0].content_as_json)

    def test_invalid_summary_ignored(self):
        d = self.create_doc(initial_par=self.MARKUP.format(track="true"))
        self.answer(d, "hello", "garbage")
        self.assertNotIn("trackSummary", self.answers(d)[0].content_as_json)
        self.answer(d, "hello2", {"s1": "x", "s2": {"t": 1, "n": 1}})
        self.assertEqual(
            {"s2": {"t": 1, "n": 1}},
            self.answers(d)[0].content_as_json["trackSummary"],
        )
