"""Unit tests for merging tracking data (imagex `track` and csplugin `track`)."""

from unittest import TestCase

from timApp.modules.cs.cs_utils import merge_track_summaries
from timApp.modules.imagex.track_utils import merge_track_sessions


def session(sid: str, t: float, n_events: int, init=None) -> dict:
    return {
        "id": sid,
        "t": t,
        "init": init or [],
        "e": [["apple", i, i, 1.0] for i in range(n_events)],
    }


class TestMergeTrackSessions(TestCase):
    def test_empty_and_missing(self):
        self.assertEqual([], merge_track_sessions())
        self.assertEqual([], merge_track_sessions(None, None))
        self.assertEqual([], merge_track_sessions([], []))
        self.assertEqual([], merge_track_sessions("garbage", {"a": 1}, 5))

    def test_single_log_passes_through(self):
        log = [session("a", 100, 2)]
        self.assertEqual(log, merge_track_sessions(log))

    def test_union_of_different_sessions(self):
        a = session("a", 100, 2)
        b = session("b", 200, 3)
        self.assertEqual([a, b], merge_track_sessions([a], [b]))

    def test_sorted_by_start_time(self):
        a = session("a", 300, 1)
        b = session("b", 100, 1)
        c = session("c", 200.5, 1)
        merged = merge_track_sessions([a, b], [c])
        self.assertEqual(["b", "c", "a"], [s["id"] for s in merged])

    def test_duplicate_keeps_longer(self):
        short = session("a", 100, 2)
        long = session("a", 100, 5)
        self.assertEqual([long], merge_track_sessions([short], [long]))
        self.assertEqual([long], merge_track_sessions([long], [short]))

    def test_duplicate_equal_length_keeps_first(self):
        first = session("a", 100, 2, init=[["x", 1, 1]])
        second = session("a", 100, 2, init=[["y", 2, 2]])
        self.assertEqual([first], merge_track_sessions([first], [second]))

    def test_two_windows_saved_in_turn(self):
        """Window B saves first, then window A saves without knowing B's later moves."""
        prev_answer = [session("old", 50, 4), session("B", 200, 3)]
        from_window_a = [
            session("old", 50, 4),
            session("A", 190, 6),
            session("B", 200, 1),
        ]
        merged = merge_track_sessions(prev_answer, from_window_a)
        self.assertEqual(["old", "A", "B"], [s["id"] for s in merged])
        self.assertEqual(3, len(merged[2]["e"]))  # B's longer copy kept

    def test_invalid_entries_dropped(self):
        good = session("a", 100, 1)
        bad = [
            "x",
            None,
            {"id": 1, "t": 1, "e": []},  # id not a string
            {"id": "b", "t": "1", "e": []},  # t not a number
            {"id": "c", "t": True, "e": []},  # bool is not a time
            {"id": "d", "t": 1, "e": "nope"},  # e not a list
            {"id": "e", "t": 1},  # e missing
            ["a", 1, 2, 3],  # old flat format entry
        ]
        self.assertEqual([good], merge_track_sessions([good, *bad]))


class TestMergeTrackSummaries(TestCase):
    def test_empty_and_missing(self):
        self.assertEqual({}, merge_track_summaries())
        self.assertEqual({}, merge_track_summaries(None, None))
        self.assertEqual({}, merge_track_summaries({}, "x", [1, 2]))

    def test_union(self):
        a = {"s1": {"t": 1, "n": 3, "paste": 1}}
        b = {"s2": {"t": 2, "n": 5, "paste": 0}}
        self.assertEqual({**a, **b}, merge_track_summaries(a, b))

    def test_duplicate_keeps_more_events(self):
        old = {"s1": {"t": 1, "n": 3, "typed": 10}}
        new = {"s1": {"t": 1, "n": 7, "typed": 40}}
        self.assertEqual(new, merge_track_summaries(old, new))
        self.assertEqual(new, merge_track_summaries(new, old))

    def test_previous_kept_when_no_new(self):
        old = {"s1": {"t": 1, "n": 3}}
        self.assertEqual(old, merge_track_summaries(old, None))

    def test_invalid_entries_dropped(self):
        good = {"s1": {"t": 1, "n": 2}}
        bad = {"s2": "x", "s3": None, "s4": {"t": 1, "n": "many"}}
        self.assertEqual(good, merge_track_summaries({**good, **bad}))
