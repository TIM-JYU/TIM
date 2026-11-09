"""Helpers for imagex drag tracking (markup option "track").

Kept free of plugin server dependencies so that it can be unit tested.
"""


def merge_track_sessions(*logs) -> list:
    """Merges drag tracking logs (lists of sessions) by session id.

    For a session present in several logs, the copy with more events is kept.
    Entries that are not valid sessions are dropped. Result is sorted by session start time.
    """
    by_id = {}
    for log in logs:
        if not isinstance(log, list):
            continue
        for s in log:
            if not (
                isinstance(s, dict)
                and isinstance(s.get("id"), str)
                and isinstance(s.get("t"), (int, float))
                and not isinstance(s.get("t"), bool)
                and isinstance(s.get("e"), list)
            ):
                continue
            prev = by_id.get(s["id"])
            if prev is None or len(s["e"]) > len(prev["e"]):
                by_id[s["id"]] = s
    return sorted(by_id.values(), key=lambda s: s["t"])
