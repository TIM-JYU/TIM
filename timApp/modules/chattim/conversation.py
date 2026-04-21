from __future__ import annotations

import os
import json
import time
from dataclasses import dataclass, asdict
from collections import deque
from timApp.util.flask.cache import cache
from typing import BinaryIO, Iterator, Any
from .model import Message, Usage


@dataclass
class ChatMessage:
    """Conversation message saved on the disk."""

    content: str
    """Content of the message."""
    role: Message.Role
    """Role of the message sender."""
    timestamp: int
    """Timestamp of when the message was sent in milliseconds."""
    usage: Usage | None = None
    """Tokens used for generating the message."""

    def to_dict(self) -> dict:
        """Convert `ChatMessage` object to a `dict`.
        :return: Dictionary of `ChatMessage` data.
        """
        return asdict(self)

    @staticmethod
    def from_dict(d: dict) -> ChatMessage:
        """Parse `ChatMessage` from `dict`.
        :param d: Dictionary of `ChatMessage` data.
        :return: `ChatMessage` instance.
        """
        usage = d.get("usage")
        msg_dict = dict(d)
        if isinstance(usage, dict):
            try:
                msg_dict["usage"] = Usage(**usage)
            except TypeError:
                msg_dict["usage"] = None
        return ChatMessage(**msg_dict)

    @staticmethod
    def ts_ms() -> int:
        """Timestamp in milliseconds."""
        return time.time_ns() // 1_000_000


class ConversationManager:
    """Manages conversation histories."""

    def __init__(
        self,
        root_dir: str,
        *,
        cache_ttl_s: int = 60 * 15,  # 15 min
        cache_tail_len: int = 64,
    ):
        # TODO: change root path naming
        root_path = os.path.join(root_dir, "history", "chattim")
        self._store = ConversationStore(root_path)
        self.ttl = cache_ttl_s
        self.tail_len = cache_tail_len

    def append_messages(
        self, plugin_id: str, user_id: str, messages: list[ChatMessage]
    ) -> None:
        """
        Append messages to the conversation history of the user.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param messages: A list of messages to append to the file.
        """
        json_messages = self._store.append_messages(plugin_id, user_id, messages)
        if not json_messages:
            return
        self._append_to_cache(plugin_id, user_id, json_messages)

    def get_history_n(
        self,
        plugin_id: str,
        user_id: str,
        last_n: int | None = None,
        offset: int = 0,
    ) -> list[ChatMessage]:
        """
        Return the last N messages from the history of the specified conversation.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param last_n: Last N messages to return or all if None.
        :param offset: Amount of messages to skip from the end.
        :return: List of `ChatMessage` objects in the conversation.
        """
        off = max(offset, 0)

        # Check if the needed messages could be in the cache
        if last_n is not None and last_n + off <= self.tail_len:
            tail_needed_n = last_n + off
            cached = self._get_from_cache(plugin_id, user_id) or self._update_cache(
                plugin_id, user_id
            )
            start = -tail_needed_n
            end = None if off == 0 else -off
            if cached is not None and len(cached) >= tail_needed_n:
                raw_slice = cached[start:end]
                return self._parse_raw_messages(raw_slice)

        return self._store.load_messages_n(plugin_id, user_id, last_n, off) or []

    def get_history_time_window(
        self,
        plugin_id: str,
        user_id: str,
        ts_begin: int | None,
        ts_end: int | None,
        max_messages: int,
    ) -> list[ChatMessage]:
        """
        Return the message from the history of the specified conversation.
        Only includes the messages inside the time window.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param ts_begin: Timestamp of the beginning of the time window in milliseconds or 0.
        :param ts_end: Timestamp of the end of the time window in milliseconds or current time.
        :param max_messages: Maximum amount of messages to return in the time window.
        :return: List of `ChatMessage` objects or None if no history.
        """
        ts_b = ts_begin or 0
        ts_e = ts_end or ChatMessage.ts_ms()
        kwargs_id = dict(plugin_id=plugin_id, user_id=user_id)
        kwargs: dict[str, Any] = dict(
            **kwargs_id,
            ts_begin=ts_b,
            ts_end=ts_e,
            max_messages=max_messages,
        )

        cached = self._get_from_cache(**kwargs_id) or self._update_cache(**kwargs_id)
        if cached is not None:
            # Check if the time window is in the cached tail
            oldest_idx, newest_idx = self._time_window_edges_idx(
                cached, ts_b, ts_e, ts_begin is not None, ts_end is not None
            )
            end = newest_idx + 1 if newest_idx >= 0 else 0
            if ts_begin is None and len(cached[:end]) < max_messages:
                # Cache doesn't have enough older messages
                oldest_idx = -1

            if oldest_idx >= 0 and newest_idx >= 0:
                cached_slice = cached[oldest_idx:end]
                start = None if len(cached_slice) < max_messages else -max_messages
                return self._parse_raw_messages(cached_slice[start:])

        return self._store.load_messages_time_window(**kwargs) or []

    def _get_from_cache(self, plugin_id: str, user_id: str) -> list[dict] | None:
        """Get the message list tail from the cache if it exists and is up to date."""
        key = self._cache_key_tail(plugin_id, user_id)
        cache_res = cache.get(key)
        if not isinstance(cache_res, dict):
            return None

        cache_modify = cache_res.get("mtime_ns")
        cache_size = cache_res.get("size")
        if not isinstance(cache_modify, int) or not isinstance(cache_size, int):
            return None
        try:
            modify_time, size = self._file_stat(plugin_id, user_id)
        except FileNotFoundError:
            return None
        if cache_modify != modify_time or cache_size != size:
            return None

        messages = cache_res.get("messages")
        if not isinstance(messages, list):
            return None

        return messages

    def _update_cache(self, plugin_id: str, user_id: str) -> list[dict] | None:
        """Update the cache with the new conversation tail."""
        try:
            modify_time, size = self._file_stat(plugin_id, user_id)
        except FileNotFoundError:
            return None

        key = self._cache_key_tail(plugin_id, user_id)
        msgs = self._store.load_messages_n(plugin_id, user_id, self.tail_len, 0) or []
        messages_dict = [m.to_dict() for m in msgs]
        payload = self._cache_payload_tail(modify_time, size, messages_dict)
        self._set_cache(key, payload)
        return messages_dict

    def _append_to_cache(
        self, plugin_id: str, user_id: str, messages: list[dict]
    ) -> None:
        """Update the cache by appending new messages to the end."""
        key = self._cache_key_tail(plugin_id, user_id)
        try:
            modify_time, size = self._file_stat(plugin_id, user_id)
        except FileNotFoundError:
            cache.delete(key)
            return

        res = cache.get(key)
        cached_tail: list[dict] = []
        if isinstance(res, dict):
            m = res.get("messages")
            if isinstance(m, list):
                cached_tail = m
        new_messages = (cached_tail + messages)[-self.tail_len :]
        payload = self._cache_payload_tail(modify_time, size, new_messages)
        self._set_cache(key, payload)

    def _file_stat(self, plugin_id: str, user_id: str) -> tuple[int, int]:
        """Get the modification time and size of the conversation file."""
        path = self._store.resolve_conversation_path(plugin_id, user_id)
        res = os.stat(path)
        return res.st_mtime_ns, res.st_size

    def _set_cache(self, key: str, value: Any) -> None:
        cache.set(key, value, timeout=self.ttl)

    @staticmethod
    def _cache_key_tail(plugin_id: str, user_id: str) -> str:
        return f"chattim:tail:{plugin_id}:{user_id}"

    @staticmethod
    def _cache_payload_tail(mtime_ns: int, size: int, messages: list[dict]) -> dict:
        return {"mtime_ns": mtime_ns, "size": size, "messages": messages}

    @staticmethod
    def _parse_raw_messages(raw_messages: list[dict]) -> list[ChatMessage]:
        out: list[ChatMessage] = []
        for d in raw_messages:
            if not isinstance(d, dict):
                continue
            try:
                out.append(ChatMessage.from_dict(d))
            except TypeError:
                continue
        return out

    @staticmethod
    def _time_window_edges_idx(
        messages: list[dict],
        ts_b: int,
        ts_e: int,
        find_border_b: bool = False,
        find_border_e: bool = False,
    ) -> tuple[int, int]:
        """
        Find the edges of the time window from the message list.
        Returns valid indexes only if the whole time window is included.
        Assumes that the messages are in chronological order.

        :param messages: List of messages to find from.
        :param ts_b: Beginning of the time window in milliseconds.
        :param ts_e: Ending of the time window in milliseconds.
        :param find_border_b: Whether to find the start border of the time window.
        :param find_border_e: Whether to find the end border of the time window.
        :return: Tuple containing the start and end index in the list.
        """
        oldest_idx: int = -1
        newest_idx: int = -1
        oldest_found: bool = False
        newest_found: bool = False

        if ts_b > ts_e:
            return oldest_idx, newest_idx

        def get_ts(dict_maybe: dict) -> int | None:
            if not isinstance(dict_maybe, dict):
                return None
            ts_any = dict_maybe.get("timestamp")
            if not isinstance(ts_any, int):
                return None
            return ts_any

        for i in range(len(messages)):
            idx_end = len(messages) - i - 1
            # TODO: optimize
            # only get if not found yet
            ts = get_ts(messages[i])
            ts_end = get_ts(messages[idx_end])

            # Check start border
            if not oldest_found and ts is not None:
                if ts > ts_e:
                    break
                if ts <= ts_b:
                    oldest_idx = i
                if ts_b <= ts <= ts_e:
                    if not find_border_b or oldest_idx >= 0:
                        oldest_idx = i
                        oldest_found = True
                    else:
                        # There could exist an older message that is not included
                        break

            # Check end border
            if not newest_found and ts_end is not None:
                if ts_end < ts_b:
                    break
                if ts_end >= ts_e:
                    newest_idx = idx_end
                if ts_b <= ts_end <= ts_e:
                    if not find_border_e or newest_idx >= 0:
                        newest_idx = idx_end
                        newest_found = True
                    else:
                        # There could exist a newer message that is not included
                        break

            if newest_found and oldest_found:
                return oldest_idx, newest_idx  # Time window exists

        return -1, -1


class ConversationStore:
    """Handles disk IO for storing the conversations."""

    def __init__(self, root_path: str):
        self.root_path = root_path

    def append_messages(
        self, plugin_id: str, user_id: str, messages: list[ChatMessage]
    ) -> list[dict]:
        """
        Append a list of `ChatMessage` objects to the conversation file in order.
        If the conversation file path does not exist, it will be created.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param messages: A list of messages to append to the file.
        :return: List of dicts of the appended messages.
        """
        file_path = self.resolve_conversation_path(plugin_id, user_id)
        os.makedirs(os.path.dirname(file_path), exist_ok=True)

        json_lines: list[str] = []
        json_messages: list[dict] = []
        for message in messages:
            try:
                msg_dict = message.to_dict()
                json_lines.append(json.dumps(msg_dict) + "\n")
                json_messages.append(msg_dict)
            except TypeError:
                continue

        data = "".join(json_lines)
        with open(file_path, "a", encoding="utf-8") as f:
            f.write(data)

        return json_messages

    def load_messages_n(
        self, plugin_id: str, user_id: str, last_n: int | None = None, offset: int = 0
    ) -> list[ChatMessage] | None:
        """
        Loads messages from disk if the conversation exists.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param last_n: Last N messages to return or all if None.
        :param offset: Amount of messages to skip from the end.
        :return: List of `ChatMessage` objects or None if no history.
        """
        if last_n is not None and last_n <= 0:
            return []
        file_path = self.resolve_conversation_path(plugin_id, user_id)
        to_skip = max(offset, 0)
        try:
            if last_n is None:
                # Read all the lines in order from the beginning of the file.
                with open(file_path, "r", encoding="utf-8") as f:
                    out: list[ChatMessage] = []
                    for line in f:
                        msg = self._parse_message_line(line)
                        if msg is not None:
                            out.append(msg)
                    return out[:-to_skip]

            # Read from the end without reading the whole file.
            out_rev: list[ChatMessage] = []
            with open(file_path, "rb") as f:
                for line in self._iter_lines_reverse(f):
                    if to_skip > 0:
                        to_skip -= 1
                        continue
                    msg = self._parse_message_line(line)
                    if msg is None:
                        continue
                    out_rev.append(msg)
                    if len(out_rev) >= last_n:
                        break
            out_rev.reverse()
            return out_rev

        except FileNotFoundError:
            return None

    def load_messages_time_window(
        self,
        plugin_id: str,
        user_id: str,
        ts_begin: int,
        ts_end: int,
        max_messages: int,
    ) -> list[ChatMessage] | None:
        """
        Loads messages from disk if the conversation exists.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param ts_begin: Timestamp of the beginning of the time window in milliseconds.
        :param ts_end: Timestamp of the end of the time window in milliseconds.
        :param max_messages: Maximum amount of messages to return in the time window.
        :return: List of `ChatMessage` objects or None if no history.
        """
        if ts_end <= ts_begin:
            return []
        file_path = self.resolve_conversation_path(plugin_id, user_id)
        try:
            # Read from the end and stop early if the file appears chronological.
            out_rev: list[ChatMessage] = []
            chronological = True
            last_ts: int | None = None

            with open(file_path, "rb") as f:
                for line in self._iter_lines_reverse(f):
                    if len(out_rev) >= max_messages:
                        break

                    msg = self._parse_message_line(line)
                    if msg is None:
                        continue

                    if last_ts is not None and msg.timestamp > last_ts:
                        # Messages are not in chronological order.
                        chronological = False
                    last_ts = msg.timestamp

                    if msg.timestamp > ts_end:
                        continue
                    if msg.timestamp < ts_begin:
                        if chronological:
                            break
                        continue

                    out_rev.append(msg)

            out_rev.reverse()
            return out_rev

        except FileNotFoundError:
            return None

    @staticmethod
    def _parse_message_line(line: str) -> ChatMessage | None:
        """Parse a message line from JSONL to `ChatMessage`.

        :param line: Line from the JSONL file.
        :return: The parsed `ChatMessage` object or `None` if invalid.
        """

        line = line.strip()
        if not line:
            return None
        try:
            d = json.loads(line)
            if not isinstance(d, dict):
                return None
            return ChatMessage.from_dict(d)
        except (json.JSONDecodeError, TypeError, ValueError):
            return None

    @staticmethod
    def _iter_lines_reverse(file: BinaryIO, chunk_size: int = 8192) -> Iterator[str]:
        """
        Iterate a UTF-8 text file line-by-line from the end.
        The file must be opened in binary mode, and it is read in chunks.

        :param file: The file to iterate.
        :param chunk_size: Size of the chunks to read.
        :raises ValueError: If chunk size is negative.
        :return: An iterator over the lines in the file in reverse order.
        """
        if chunk_size <= 0:
            raise ValueError("chunk_size must be positive")

        file.seek(0, os.SEEK_END)
        pos = file.tell()

        buffer: deque[bytes] = deque()  # Contains a part of a line

        # Return the pending line and clear buffer
        def emit_pending() -> str | None:
            if not buffer:
                return None
            b = b"".join(buffer)
            buffer.clear()
            if b.endswith(b"\r"):
                b = b[:-1]
            if not b:
                return None
            return b.decode("utf-8", errors="replace")

        while pos > 0:
            # Read a chunk
            read_size = chunk_size if pos >= chunk_size else pos
            pos -= read_size
            file.seek(pos, os.SEEK_SET)
            chunk = file.read(read_size)
            if not chunk:
                break

            # Process the chunk and yield lines if there are any
            nl_rfind_pos: int = len(chunk)
            while nl_rfind_pos > 0:
                nl = chunk.rfind(b"\n", 0, nl_rfind_pos)
                if nl == -1:
                    # The line is either too big for one chunk or cut
                    buffer.appendleft(chunk[:nl_rfind_pos])
                    break

                # Combine the start of the line with possible pending line end
                line_start = chunk[nl + 1 : nl_rfind_pos]
                if line_start:
                    buffer.appendleft(line_start)
                line = emit_pending()
                if line is not None:
                    yield line

                nl_rfind_pos = nl

        if buffer:
            line = emit_pending()
            if line is not None:
                yield line

    def resolve_conversation_path(self, plugin_id: str, user_id: str) -> str:
        """
        Resolve the path to the conversation JSONL file.
        Does not verify if the path exists on the disk.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :return: The file system path to the conversation JSONL file.
        """
        file_name = "messages.jsonl"
        return os.path.join(self.root_path, plugin_id, user_id, file_name)
