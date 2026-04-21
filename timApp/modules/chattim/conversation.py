from __future__ import annotations

import os
import json
import time
from dataclasses import dataclass, asdict
from collections import deque
from timApp.util.flask.cache import cache
from typing import BinaryIO, Iterator
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
        cache_ttl_s: int = 3600,  # 1h
        cache_tail_len: int = 100,
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
        self.append_to_cache(plugin_id, user_id, json_messages)

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
        try_tail_cache = last_n is not None and last_n + off <= self.tail_len

        if try_tail_cache:
            tail_needed_n = last_n + off
            cached = self.get_from_cache(plugin_id, user_id) or self.update_cache(
                plugin_id, user_id
            )
            start = -tail_needed_n
            end = None if off == 0 else -off
            if cached is not None and len(cached) >= tail_needed_n:
                raw_slice = cached[start:end]
                return self._parse_raw_messages(raw_slice)

        return self._store.load_messages_n(plugin_id, user_id, last_n, offset) or []

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
        kwargs = dict(
            **kwargs_id,
            ts_begin=ts_b,
            ts_end=ts_e,
            max_messages=max_messages,
        )

        # Not the tail
        if ts_end is not None:  # TODO: check if in cache
            return self._store.load_messages_time_window(**kwargs) or []

        cached = self.get_from_cache(**kwargs_id) or self.update_cache(**kwargs_id)
        if cached is not None:
            oldest_idx: int = self._oldest_in_time_window(cached, ts_b, ts_e)
            oldest_idx = 0 if len(cached) >= max_messages else oldest_idx
            if oldest_idx >= 0:
                filtered_raw = cached[oldest_idx:]
                start = None if len(filtered_raw) < max_messages else -max_messages
                return self._parse_raw_messages(filtered_raw[start:])

        return self._store.load_messages_time_window(**kwargs) or []

    def get_from_cache(self, plugin_id: str, user_id: str) -> list[dict] | None:
        key = self._cache_key_tail(plugin_id, user_id)
        cache_res = cache.get(key)
        if not isinstance(cache_res, dict):
            return None

        cache_modify = cache_res.get("mtime_ns")
        cache_size = cache_res.get("size")
        if not isinstance(cache_modify, int) or not isinstance(cache_size, int):
            return None
        try:
            modify_time, size = self.file_stat(plugin_id, user_id)
        except FileNotFoundError:
            return None
        if cache_modify != modify_time or cache_size != size:
            return None

        messages = cache_res.get("messages")
        if not isinstance(messages, list):
            return None

        return messages

    def update_cache(self, plugin_id: str, user_id: str) -> list[dict]:
        msgs = self._store.load_messages_n(plugin_id, user_id, self.tail_len, 0) or []
        try:
            modify_time, size = self.file_stat(plugin_id, user_id)
        except FileNotFoundError:
            return msgs

        key = self._cache_key_tail(plugin_id, user_id)
        messages_dict = [m.to_dict() for m in msgs]
        payload = {"mtime_ns": modify_time, "size": size, "messages": messages_dict}
        # TODO: handle error?
        cache.set(key, payload, timeout=self.ttl)
        return messages_dict

    def append_to_cache(
        self, plugin_id: str, user_id: str, messages: list[dict]
    ) -> None:
        key = self._cache_key_tail(plugin_id, user_id)
        try:
            modify_time, size = self.file_stat(plugin_id, user_id)
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
        payload = {"mtime_ns": modify_time, "size": size, "messages": new_messages}

        # TODO: handle error?
        cache.set(key, payload, timeout=self.ttl)

    def file_stat(self, plugin_id: str, user_id: str) -> tuple[int, int]:
        path = self._store.resolve_conversation_path(plugin_id, user_id)
        res = os.stat(path)
        modify_time = res.st_mtime_ns
        size = res.st_size
        return modify_time, size

    @staticmethod
    def _cache_key_tail(plugin_id: str, user_id: str) -> str:
        return f"chattim:tail:{plugin_id}:{user_id}"

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
    def _oldest_in_time_window(messages: list[dict], ts_b: int, ts_e: int) -> int:
        """
        Find the oldest message index in the given time window.
        The message timestamp must border the time window start.
        """
        older_idx: int = -1
        for i in range(len(messages)):
            d = messages[i]
            if not isinstance(d, dict):
                continue
            ts = d.get("timestamp")
            if not isinstance(ts, int):
                continue

            if ts <= ts_b:
                older_idx = i
            if ts_b <= ts <= ts_e:
                if older_idx >= 0:
                    return i
                # There could exist an older message that is not included here
                break
        return -1


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
