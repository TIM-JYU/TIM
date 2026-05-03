from __future__ import annotations

import os
import json
import time
from dataclasses import dataclass, asdict
from collections import deque
from timApp.util.flask.cache import cache
from typing import BinaryIO, Iterator, Any, Callable
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
        message_dict = dict(d)
        if isinstance(usage, dict):
            try:
                message_dict["usage"] = Usage(**usage)
            except TypeError:
                message_dict["usage"] = None
        return ChatMessage(**message_dict)

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

    def get_history(
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
        to_skip = max(offset, 0)

        # Check if the needed messages could be in the cache
        if last_n is not None and last_n + to_skip <= self.tail_len:
            tail_needed = last_n + to_skip
            cached = self._get_from_cache(plugin_id, user_id) or self._update_cache(
                plugin_id, user_id
            )
            start = -tail_needed
            end = None if to_skip == 0 else -to_skip
            if cached is not None and len(cached) >= tail_needed:
                raw_slice = cached[start:end]
                return self._parse_raw_messages(raw_slice)

        return self._store.load_messages(plugin_id, user_id, last_n, to_skip) or []

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
            oldest_index, newest_index = self._find_time_window_edges(
                cached, ts_b, ts_e, ts_begin is not None, ts_end is not None
            )
            end = newest_index + 1 if newest_index >= 0 else 0
            if ts_begin is None and len(cached[:end]) < max_messages:
                # Cache doesn't have enough older messages
                oldest_index = -1

            if oldest_index >= 0 and newest_index >= 0:
                cached_slice = cached[oldest_index:end]
                start = None if len(cached_slice) < max_messages else -max_messages
                return self._parse_raw_messages(cached_slice[start:])

        return self._store.load_messages_time_window(**kwargs) or []

    def _get_from_cache(self, plugin_id: str, user_id: str) -> list[dict] | None:
        """Get the message list tail from the cache if it exists and is up to date."""
        key = self._cache_key_tail(plugin_id, user_id)
        cache_result = cache.get(key)
        if not isinstance(cache_result, dict):
            return None

        cache_modify = cache_result.get("modify_time_ns")
        cache_size = cache_result.get("size")
        if not isinstance(cache_modify, int) or not isinstance(cache_size, int):
            return None
        try:
            modify_time, size = self._file_stat(plugin_id, user_id)
        except FileNotFoundError:
            return None
        if cache_modify != modify_time or cache_size != size:
            return None

        messages = cache_result.get("messages")
        if not isinstance(messages, list):
            return None

        return messages

    def _update_cache(self, plugin_id: str, user_id: str) -> list[dict] | None:
        """Update the cache with the new conversation tail."""
        try:
            modify_time, size = self._file_stat(plugin_id, user_id)
        except FileNotFoundError:
            return None

        messages = self._store.load_messages(plugin_id, user_id, self.tail_len, 0) or []
        messages_dict = [m.to_dict() for m in messages]
        payload = self._cache_payload_tail(modify_time, size, messages_dict)
        key = self._cache_key_tail(plugin_id, user_id)
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

        cache_result = cache.get(key)
        cached_tail: list[dict] = []
        if isinstance(cache_result, dict):
            cached_messages = cache_result.get("messages")
            cached_tail = cached_messages if isinstance(cached_messages, list) else []
        new_messages = (cached_tail + messages)[-self.tail_len :]
        payload = self._cache_payload_tail(modify_time, size, new_messages)
        self._set_cache(key, payload)

    def _file_stat(self, plugin_id: str, user_id: str) -> tuple[int, int]:
        """Get the modification time and size of the conversation file."""
        path = self._store.resolve_conversation_path(plugin_id, user_id)
        result = os.stat(path)
        return result.st_mtime_ns, result.st_size

    def _set_cache(self, key: str, value: Any) -> None:
        cache.set(key, value, timeout=self.ttl)

    @staticmethod
    def _cache_key_tail(plugin_id: str, user_id: str) -> str:
        return f"chattim:tail:{plugin_id}:{user_id}"

    @staticmethod
    def _cache_payload_tail(
        modify_time_ns: int, size: int, messages: list[dict]
    ) -> dict:
        return {"modify_time_ns": modify_time_ns, "size": size, "messages": messages}

    @staticmethod
    def _parse_raw_messages(raw_messages: list[dict]) -> list[ChatMessage]:
        out: list[ChatMessage] = []
        for message in raw_messages:
            if not isinstance(message, dict):
                continue
            try:
                out.append(ChatMessage.from_dict(message))
            except TypeError:
                continue
        return out

    @staticmethod
    def _find_time_window_edges(
        messages: list[dict],
        ts_begin: int,
        ts_end: int,
        find_border_begin: bool = False,
        find_border_end: bool = False,
    ) -> tuple[int, int]:
        """
        Find the edges of the time window from the message list.
        Returns valid indexes only if the whole time window is included.
        Assumes that the messages are in chronological order.

        :param messages: List of messages to find from.
        :param ts_begin: Beginning of the time window in milliseconds.
        :param ts_end: Ending of the time window in milliseconds.
        :param find_border_begin: Whether to find the start border of the time window.
        :param find_border_end: Whether to find the end border of the time window.
        :return: Tuple containing the start and end index in the list.
        """
        oldest_index: int = -1
        newest_index: int = -1
        oldest_found: bool = False
        newest_found: bool = False

        if ts_begin > ts_end:
            return oldest_index, newest_index

        def get_ts(dict_maybe: dict, found: bool) -> int | None:
            if found:
                return None
            if not isinstance(dict_maybe, dict):
                return None
            ts_any = dict_maybe.get("timestamp")
            if not isinstance(ts_any, int):
                return None
            return int(ts_any)

        def check_edge(
            message_ts: int,
            find_border: bool,
            current_index: int,
            latest_index: int,
            is_correct_side: Callable[[int], bool],
        ) -> tuple[bool, int]:
            """Check if the message timestamp is the edge of the time window."""
            if is_correct_side(message_ts):
                latest_index = current_index
            if ts_begin <= message_ts <= ts_end:
                if not find_border or latest_index >= 0:
                    return True, current_index  # Found the desired edge
                else:
                    # There could exist a message that is not included
                    raise ValueError
            elif latest_index == current_index:
                return False, current_index
            else:
                raise ValueError  # Message on wrong side of time window

        # Find edges
        for i in range(len(messages)):
            # Check start border
            ts_begin_message = get_ts(messages[i], oldest_found)
            if ts_begin_message is not None:
                check_side = lambda _ts: _ts <= ts_begin
                try:
                    oldest_found, oldest_index = check_edge(
                        ts_begin_message, find_border_begin, i, oldest_index, check_side
                    )
                except ValueError:
                    break

            # Check end border
            index_end = len(messages) - i - 1
            ts_end_message = get_ts(messages[index_end], newest_found)
            if ts_end_message is not None:
                check_side = lambda _ts: _ts >= ts_end
                try:
                    newest_found, newest_index = check_edge(
                        ts_end_message,
                        find_border_end,
                        index_end,
                        newest_index,
                        check_side,
                    )
                except ValueError:
                    break

            if newest_found and oldest_found:
                # Time window exists in the messages list
                return oldest_index, newest_index

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

        json_messages: list[dict] = []
        data: list[str] = []
        for message in messages:
            try:
                message_dict = message.to_dict()
                data.append(json.dumps(message_dict) + "\n")
                json_messages.append(message_dict)
            except TypeError:
                continue

        with open(file_path, "a", encoding="utf-8") as f:
            f.writelines(data)

        return json_messages

    def load_messages(
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
                        message = self._parse_message_line(line)
                        if message is not None:
                            out.append(message)
                    return out[:-to_skip]

            # Read from the end without reading the whole file.
            out_reversed: list[ChatMessage] = []
            with open(file_path, "rb") as f:
                for line in self._iter_lines_reverse(f):
                    if to_skip > 0:
                        to_skip -= 1
                        continue
                    message = self._parse_message_line(line)
                    if message is None:
                        continue
                    out_reversed.append(message)
                    if len(out_reversed) >= last_n:
                        break
            out_reversed.reverse()
            return out_reversed

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
            out_reversed: list[ChatMessage] = []
            chronological = True
            last_ts: int | None = None

            with open(file_path, "rb") as f:
                for line in self._iter_lines_reverse(f):
                    if len(out_reversed) >= max_messages:
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

                    out_reversed.append(msg)

            out_reversed.reverse()
            return out_reversed

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
            message_json = json.loads(line)
            if not isinstance(message_json, dict):
                return None
            return ChatMessage.from_dict(message_json)
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
            buffer_bytes = b"".join(buffer)
            buffer.clear()
            if buffer_bytes.endswith(b"\r"):
                buffer_bytes = buffer_bytes[:-1]
            if not buffer_bytes:
                return None
            return buffer_bytes.decode("utf-8", errors="replace")

        while pos > 0:
            # Read a chunk
            read_size = chunk_size if pos >= chunk_size else pos
            pos -= read_size
            file.seek(pos, os.SEEK_SET)
            chunk = file.read(read_size)
            if not chunk:
                break

            # Process the chunk and yield lines if there are any
            chunk_pos: int = len(chunk)
            while chunk_pos > 0:
                nl = chunk.rfind(b"\n", 0, chunk_pos)
                if nl < 0:
                    # The line is either too big for one chunk or cut
                    buffer.appendleft(chunk[:chunk_pos])
                    break

                # Combine the start of the line with possible pending line end
                line_start = chunk[nl + 1 : chunk_pos]
                if line_start:
                    buffer.appendleft(line_start)
                line = emit_pending()
                if line is not None:
                    yield line

                chunk_pos = nl

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
