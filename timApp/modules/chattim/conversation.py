from __future__ import annotations

import os
import json
import time
from dataclasses import dataclass, asdict
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

    # TODO: keep a cache for recent conversations?
    def __init__(self, root_dir: str):
        # TODO: change root path naming
        root_path = os.path.join(root_dir, "history", "chattim")
        self._store = ConversationStore(root_path)

    def append_messages(
        self, plugin_id: str, user_id: str, messages: list[ChatMessage]
    ) -> None:
        """
        Append messages to the conversation history of the user.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param messages: A list of messages to append to the file.
        """
        # TODO: update cache if we have one
        self._store.append_messages(plugin_id, user_id, messages)

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
        # TODO: check if in cache
        messages = self._store.load_messages_n(plugin_id, user_id, last_n, offset)
        return messages or []

    def get_history_time_window(
        self,
        plugin_id: str,
        user_id: str,
        ts_begin: int,
        ts_end: int,
        max_messages: int,
    ) -> list[ChatMessage] | None:
        """
        Return the message from the history of the specified conversation.
        Only includes the messages inside the time window.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param ts_begin: Timestamp of the beginning of the time window in milliseconds.
        :param ts_end: Timestamp of the end of the time window in milliseconds.
        :param max_messages: Maximum amount of messages to return in the time window.
        :return: List of `ChatMessage` objects or None if no history.
        """
        # TODO: check if in cache
        messages = self._store.load_messages_time_window(
            plugin_id, user_id, ts_begin, ts_end, max_messages
        )
        return messages or []


class ConversationStore:
    """Handles disk IO for storing the conversations."""

    def __init__(self, root_path: str):
        self.root_path = root_path

    def append_messages(
        self, plugin_id: str, user_id: str, messages: list[ChatMessage]
    ) -> None:
        """
        Append a list of `ChatMessage` objects to the conversation file in order.
        If the conversation file path does not exist, it will be created.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param messages: A list of messages to append to the file.
        """
        file_path = self.resolve_conversation_path(plugin_id, user_id)
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "a", encoding="utf-8") as f:
            for message in messages:
                try:
                    msg_dict = message.to_dict()
                    f.write(json.dumps(msg_dict) + "\n")
                except TypeError:
                    continue

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
        :return: An iterator over the lines in the file in reverse order.
        """
        # Set the read position to file end.
        file.seek(0, os.SEEK_END)
        pos = file.tell()
        buf = b""

        while pos > 0:
            read_size = chunk_size if pos >= chunk_size else pos
            pos -= read_size
            file.seek(pos, os.SEEK_SET)
            chunk = file.read(read_size)
            if not chunk:
                break
            buf = chunk + buf

            parts = buf.split(b"\n")
            # The first line in the chunk can be incomplete.
            buf = parts[0]
            for raw in reversed(parts[1:]):
                if not raw:
                    continue
                if raw.endswith(b"\r"):
                    raw = raw[:-1]
                yield raw.decode("utf-8", errors="replace")

        if buf:
            if buf.endswith(b"\r"):
                buf = buf[:-1]
            yield buf.decode("utf-8", errors="replace")

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
