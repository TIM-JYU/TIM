from __future__ import annotations

import os
import json
from dataclasses import dataclass, asdict
from typing import Callable
from .model import Message, Usage


@dataclass
class ChatMessage:
    """Conversation message saved on the disk."""

    content: str
    """Content of the message."""
    timestamp: int
    """Timestamp of when the message was sent in seconds."""
    role: Message.Role
    """Role of the message sender."""
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
        if isinstance(usage, dict):
            try:
                d = dict(d)
                d["usage"] = Usage(**usage)
            except TypeError:
                d = dict(d)
                d["usage"] = None
        return ChatMessage(**d)


class ConversationManager:
    """Manages conversation histories."""

    store: ConversationStore

    # TODO: convo history in mem
    def __init__(self, root_dir: str):
        # NOTE: use FILES_PATH as the root_dir:
        # from timApp.timdb.dbaccess import get_files_path
        # TODO: change root path naming
        root_path = os.path.join(root_dir, "history", "chattim")
        # TODO: keep a cache for recent conversations?
        self.store = ConversationStore(root_path)

    def append_messages(
        self, plugin_id: str, user_id: str, messages: list[ChatMessage]
    ):
        """
        Append messages to the conversation history of the user.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param messages: A list of messages to append to the file.
        """
        # TODO: update cache we have one
        self.store.append_messages(plugin_id, user_id, messages)

    def get_history_n(
        self, plugin_id: str, user_id: str, last_n: int | None = None
    ) -> list[ChatMessage]:
        """
        Return the last N messages from the history of the specified conversation.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param last_n: Last N messages to return or all if None.
        :return: List of `ChatMessage` objects in the conversation.
        """
        # TODO: check if in memory
        messages = self.store.load_messages_n(plugin_id, user_id, last_n)
        return messages or []

    def get_history_time_window(
        self, plugin_id: str, user_id: str, ts_begin: int, ts_end: int
    ) -> list[ChatMessage] | None:
        """
        Return the message from the history of the specified conversation.
        Only includes the messages inside the time window.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param ts_begin: Timestamp of the beginning of the time window in seconds.
        :param ts_end: Timestamp of the end of the time window in seconds.
        :return: List of `ChatMessage` objects or None if no history.
        """
        # TODO: check if in memory
        messages = self.store.load_messages_time_window(
            plugin_id, user_id, ts_begin, ts_end
        )
        return messages or []


class ConversationStore:
    """Handles disk IO for storing the conversations."""

    root_path: str

    def __init__(self, root_path: str):
        self.root_path = root_path

    def append_messages(
        self, plugin_id: str, user_id: str, messages: list[ChatMessage]
    ):
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
        self, plugin_id: str, user_id: str, last_n: int | None = None
    ) -> list[ChatMessage] | None:
        """
        Loads messages from disk if the conversation exists.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param last_n: Last N messages to return or all if None.
        :return: List of `ChatMessage` objects or None if no history.
        """
        if last_n is not None and last_n <= 0:
            return []
        file_path = self.resolve_conversation_path(plugin_id, user_id)
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                # TODO: last_n read can be optimized for large files
                lines = f.readlines()[-last_n:] if last_n is not None else f.readlines()
                return self.parse_messages(lines)

        except FileNotFoundError:
            return None

    def load_messages_time_window(
        self, plugin_id: str, user_id: str, ts_begin: int, ts_end: int
    ) -> list[ChatMessage] | None:
        """
        Loads messages from disk if the conversation exists.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param ts_begin: Timestamp of the beginning of the time window in seconds.
        :param ts_end: Timestamp of the end of the time window in seconds.
        :return: List of `ChatMessage` objects or None if no history.
        """
        if ts_end <= ts_begin:
            return []
        file_path = self.resolve_conversation_path(plugin_id, user_id)
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                # TODO: Can be optimized for large files. Start reading from the end.
                # Don't read all the lines before parsing
                lines = f.readlines()
                ts_filter: Callable[[ChatMessage], bool] = (
                    lambda msg: ts_begin >= msg.timestamp >= ts_begin
                )
                return self.parse_messages(lines, ts_filter)

        except FileNotFoundError:
            return None

    @staticmethod
    def parse_messages(
        lines: list[str], filter_f: Callable[[ChatMessage], bool] | None = None
    ) -> list[ChatMessage]:
        """
        Parse the messages from JSONL to `ChatMessage` objects.

        :param lines: Lines from the JSONL file.
        :param filter_f: Filter function to validate the chat message.
        :return: List of `ChatMessage` objects.
        """
        out: list[ChatMessage] = []
        for line in lines:
            line = line.strip()
            if not line:
                continue
            try:
                d = json.loads(line)
                if not isinstance(d, dict):
                    continue
                chat_message = ChatMessage.from_dict(d)

                if filter_f is not None and filter_f(chat_message):
                    out.append(chat_message)
            except (json.JSONDecodeError, TypeError, ValueError):
                continue
        return out

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
