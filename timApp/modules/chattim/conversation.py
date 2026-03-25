from __future__ import annotations

import os
import json
from dataclasses import dataclass, asdict
from timApp.modules.chattim.model import Message, Usage
from timApp.defaultconfig import FILES_PATH


@dataclass
class ChatMessage:
    content: str
    """Content of the message."""
    timestamp: int
    """Timestamp of when the message was sent."""
    role: Message.Role
    """Role of the message sender."""
    usage: Usage | None = None
    """Tokens used for generating the message."""

    def to_dict(self) -> dict:
         return asdict(self)

    @staticmethod
    def from_dict(d: dict) -> ChatMessage:
        """Parse `ChatMessage` from `dict`.`"""
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
    def __init__(self):
        # TODO: change root path naming
        # TODO: get the `FILES_PATH` as an argument
        root_path = os.path.join(FILES_PATH, "history", "chattim")
        # TODO: keep a cache for recent conversations?
        self.store = ConversationStore(root_path)

    def append_messages(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
        messages: list[ChatMessage],
    ):
        """
        Append messages to the history of the specified conversation.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param conversation_id: The ID of the conversation.
        :param messages: A list of messages to append to the file.
        :return: None
        """

        # TODO: update cache we have one
        self.store.append_messages(plugin_id, user_id, conversation_id, messages)

    def get_history(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
        last_n: int | None = None,
    ) -> list[ChatMessage]:
        """
        Return the history of the specified conversation.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param conversation_id: The ID of the conversation.
        :param last_n: Last N messages to return or all if None.
        :return: List of `ChatMessage` objects in the conversation.
        """
        # TODO: check if in memory
        messages = self.store.load_messages(plugin_id, user_id, conversation_id, last_n)
        return messages or []

    def user_conversations(self, plugin_id: str, user_id: str) -> list[str]:
        """
        Get all the conversations of the specified user with the plugin.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :return: List of conversation IDs.
        """
        return self.store.user_conversations(plugin_id, user_id)


class ConversationStore:
    """Handles disk IO for storing the conversations."""

    root_path: str

    def __init__(self, root_path: str):
        self.root_path = root_path

    def append_messages(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
        messages: list[ChatMessage],
    ):
        """
        Append a list of `ChatMessage` objects to the conversation file in order.
        If the conversation file path does not exist, it will be created.

        :param plugin_id: The ID of the plugin instance.
        :param user_id: The ID of the user.
        :param conversation_id: The ID of the conversation.
        :param messages: A list of messages to append to the file.
        :return: None
        """
        file_path = self.resolve_conversation_path(plugin_id, user_id, conversation_id)
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "a", encoding="utf-8") as f:
            for message in messages:
                try:
                    msg_dict = message.to_dict()
                    f.write(json.dumps(msg_dict) + "\n")
                except TypeError:
                    continue

    def load_messages(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
        last_n: int | None = None,
    ) -> list[ChatMessage] | None:
        """
        Loads messages from disk if the conversation exists.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param conversation_id: Conversation ID.
        :param last_n: Last N messages to return or all if None.
        :return: List of `ChatMessage` objects or None if no history.
        """
        if last_n is not None and last_n <= 0:
            return []
        file_path = self.resolve_conversation_path(plugin_id, user_id, conversation_id)
        try:
            out: list[ChatMessage] = []

            with open(file_path, "r", encoding="utf-8") as f:
                # TODO: last_n read can be optimized for large files
                lines = f.readlines()[-last_n:] if last_n is not None else f.readlines()
                for line in lines:
                    line = line.strip()
                    if not line:
                        continue
                    try:
                        d = json.loads(line)
                        if not isinstance(d, dict):
                            continue
                        out.append(ChatMessage.from_dict(d))
                    except (json.JSONDecodeError, TypeError, ValueError):
                        continue
            return out

        except FileNotFoundError:
            return None

    def user_conversations(
        self,
        plugin_id: str,
        user_id: str,
    ) -> list[str]:
        """
        Return the conversation IDs of the user.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :return: List of conversation IDs.
        """
        conversations_path = self.resolve_conversation_path(plugin_id, user_id)
        out: list[str] = []
        try:
            for e in os.scandir(conversations_path):
                if not e.is_file():
                    continue
                if not e.name.endswith(".jsonl"):
                    continue
                out.append(os.path.splitext(e.name)[0])
        except FileNotFoundError:
            return []
        return out

    def resolve_conversation_path(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str | None = None,
    ) -> str:
        """
        Resolve the path to the conversation JSONL file.
        Does not verify if the path exists on the disk.

        :param plugin_id: Plugin instance ID.
        :param user_id: User ID.
        :param conversation_id: Conversation ID.
        :return: The file system path to the conversation JSONL file.
        """
        if conversation_id is None:
            return os.path.join(self.root_path, plugin_id, user_id)
        file_name = f"{conversation_id}.jsonl"
        return os.path.join(self.root_path, plugin_id, user_id, file_name)
