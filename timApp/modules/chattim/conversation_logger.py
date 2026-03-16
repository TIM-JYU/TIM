from __future__ import annotations

import dataclasses
import os
import json
from dataclasses import dataclass
from timApp.defaultconfig import FILES_PATH

# from timApp.modules.chattim.model import Message


@dataclass
class ChatMessage:
    # role: Message.Role
    role: str
    timestamp: int
    content: str
    # usage: Usage | None = None


class ConversationManager:
    """Manages conversation histories."""

    store: ConversationStore

    # TODO: convo history in mem
    def __init__(self):
        # TODO: change root path naming
        root_path = os.path.join(FILES_PATH, "history", "chattim")
        self.store = ConversationStore(root_path)

    def append_message(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
        message: ChatMessage,
    ):
        """Append a message to the history of the specified conversation."""
        # TODO: update cache
        self.store.append_message(plugin_id, user_id, conversation_id, message)

    def get_history(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
        last_n: int | None = None,
    ) -> list[ChatMessage]:
        """Return the history of the specified conversation."""
        # TODO: check if in memory already
        messages = self.store.load_messages(plugin_id, user_id, conversation_id, last_n)
        return messages or []


class ConversationStore:
    """Handles disk IO for storing the conversations."""

    root_path: str

    def __init__(self, root_path: str):
        self.root_path = root_path

    def append_message(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
        message: ChatMessage,
    ):
        """
        Append one `ChatMessage` to the JSONL file.
        Creates the file path for the conversation file if not found.
        """
        file_path = self.resolve_conversation_file_path(
            plugin_id, user_id, conversation_id
        )
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        msg_dict = dataclasses.asdict(message)
        with open(file_path, "a", encoding="utf-8") as f:
            f.write(json.dumps(msg_dict) + "\n")

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
        file_path = self.resolve_conversation_file_path(
            plugin_id, user_id, conversation_id
        )
        try:
            out: list[ChatMessage] = []

            with open(file_path, "r", encoding="utf-8") as f:
                # TODO: last_n read can be optimized for large files
                lines = f.readlines()[-last_n:] if last_n is not None else f.readlines()
                for line in lines:
                    line = line.strip()
                    if not line:
                        continue
                    d = json.loads(line)
                    if not isinstance(d, dict):
                        continue
                    out.append(ChatMessage(**d))
            return out

        except FileNotFoundError:
            return None

    def resolve_conversation_file_path(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
    ) -> str:
        """Resolve the path to the conversation JSONL file."""
        file_name = f"{conversation_id}.jsonl"
        return os.path.join(self.root_path, plugin_id, user_id, file_name)
