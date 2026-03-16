from __future__ import annotations

import dataclasses
import os
import json

# from timApp.modules.chattim.model import Message
from dataclasses import dataclass
from typing import cast
from timApp.defaultconfig import FILES_PATH


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
        self, plugin_id: str, user_id: str, conversation_id: str
    ) -> list[ChatMessage]:
        """Return the history of the specified conversation."""
        # TODO: check if in memory already
        return self.get_history_from_disk(plugin_id, user_id, conversation_id)

    def get_history_from_disk(
        self, plugin_id: str, user_id: str, conversation_id: str
    ) -> list[ChatMessage]:
        """Loads the conversation history from the disk."""
        messages = self.store.load_messages(plugin_id, user_id, conversation_id)
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
        with open(file_path, "a+") as f:
            f.write(json.dumps(msg_dict) + "\n")
        f.close()

    def load_messages(
        self, plugin_id: str, user_id: str, conversation_id: str
    ) -> list[ChatMessage] | None:
        """
        Loads a list of `ChatMessage` from disk if the conversation exists.
        Returns `None` in the case the conversation does not exist.
        """
        file_path = self.resolve_conversation_file_path(
            plugin_id, user_id, conversation_id
        )
        try:
            f = open(file_path, "r")
        except FileNotFoundError as e:
            return None
        messages = [cast(ChatMessage, json.loads(line)) for line in f]
        f.close()
        return messages

    def resolve_conversation_file_path(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
    ) -> str:
        """Resolve the path to the conversation JSONL file."""
        file_name = f"{conversation_id}.jsonl"
        return os.path.join(self.root_path, plugin_id, user_id, file_name)
