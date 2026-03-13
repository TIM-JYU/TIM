from __future__ import annotations

import os
from model import Message
from dataclasses import dataclass


@dataclass
class ChatMessage:
    role: Message.Role
    timestamp: int
    content: str
    # usage: Usage | None = None


# TODO: Do we need other metadata associated with a conversation
# Or save a convo as a file instead
#
# <tim_files>
#  <logs>
#    <chattim/something>
#      <plugin_id>
#        <user_id>
#          <conversation_id>
#            messages.jsonl
#            meta.json


class ConversationLogger:
    root_path: str

    def __init__(self, root_path: str):
        if not os.path.exists(root_path):
            os.mkdir(root_path)
        self.root_path = root_path

    def append_chat(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
        message: Message,
    ):
        # TODO: maybe make a new message type to include usage
        # Append only jsonl file
        pass

    def get_history(self, plugin_id: str, user_id: str, conversation_id: str):
        pass

    def get_user_conversations(self, plugin_id: str, user_id: str):
        pass

    def create_conversation(self, plugin_id: str, user_id: str):
        pass

    def resolve_conversation_path(
        self,
        plugin_id: str,
        user_id: str,
        conversation_id: str,
    ):
        pass
