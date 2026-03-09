from __future__ import annotations

import os
from dataclasses import dataclass
from model import (
    ChatModel,
    GenerateOptions,
    Message,
    ModelResponse,
    ModelResponseChunk,
    ModelRegistry,
    SUPPORTED_MODELS,
)
from enum import Enum

_DEFAULT_SYSTEM_PROMPT_RETRIEVE = ""
_DEFAULT_SYSTEM_PROMPT_CREATIVE = ""


@dataclass
class UserPrompt:
    user_id: str
    content: str


class RagMode(Enum):
    RETRIEVE = 1
    CREATIVE = 2


@dataclass
class RagOptions:
    top_k: int
    max_history_len: int
    max_context_tokens: int


class Rag:
    model: ChatModel
    options: RagOptions
    mode: RagMode
    # TODO:
    # Need reference to PluginCore/indexer/database
    # or callbacks to fetch functions
    # to get context, user chat history

    def __init__(self):
        # TODO: per request or owned in the class?
        # Options should be modifiable per plugin instance
        self.options = RagOptions(
            top_k=5,
            max_history_len=10,
            max_context_tokens=200_000,
        )
        self.mode = RagMode.RETRIEVE
        # TODO: modify model creation
        # `ModelRegistry` is temporary
        # Get the info needed to create the model from the caller
        reg = ModelRegistry(SUPPORTED_MODELS)
        info = SUPPORTED_MODELS.get("openai")[0]
        try:
            api_key = os.getenv("OPENAI_API_KEY")
        except ValueError as e:
            print("Failed to get api_key: ", str(e))
            return
        self.model = reg.create(
            ModelRegistry.ModelSpec(
                provider=info.provider,
                model_id=info.model_id,
                api_key=api_key,
            )
        )

    def answer(self, user_prompt: UserPrompt) -> ModelResponse:
        messages = self.build_prompt(user_prompt)
        print(messages)
        res = self.model.generate(messages, GenerateOptions())
        return res

    def build_prompt(self, user_prompt: UserPrompt) -> list[Message]:
        system_msg: Message = self.system_message()
        history: list[Message] = self.get_history(user_prompt.user_id)
        context: str = self.find_similar_context(user_prompt.content)
        context_msg: Message = Message(
            # TODO: change role to context?
            role="user",
            content=f"CONTEXT: {context}",
        )
        user_msg: Message = Message(role="user", content=user_prompt.content)
        prompt: list[Message] = [system_msg]
        prompt.extend(history)
        prompt.append(context_msg)
        prompt.append(user_msg)
        return prompt

    def system_message(self) -> Message:
        # TODO: optimize
        if self.mode == RagMode.RETRIEVE:
            msg = _DEFAULT_SYSTEM_PROMPT_RETRIEVE
        elif self.mode == RagMode.CREATIVE:
            msg = _DEFAULT_SYSTEM_PROMPT_CREATIVE
        else:
            msg = ""
        return Message(role="system", content=msg)

    def get_history(self, user_id: str) -> list[Message]:
        # TODO: get history with callback or somehow else
        # Trim history
        # Include only user messages or also the assistant responses?
        # Summarize earlier history?
        return []

    def find_similar_context(self, msg: str) -> str:
        # TODO: callback to use embeddings
        # Change return type to include more info like urls
        return ""
