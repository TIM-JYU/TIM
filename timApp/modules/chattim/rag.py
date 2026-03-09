from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable
from model import (
    ChatModel,
    GenerateOptions,
    Message,
    ModelResponse,
    ModelResponseChunk,
    ModelRegistry,
    SUPPORTED_MODELS,
    get_dummy_model,
)
from enum import Enum

_DEFAULT_SYSTEM_PROMPT_RETRIEVE = ""
_DEFAULT_SYSTEM_PROMPT_CREATIVE = ""

# TODO: remove
registry = ModelRegistry(SUPPORTED_MODELS)


@dataclass
class UserPrompt:
    user_id: str
    content: str


class RagMode(Enum):
    RETRIEVE = 1
    CREATIVE = 2


@dataclass
class RagOptions:
    top_k: int = 5
    """Number of best matches to fetch from the index material."""
    max_history_len: int = 10
    """Maximum amount of previous messages to include in the prompt."""
    # max_context_tokens: int = 200_000


class Rag:
    model: ChatModel
    options: RagOptions
    mode: RagMode
    # TODO:
    # Need reference to PluginCore/indexer/database
    # or callbacks to functions to get context, user chat history

    def __init__(
        self,
        options: RagOptions = RagOptions(),
        mode: RagMode = RagMode.RETRIEVE,
        model_spec: ModelRegistry.ModelSpec | None = None,
    ):
        self.options = options
        self.mode = mode
        # TODO: modify model creation
        self.model = registry.create(model_spec) if model_spec else get_dummy_model()

    def answer(self, user_prompt: UserPrompt) -> Iterable[ModelResponseChunk]:
        """Give an answer to the user using the model."""
        # TODO: append chat history here or in the caller?
        messages = self.build_prompt(user_prompt)
        # TODO: simplify?
        try:
            if self.model.get_info().supports_streaming:
                return self.model.generate_stream(messages, GenerateOptions())
            res: ModelResponse = self.model.generate(messages, GenerateOptions())
        except Exception as e:
            # TODO: better error handling
            print("error(RAG): ", str(e))
            return []
        # TODO: answer post processing
        # Include the urls/document ids?
        return [ModelResponseChunk(delta=res.content, usage=res.usage, done=True)]

    def build_prompt(self, user_prompt: UserPrompt) -> list[Message]:
        """Build the message list to send to the model."""
        system_msg: Message = self.system_message()
        history: list[Message] = self.get_history(user_prompt.user_id)
        context: str = self.build_context(user_prompt.content)
        context_msg: Message = Message(
            # TODO: change role?
            role="user",
            content=f"<CONTEXT> {context} </CONTEXT>",
        )
        user_msg: Message = Message(role="user", content=user_prompt.content)
        prompt: list[Message] = [system_msg]
        prompt.extend(history)
        prompt.append(context_msg)
        prompt.append(user_msg)
        return prompt

    def system_message(self) -> Message:
        """Initialize the system message."""
        # TODO: optimize
        if self.mode == RagMode.RETRIEVE:
            msg = _DEFAULT_SYSTEM_PROMPT_RETRIEVE
        elif self.mode == RagMode.CREATIVE:
            msg = _DEFAULT_SYSTEM_PROMPT_CREATIVE
        else:
            msg = ""
        return Message(role="system", content=msg)

    def get_history(self, user_id: str) -> list[Message]:
        """Fetch the current chat history of the user."""
        # TODO: get history with callback or somehow else
        # Trim history
        # Include only user messages or also the assistant responses?
        # Summarize earlier history? Configurable option for summarizing?
        return []

    def build_context(self, msg: str) -> str:
        """Build the context to add to the message."""
        # TODO: format the context
        context = self.find_similar_context(msg)
        return context

    def find_similar_context(self, msg: str) -> str:
        """Find similar context from the vector index."""
        # TODO: callback to use embeddings
        # Change return type to include more info like urls
        return ""
