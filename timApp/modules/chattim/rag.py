from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable
from timApp.modules.chattim.model import (
    ChatModel,
    GenerateOptions,
    Message,
    ModelResponse,
    ModelResponseChunk,
    ModelRegistry,
    ModelInfo,
    ModelSpec,
    Usage,
    SUPPORTED_MODELS,
    Provider,
)
from enum import Enum


_DEFAULT_SYSTEM_PROMPT_RETRIEVE = ""  # TODO: define
_DEFAULT_SYSTEM_PROMPT_CREATIVE = ""  # TODO: define


class RagMode(Enum):
    RETRIEVE = "RETRIEVE"
    CREATIVE = "CREATIVE"


@dataclass
class MessageData:
    user_prompt: str
    context: str
    chat_history: list[Message]
    mode: RagMode
    max_tokens: int


def sum_chunks(iterable: Iterable[ModelResponseChunk]) -> ModelResponseChunk:
    whole_msg: str = ""
    usage: Usage | None = None
    for chunk in iterable:
        if chunk.delta:
            whole_msg += chunk.delta
        if chunk.usage:
            usage = chunk.usage

    return ModelResponseChunk(delta=whole_msg, usage=usage, done=True)


class Rag:
    registry: ModelRegistry = ModelRegistry(SUPPORTED_MODELS)
    models: dict[int, ChatModel] = {}

    def add_model(self, spec: ModelSpec, identifier: int):
        """
        Model spec need not specify the base_url.
        Note that if model with identifier exists it is overwritten.
        :param spec: Model is built according to this spec
        :param identifier: identifier of the model, used for answer calls etc
        Raises:
            ValueError: If the provider or model is unknown
        """

        model = self.registry.create(spec)
        self.models[identifier] = model

    def remove_model(self, identifier: int):
        """
        Model spec need not specify the base_url.
        Nothing is done if model with identifier doesn't exist.
        :param identifier: identifier of the model
        Raises:
            ValueError: If the provider or model is unknown
        """
        if identifier in self.models:
            del self.models[identifier]

    def model_exists(self, identifier: int) -> bool:
        if identifier in self.models:
            return True
        return False

    def answer(
        self, request_data: MessageData, identifier: int
    ) -> Iterable[ModelResponseChunk]:
        """
        Give an answer to the user using the model.
        :param request_data: Information for the prompt
        :param identifier: identifier of the model to be used

        Raises:
            ValueError: If the model isn't created
        """

        if identifier not in self.models:
            raise KeyError(f"Key '{identifier}' not found in the dictionary")

        model = self.models[identifier]
        messages = self.build_prompt(request_data)

        # TODO: simplify?
        try:
            if model.get_info().supports_streaming:
                return model.generate_stream(messages, GenerateOptions())
            res: ModelResponse = model.generate(messages, GenerateOptions())
        except Exception as e:
            # TODO: better error handling
            print("error(RAG): ", str(e))
            return []
        # TODO: answer post processing
        # Include the urls/document ids?
        return [ModelResponseChunk(delta=res.content, usage=res.usage, done=True)]

    def get_supported_models(
        self, provider: Provider | None = None
    ) -> dict[str, ModelInfo]:
        """
        Get all supported models. Example:
        "gpt-4.1-mini": [
            ModelInfo(
                provider="openai",
                model_id="gpt-4.1-mini",
                label="GPT-4.1 Mini",
                supports_temperature=True,
                supports_streaming=True,
            ),
        ],
        :return:
        """
        return self.registry.get_models(provider)

    def build_prompt(self, message_data: MessageData) -> list[Message]:
        """Build the message list to send to the model."""
        mode: RagMode = message_data.mode
        system_msg: Message = self.system_message(mode)
        content: str = message_data.user_prompt
        history: list[Message] = message_data.chat_history
        context: str = message_data.context
        context_msg: Message = Message(
            # TODO: change role?
            role="user",
            content=f"<CONTEXT> {context} </CONTEXT>",
        )
        user_msg: Message = Message(role="user", content=content)
        prompt: list[Message] = [system_msg]
        prompt.extend(history)
        prompt.append(context_msg)
        prompt.append(user_msg)
        return prompt

    def system_message(self, mode: RagMode) -> Message:
        """Initialize the system message."""
        # TODO: optimize
        if mode == RagMode.RETRIEVE:
            msg = _DEFAULT_SYSTEM_PROMPT_RETRIEVE
        elif mode == RagMode.CREATIVE:
            msg = _DEFAULT_SYSTEM_PROMPT_CREATIVE
        else:
            msg = ""
        return Message(role="system", content=msg)
