from __future__ import annotations

from dataclasses import dataclass, asdict
from typing import Literal, Protocol, Callable, Iterable, Any, cast
from enum import StrEnum


class ModelErrorKind(StrEnum):
    timeout = "timeout"
    rate_limit = "rate_limit"
    auth = "auth"
    unknown = "unknown"


@dataclass(frozen=True)
class ChatModelError(Exception):
    kind: ModelErrorKind
    description: str | None = None
    cause: BaseException | None = None
    """Original exception cause."""

    def __str__(self) -> str:
        return f"{self.kind}: {self.description}"


class ChatModel(Protocol):
    """An abstract model class for generating responses."""

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        ...

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> Iterable[ModelResponseChunk]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.
        """
        ...

    def get_info(self) -> ModelInfo:
        """Return info about the model."""
        ...

    def get_models(self) -> list[ModelInfo]:
        """Get all the available models from the Model API."""
        ...


@dataclass(frozen=True)
class Message:
    """A message sent to the model."""

    Role = Literal["system", "user", "assistant"]

    role: Role
    content: str


@dataclass(frozen=True)
class GenerateOptions:
    """Completion request options."""

    temperature: float | None = 0.2
    """Randomness and creativity of the response. Between 0 and 2."""
    max_tokens: int | None = None
    """An upper bound for the number of tokens that can be generated for a completion."""


@dataclass(frozen=True)
class Usage:
    """Usage statistics for the completion request."""

    completion_tokens: int
    """Number of tokens in the generated completion."""

    prompt_tokens: int
    """Number of tokens in the prompt."""

    total_tokens: int
    """Total number of tokens used in the request (prompt + completion)."""


@dataclass(frozen=True)
class ModelResponse:
    """Completion request response from a model."""

    content: str
    """The full response from the model."""
    usage: Usage | None = None
    """Usage statistics."""


@dataclass(frozen=True)
class ModelResponseChunk:
    """Streaming response chunk."""

    delta: str | None = None
    """Message chunk."""
    usage: Usage | None = None
    """Usage statistics."""
    done: bool = False
    """Is the last message in the stream."""


@dataclass(frozen=True)
class ModelInfo:
    """Information about a model."""

    provider: Provider
    model_id: str
    label: str | None = None
    supports_temperature: bool = False
    supports_streaming: bool = False


class GenericApiChatModel(ChatModel):
    """
    `ChatModel` implementation for providers that are OpenAI SDK compatible.
    """

    def __init__(self, info: ModelInfo, api_key: str, base_url: str):
        # TODO: move or change lib
        try:
            from openai import OpenAI, types, AsyncOpenAI  # type: ignore
        except ModuleNotFoundError as e:
            raise ModuleNotFoundError("Python module `openai` not found.") from e

        self._info = info
        self._api_key = api_key
        self._base_url = base_url
        self._client = OpenAI(api_key=self._api_key, base_url=self._base_url)

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        res = self.create_completion(options=options, messages=messages)
        message_content = res.choices[0].message.content or ""
        usage = OpenAiChatModel.get_usage(res.usage)
        return ModelResponse(content=message_content, usage=usage)

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> Iterable[ModelResponseChunk]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.
        """
        if not self._info.supports_streaming:
            raise ValueError(
                f"Streaming is not supported with model: {self._info.model_id}"
            )

        stream = self.create_completion(options=options, messages=messages, stream=True)

        # Iterate the message chunks in the stream
        for chunk in stream:
            usage = OpenAiChatModel.get_usage(chunk.usage)
            if len(chunk.choices) == 0:
                # No more messages
                yield ModelResponseChunk(usage=usage, done=True)
                continue
            msg = chunk.choices[0]
            message_delta = msg.delta.content
            # Return the partial message
            yield ModelResponseChunk(delta=message_delta, usage=usage)

    def get_info(self) -> ModelInfo:
        """Return info about the model."""
        return self._info

    def get_models(self) -> list[ModelInfo]:
        """Get all the available models from the Model API."""
        client = self._client
        return [
            ModelInfo(model_id=m.id, provider="openai") for m in client.models.list()
        ]

    @staticmethod
    def get_usage(
        usage: Any,
    ) -> Usage | None:
        """Convert completion usage to `Usage`"""
        if not usage:
            return None
        return Usage(
            completion_tokens=usage.completion_tokens,
            prompt_tokens=usage.prompt_tokens,
            total_tokens=usage.total_tokens,
        )

    def create_completion(
        self,
        messages: list[Message],
        options: GenerateOptions,
        stream: bool = False,
    ):
        """Create a completion response from the given messages."""
        client = self._client
        msgs: list[dict[str, str]] = [asdict(m) for m in messages]
        temperature = options.temperature if self._info.supports_temperature else None
        stream_options = (
            {"stream": True, "stream_options": {"include_usage": True}}
            if stream
            else {}
        )
        # TODO: handle errors
        return client.chat.completions.create(
            model=self._info.model_id,
            messages=cast(Any, msgs),
            temperature=temperature,
            max_completion_tokens=options.max_tokens,
            **stream_options,
        )


class OpenAiChatModel(GenericApiChatModel):
    """`ChatModel` implementation for OpenAI models."""

    def __init__(self, info: ModelInfo, api_key: str, base_url: str | None = None):
        _base_url = (base_url or "https://api.openai.com/v1").rstrip("/")
        super().__init__(info, api_key, base_url)


class DummyChatModel(ChatModel):
    """A dummy chat model for testing."""

    def __init__(self, info: ModelInfo):
        self._info = info

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        return ModelResponse(
            content="This is a dummy response",
            usage=Usage(completion_tokens=0, prompt_tokens=0, total_tokens=0),
        )

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> Iterable[ModelResponseChunk]:
        return [
            ModelResponseChunk(delta="This", usage=None, done=False),
            ModelResponseChunk(delta=" is", usage=None, done=False),
            ModelResponseChunk(delta=" a", usage=None, done=False),
            ModelResponseChunk(delta=" dummy", usage=None, done=False),
            ModelResponseChunk(delta=" response", usage=None, done=False),
            ModelResponseChunk(
                delta=None,
                usage=Usage(completion_tokens=0, prompt_tokens=0, total_tokens=0),
                done=True,
            ),
        ]

    def get_info(self) -> ModelInfo:
        return self._info

    def get_models(self) -> list[ModelInfo]:
        return []


# TODO: Let database handle the supported models and retrieving model info.
# Here we should just create the correct model instance from the given spec.
class ModelRegistry:
    """Registry for all the supported models."""

    @dataclass(frozen=True)
    class ModelSpec:
        """Data needed for creating a new `ChatModel` instance."""

        provider: Provider
        model_id: str
        api_key: str
        base_url: str | None = None

    def __init__(self, models: dict[Provider, list[ModelInfo]]):
        self._models: dict[Provider, dict[str, ModelInfo]] = {
            provider: {model.model_id: model for model in model_list}
            for provider, model_list in models.items()
        }

    def get_models(self, provider: Provider | None = None) -> dict[str, ModelInfo]:
        """Get all the supported models."""
        if not provider:
            out: dict[str, ModelInfo] = {}
            for models in self._models.values():
                out.update(models)
            return out
        return self._models.get(provider, {})

    def get_model_info(
        self,
        provider: Provider,
        model_id: str,
    ) -> ModelInfo | None:
        """Get model info for a specific model."""
        models = self._models.get(provider, {})
        return models.get(model_id)

    def create(self, spec: ModelSpec) -> ChatModel:
        """
        Create a new model from a `ModelSpec`.
        Throws an error if the provider or the model is not supported.
        """
        info = self.get_model_info(spec.provider, spec.model_id)
        if info is None:
            raise ValueError(f"Unknown model: {spec.provider}/{spec.model_id}")

        init_fn = PROVIDERS.get(spec.provider)
        if init_fn is None:
            raise ValueError(f"Unknown provider: {spec.provider}")
        return init_fn(info, spec.api_key, spec.base_url)


# TODO: add more providers
Provider = Literal["openai", "dummy"]

ProviderInitFn = Callable[[ModelInfo, str, str | None], ChatModel]
"""Function type for initializing `Model` instances from different providers."""

PROVIDERS: dict[Provider, ProviderInitFn] = {
    "openai": lambda info, key, url: OpenAiChatModel(info, key, url),
}
"""All the supported providers."""

# TODO: save in the database
SUPPORTED_MODELS: dict[Provider, list[ModelInfo]] = {
    "openai": [
        ModelInfo(
            provider="openai",
            model_id="gpt-4.1-mini",
            label="GPT-4.1 Mini",
            supports_temperature=True,
            supports_streaming=True,
        ),
    ],
}


def get_dummy_model() -> ChatModel:
    """Create a dummy model for testing."""
    return DummyChatModel(
        ModelInfo(
            provider="dummy",
            model_id="dummy-model-1",
            label="Dummy model",
            supports_streaming=True,
        )
    )
