from __future__ import annotations

from dataclasses import dataclass, asdict
from typing import Literal, Protocol, Callable, Iterable, Any, cast, AsyncIterator
from enum import StrEnum
from openai import OpenAI, AsyncOpenAI


class ModelErrorKind(StrEnum):
    timeout = "timeout"
    rate_limit = "rate_limit"
    auth = "auth"
    unknown = "unknown"


@dataclass(frozen=True)
class ChatModelError(Exception):
    kind: ModelErrorKind
    """Error kind."""
    description: str | None = None
    """Error description."""
    cause: BaseException | None = None
    """Original exception cause."""

    def __str__(self) -> str:
        return f"{self.kind}: {self.description}"


class ChatModel(Protocol):
    """
    An abstract model class for generating responses.
    Any new chat model must implement this protocol.
    """

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """
        Generate a model response from the given messages.

        :param messages: The messages to send to the model.
        :param options: Options for controlling the model response.
        :return: Response from the model.
        """
        ...

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> Iterable[ModelResponseChunk]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.

        :param messages: The messages to send to the model.
        :param options: Options for controlling the model response.
        :return: Response from the model.
        """
        ...

    def get_info(self) -> ModelInfo:
        """
        Return info about the model.

        :return: `ModelInfo` of the chat model.
        """
        ...

    # TODO: needed?
    def get_models(self) -> list[ModelInfo]:
        """Get all the available models from the Model API."""
        ...


class AsyncChatModel(Protocol):
    """
    An abstract async model class for generating responses.
    Any new async chat model must implement this protocol.
    """

    async def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """
        Generate a model response from the given messages.

        :param messages: The messages to send to the model.
        :param options: Options for controlling the model response.
        :return: Response from the model.
        """
        ...

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> AsyncIterator[ModelResponseChunk]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.

        :param messages: The messages to send to the model.
        :param options: Options for controlling the model response.
        :return: Response from the model.
        """
        ...

    def get_info(self) -> ModelInfo:
        """
        Return info about the model.

        :return: `ModelInfo` of the chat model.
        """
        ...

    async def get_models(self) -> list[ModelInfo]:
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


def _convert_usage(usage: Any) -> Usage | None:
    """
    Convert completion usage from the API response to `Usage`

    :param usage: The usage dictionary received from the model API.
    :return: `Usage` or None if no usage was in the model response.
    """
    if not usage:
        return None
    return Usage(
        completion_tokens=usage.completion_tokens,
        prompt_tokens=usage.prompt_tokens,
        total_tokens=usage.total_tokens,
    )


def _completion_kwargs(
    info: ModelInfo,
    messages: list[Message],
    options: GenerateOptions,
    stream: bool,
) -> dict[str, Any]:
    """
    Prepare the keyword arguments for a completion request.

    :param info: Information about the model being used.
    :param messages: A list of messages to be sent to the model.
    :param options: Options for generating the completion.
    :param stream: Whether to use streaming for the response.
    :return: A dictionary of keyword arguments for the completion request.
    """
    msgs: list[dict[str, str]] = [asdict(m) for m in messages]
    temperature = options.temperature if info.supports_temperature else None
    stream_options = (
        {"stream": True, "stream_options": {"include_usage": True}} if stream else {}
    )
    return dict(
        model=info.model_id,
        messages=cast(Any, msgs),
        temperature=temperature,
        max_completion_tokens=options.max_tokens,
        **stream_options,
    )


def _parse_completion_response(res: Any) -> ModelResponse:
    """
    Parse the API response from the LLM to `ModelResponse`.

    :param res: The response from the model API.
    :return: Response in `ModelResponse`.
    """
    message_content = res.choices[0].message.content or ""
    usage = _convert_usage(res.usage)
    return ModelResponse(content=message_content, usage=usage)


def _parse_stream_chunk(chunk: Any) -> ModelResponseChunk:
    """
    Parse the streaming API response from the LLM to `ModelResponseChunk`.

    :param chunk: The response chunk from the model API.
    :return: Response in `ModelResponseChunk`.
    """
    usage = _convert_usage(chunk.usage)
    if len(chunk.choices) == 0:
        # No more messages
        return ModelResponseChunk(usage=usage, done=True)
    msg = chunk.choices[0]
    return ModelResponseChunk(delta=msg.delta.content, usage=usage)


class GenericApiChatModel(ChatModel):
    """
    `ChatModel` implementation for providers that are OpenAI SDK compatible.
    """

    def __init__(self, info: ModelInfo, api_key: str, base_url: str):
        self._info = info
        self._api_key = api_key
        self._base_url = base_url
        self._client = OpenAI(api_key=self._api_key, base_url=self._base_url)

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        kwargs = _completion_kwargs(self._info, messages, options, False)
        # TODO: handle errors
        res = self._client.chat.completions.create(**kwargs)
        return _parse_completion_response(res)

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

        kwargs = _completion_kwargs(self._info, messages, options, True)
        # TODO: handle errors
        stream = self._client.chat.completions.create(**kwargs)

        # Iterate the message chunks in the stream
        for chunk in stream:
            yield _parse_stream_chunk(chunk)

    def get_info(self) -> ModelInfo:
        """Return info about the model."""
        return self._info

    def get_models(self) -> list[ModelInfo]:
        """Get all the available models from the Model API."""
        return [
            ModelInfo(model_id=m.id, provider=self._info.provider)
            for m in self._client.models.list()
        ]


class AsyncGenericApiChatModel(AsyncChatModel):
    """
    `AsyncChatModel` implementation for providers that are OpenAI SDK compatible.
    """

    def __init__(self, info: ModelInfo, api_key: str, base_url: str):
        self._info = info
        self._api_key = api_key
        self._base_url = base_url
        self._client = AsyncOpenAI(api_key=self._api_key, base_url=self._base_url)

    async def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        kwargs = _completion_kwargs(self._info, messages, options, False)
        # TODO: handle errors
        res = await self._client.chat.completions.create(**kwargs)
        return _parse_completion_response(res)

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> AsyncIterator[ModelResponseChunk]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.
        """

        async def gen() -> AsyncIterator[ModelResponseChunk]:
            if not self._info.supports_streaming:
                raise ValueError(
                    f"Streaming is not supported with model: {self._info.model_id}"
                )

            kwargs = _completion_kwargs(self._info, messages, options, True)
            # TODO: handle errors
            stream = await self._client.chat.completions.create(**kwargs)

            async for chunk in stream:
                yield _parse_stream_chunk(chunk)

        return gen()

    def get_info(self) -> ModelInfo:
        """Return info about the model."""
        return self._info

    async def get_models(self) -> list[ModelInfo]:
        """Get all the available models from the Model API."""
        models = await self._client.models.list()
        return [
            ModelInfo(model_id=m.id, provider=self._info.provider) async for m in models
        ]


class OpenAiChatModel(GenericApiChatModel):
    """`ChatModel` implementation for OpenAI models."""

    def __init__(self, info: ModelInfo, api_key: str, base_url: str | None = None):
        _base_url = (base_url or "https://api.openai.com/v1").rstrip("/")
        super().__init__(info, api_key, _base_url)


class AnthropicChatModel(GenericApiChatModel):
    """`ChatModel` implementation for Anthropic models."""

    def __init__(self, info: ModelInfo, api_key: str, base_url: str | None = None):
        _base_url = (base_url or "https://api.anthropic.com/v1").rstrip("/")
        super().__init__(info, api_key, _base_url)


class GoogleChatModel(GenericApiChatModel):
    """`ChatModel` implementation for Google Gemini models."""

    def __init__(self, info: ModelInfo, api_key: str, base_url: str | None = None):
        _base_url = (
            base_url or "https://generativelanguage.googleapis.com/v1beta/openai"
        ).rstrip("/")
        super().__init__(info, api_key, _base_url)


class DummyChatModel(ChatModel):
    """A dummy chat model for testing."""

    def __init__(self, info: ModelInfo):
        self._info = info
        self.response = "This is a dummy response"

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        return ModelResponse(content=self.response, usage=Usage(0, 0, 0))

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> Iterable[ModelResponseChunk]:
        for part in _split_keep_left(self.response, " "):
            yield ModelResponseChunk(delta=part)
        yield ModelResponseChunk(delta=None, usage=Usage(0, 0, 0), done=True)

    def get_info(self) -> ModelInfo:
        return self._info

    def get_models(self) -> list[ModelInfo]:
        return []


class DummyAsyncChatModel(AsyncChatModel):
    """A dummy async chat model for testing."""

    def __init__(self, info: ModelInfo):
        self._info = info
        self.response = "This is a dummy response"

    async def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        return ModelResponse(content=self.response, usage=Usage(0, 0, 0))

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> AsyncIterator[ModelResponseChunk]:
        async def gen() -> AsyncIterator[ModelResponseChunk]:
            for part in _split_keep_left(self.response, " "):
                yield ModelResponseChunk(delta=part)
            yield ModelResponseChunk(delta=None, usage=Usage(0, 0, 0), done=True)

        return gen()

    def get_info(self) -> ModelInfo:
        return self._info

    async def get_models(self) -> list[ModelInfo]:
        return []


def _split_keep_left(data: str, d: str) -> list[str]:
    """
    Split the string based on the specified delimiter.
    Keep the delimiter on the left side of each split segment.

    :param data: String to split.
    :param d: The split delimiter.
    :return: Split string in a list.
    """
    parts = [p + d for p in data.split(d) if p]
    if data.endswith(d):
        return parts
    return parts[:-1] + [parts[-1][: -len(d)]] if parts else []


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


Provider = Literal["openai", "anthropic", "google", "dummy"]

ProviderInitFn = Callable[[ModelInfo, str, str | None], ChatModel]
"""Function type for initializing `Model` instances from different providers."""

PROVIDERS: dict[Provider, ProviderInitFn] = {
    "openai": lambda info, key, url: OpenAiChatModel(info, key, url),
    "anthropic": lambda info, key, url: AnthropicChatModel(info, key, url),
    "google": lambda info, key, url: GoogleChatModel(info, key, url),
}
"""All the supported providers."""

# TODO: save in the database
SUPPORTED_MODELS: dict[Provider, list[ModelInfo]] = {
    "openai": [
        ModelInfo(
            provider="openai",
            model_id="gpt-4.1-nano",
            label="GPT-4.1 Nano",
            supports_temperature=True,
            supports_streaming=True,
        ),
        ModelInfo(
            provider="openai",
            model_id="gpt-4.1-mini",
            label="GPT-4.1 Mini",
            supports_temperature=True,
            supports_streaming=True,
        ),
        ModelInfo(
            provider="openai",
            model_id="gpt-4o-mini",
            label="GPT-4o Mini",
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
