from __future__ import annotations

from dataclasses import dataclass, asdict
from typing import Literal, Protocol, Callable, Iterable, Any, cast, AsyncIterator
from enum import Enum
import openai


class ModelErrorKind(Enum):
    Timeout = "Timeout"
    RateLimit = "RateLimit"
    Auth = "Auth"
    NotFound = "NotFound"
    BadRequest = "BadRequest"
    Unknown = "Unknown"


@dataclass(frozen=True)
class ModelError(Exception):
    kind: ModelErrorKind
    """Error kind."""
    description: str | None = None
    """Error description."""
    cause: BaseException | None = None
    """Original exception cause."""
    status: int | None = None
    """Status code of the error."""

    def __str__(self) -> str:
        if self.kind == ModelErrorKind.Unknown and self.cause is not None:
            return (
                f"{str(self.cause)} | Status: {self.status}"
                if self.status
                else str(self.cause)
            )

        return f"{self.kind}: {self.description or ''}"


class ChatModel(Protocol):
    """
    An abstract model class for generating responses.
    Any new chat model must implement this protocol.
    """

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages.

        :param messages: The messages to send to the model.
        :param options: Options for controlling the model response.
        :raises ModelError: If failed to generate a response.
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
        :raises ModelError: If failed to generate a response.
        :return: Response from the model.
        """
        ...

    def get_info(self) -> ModelInfo:
        """Return info about the model.
        :return: `ModelInfo` of the chat model.
        """
        ...

    def close(self) -> None:
        """Close the underlying HTTPX client."""
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
        :raises ModelError: If failed to generate a response.
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
        :raises ModelError: If failed to generate a response.
        :return: Response from the model.
        """
        ...

    def get_info(self) -> ModelInfo:
        """
        Return info about the model.

        :return: `ModelInfo` of the chat model.
        """
        ...

    def close(self) -> None:
        """Close the underlying HTTPX client."""
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
        self._client = openai.OpenAI(api_key=self._api_key, base_url=self._base_url)

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        kwargs = _completion_kwargs(self._info, messages, options, False)
        try:
            res = self._client.chat.completions.create(**kwargs)
        except Exception as e:
            raise _openai_to_model_error(e) from e
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
        try:
            stream = self._client.chat.completions.create(**kwargs)
        except Exception as e:
            raise _openai_to_model_error(e) from e

        # Iterate the message chunks in the stream
        for chunk in stream:
            yield _parse_stream_chunk(chunk)

    def get_info(self) -> ModelInfo:
        return self._info

    def close(self) -> None:
        self._client.close()


class AsyncGenericApiChatModel(AsyncChatModel):
    """
    `AsyncChatModel` implementation for providers that are OpenAI SDK compatible.
    """

    def __init__(self, info: ModelInfo, api_key: str, base_url: str):
        self._info = info
        self._api_key = api_key
        self._base_url = base_url
        self._client = openai.AsyncOpenAI(
            api_key=self._api_key, base_url=self._base_url
        )

    async def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        kwargs = _completion_kwargs(self._info, messages, options, False)
        try:
            res = await self._client.chat.completions.create(**kwargs)
        except Exception as e:
            raise _openai_to_model_error(e) from e
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
            try:
                stream = await self._client.chat.completions.create(**kwargs)
            except Exception as e:
                raise _openai_to_model_error(e) from e

            async for chunk in stream:
                yield _parse_stream_chunk(chunk)

        return gen()

    def get_info(self) -> ModelInfo:
        return self._info

    def close(self) -> None:
        self._client.close()


class OpenAiChatModel(GenericApiChatModel):
    """`ChatModel` implementation for OpenAI models."""

    def __init__(self, info: ModelInfo, api_key: str, base_url: str | None = None):
        super().__init__(info, api_key, _resolve_base_url("openai", base_url))


class AnthropicChatModel(GenericApiChatModel):
    """`ChatModel` implementation for Anthropic models."""

    def __init__(self, info: ModelInfo, api_key: str, base_url: str | None = None):
        super().__init__(info, api_key, _resolve_base_url("anthropic", base_url))


class GoogleChatModel(GenericApiChatModel):
    """`ChatModel` implementation for Google Gemini models."""

    def __init__(self, info: ModelInfo, api_key: str, base_url: str | None = None):
        super().__init__(info, api_key, _resolve_base_url("google", base_url))


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

    def close(self) -> None:
        pass


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

    def close(self) -> None:
        pass


class GenericApiClient:
    """API client for account level information."""

    def __init__(self, provider: Provider, api_key: str, base_url: str | None = None):
        self._provider = provider
        self._api_key = api_key
        self._base_url = _resolve_base_url(provider, base_url)
        self._client = openai.OpenAI(api_key=self._api_key, base_url=self._base_url)

    def list_models(self) -> list[ModelInfo]:
        """Get all the available models from the Model API.

        :raises ModelError: If an error occurred.
        :return: List of all models from the provider API.
        """
        try:
            models = self._client.models.list()
            return [ModelInfo(model_id=m.id, provider=self._provider) for m in models]
        except Exception as e:
            raise _openai_to_model_error(e) from e

    def verify_api_key(self) -> bool:
        """Return True if the key can access the provider API."""
        try:
            self.list_models()
            return True
        except ModelError:
            return False

    def check_model_access(self, model_id: str) -> bool:
        """Check if the API key has access to the given model id.

        :param model_id: The model id to check.
        :return: True if the model is accessible, False otherwise.
        """
        try:
            return any(m.model_id == model_id for m in self.list_models())
        except ModelError:
            return False

    def close(self) -> None:
        """Close the underlying HTTPX client."""
        self._client.close()


@dataclass(frozen=True)
class ModelSpec:
    """Data needed for creating a new `ChatModel` instance."""

    provider: Provider
    model_id: str
    api_key: str
    base_url: str | None = None


# TODO: Let database handle the supported models and retrieving model info.
# Here we should just create the correct model instance from the given spec.
class ModelRegistry:
    """Registry for all the supported models."""

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

    @staticmethod
    def get_supported_providers() -> list[Provider]:
        """Get the list of supported API providers."""
        return list(PROVIDERS.keys())

    def create(self, spec: ModelSpec) -> ChatModel:
        """
        Create a new model from a `ModelSpec`.
        Throws an error if the provider or the model is not supported.

        :param spec: Spec to create the model for.
        :raises ValueError: If the provider or the model is not supported.
                            Or if the provided api key has no access to the model.
        :return: Created ChatModel instance.
        """
        info = self.get_model_info(spec.provider, spec.model_id)
        if info is None:
            raise ValueError(f"Unknown model: {spec.provider}/{spec.model_id}")

        init_fn = PROVIDERS.get(spec.provider)
        if init_fn is None:
            raise ValueError(f"Unknown provider: {spec.provider}")

        # Check if the api key has access to the given model
        client = GenericApiClient(spec.provider, spec.api_key, spec.base_url)
        has_access = client.check_model_access(spec.model_id)
        client.close()
        if not has_access:
            raise ValueError(f"No access to model: {spec.model_id}")

        return init_fn(info, spec.api_key, spec.base_url)


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


def _resolve_base_url(provider: Provider, base_url: str | None = None) -> str:
    """Resolve provider base URL for OpenAI-compatible APIs.

    :param provider: OpenAI-compatible API provider.
    :param base_url: Optional base URL to use.
    :raises ValueError: Provider is unknown.
    :return: OpenAI-compatible API base URL.
    """
    if base_url is not None:
        return base_url.rstrip("/")
    resolved = _DEFAULT_BASE_URL_BY_PROVIDER.get(provider)
    if resolved is None:
        raise ValueError(f"Unknown provider: {provider}")
    return resolved


def _openai_to_model_error(e: BaseException) -> ModelError:
    """Map OpenAI SDK errors into ModelError."""
    if isinstance(
        e,
        (
            openai.AuthenticationError,
            openai.PermissionDeniedError,
        ),
    ):
        return ModelError(ModelErrorKind.Auth, description=str(e), cause=e)
    if isinstance(e, openai.RateLimitError):
        return ModelError(ModelErrorKind.RateLimit, description=str(e), cause=e)
    if isinstance(e, (openai.APITimeoutError, openai.APIConnectionError)):
        return ModelError(ModelErrorKind.Timeout, description=str(e), cause=e)
    if isinstance(e, (openai.NotFoundError, openai.APIConnectionError)):
        return ModelError(ModelErrorKind.NotFound, description=str(e), cause=e)
    if isinstance(e, (openai.BadRequestError, openai.APIConnectionError)):
        return ModelError(ModelErrorKind.BadRequest, description=str(e), cause=e)
    if isinstance(e, openai.APIStatusError):
        status = getattr(e, "status_code", None)
        return ModelError(
            kind=ModelErrorKind.Unknown, description=str(e), cause=e, status=status
        )
    return ModelError(kind=ModelErrorKind.Unknown, description=str(e), cause=e)


Provider = Literal["openai", "anthropic", "google", "dummy"]

ProviderInitFn = Callable[[ModelInfo, str, str | None], ChatModel]
"""Function type for initializing `Model` instances from different providers."""

PROVIDERS: dict[Provider, ProviderInitFn] = {
    "openai": lambda info, key, url: OpenAiChatModel(info, key, url),
    "anthropic": lambda info, key, url: AnthropicChatModel(info, key, url),
    "google": lambda info, key, url: GoogleChatModel(info, key, url),
}
"""All the supported providers."""

_DEFAULT_BASE_URL_BY_PROVIDER: dict[Provider, str] = {
    "openai": "https://api.openai.com/v1",
    "anthropic": "https://api.anthropic.com/v1",
    "google": "https://generativelanguage.googleapis.com/v1beta/openai",
}


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
