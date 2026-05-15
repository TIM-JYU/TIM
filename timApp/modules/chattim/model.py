from __future__ import annotations

from flask_babel import gettext
from dataclasses import dataclass, asdict
from typing import Literal, Protocol, Callable, Iterable, Any, cast, AsyncIterator
from enum import Enum
from openai import (
    OpenAI,
    AsyncOpenAI,
    AuthenticationError,
    PermissionDeniedError,
    RateLimitError,
    APITimeoutError,
    APIConnectionError,
    NotFoundError,
    BadRequestError,
    APIStatusError,
    types,
)

DEFAULT_COMPLETION_TIMEOUT_S = 60


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

    def text(self) -> str:
        """Return the translated error text."""
        match self.kind:
            case ModelErrorKind.Timeout:
                return gettext("The request timed out. Please try again later.")
            case ModelErrorKind.RateLimit:
                return gettext("Rate limit exceeded. Please wait and try again.")
            case ModelErrorKind.Auth:
                return gettext("Authentication failed.")
            case ModelErrorKind.NotFound:
                return gettext("The requested resource was not found.")
            case ModelErrorKind.BadRequest:
                return gettext("Bad request.")
        return gettext("An error occurred while generating a response.")

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
    ) -> Iterable[ModelResponse]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.

        :param messages: The messages to send to the model.
        :param options: Options for controlling the model response.
        :raises ModelError: If failed to generate a response.
        :return: Response from the model.
        """
        ...

    def get_info(self) -> tuple[Provider, str]:
        """Return info about the model.
        :return: A tuple of the provider name and the model ID.
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
    ) -> AsyncIterator[ModelResponse]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.

        :param messages: The messages to send to the model.
        :param options: Options for controlling the model response.
        :raises ModelError: If failed to generate a response.
        :return: Response from the model.
        """
        ...

    def get_info(self) -> tuple[Provider, str]:
        """Return info about the model.
        :return: A tuple of the provider name and the model ID.
        """
        ...

    async def close(self) -> None:
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
    """
    Completion request response from a model.
    Can be either the full response or a part of the response.
    """

    content: str | None = None
    """The full response from the model."""
    delta: str | None = None
    """A chunk of the response content."""
    usage: Usage | None = None
    """Usage statistics."""
    used_chunks: list[str] | None = None
    """Used chunks of the response content."""


def _convert_usage(usage: types.CompletionUsage | None) -> Usage | None:
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
    model_id: str,
    messages: list[Message],
    options: GenerateOptions,
    stream: bool,
) -> dict[str, Any]:
    """
    Prepare the keyword arguments for a completion request.

    :param model_id: The id of the model being used.
    :param messages: A list of messages to be sent to the model.
    :param options: Options for generating the completion.
    :param stream: Whether to use streaming for the response.
    :return: A dictionary of keyword arguments for the completion request.
    """
    msgs: list[dict[str, str]] = [asdict(m) for m in messages]
    stream_options = (
        {"stream": True, "stream_options": {"include_usage": True}} if stream else {}
    )
    return dict(
        model=model_id,
        messages=cast(Any, msgs),
        temperature=options.temperature,
        max_completion_tokens=options.max_tokens,
        timeout=DEFAULT_COMPLETION_TIMEOUT_S,
        **stream_options,
    )


def _parse_completion_response(res: Any) -> ModelResponse:
    """
    Parse the API response from the LLM to `ModelResponse`.

    :param res: The response from the model API.
    :return: Response in `ModelResponse`.
    """
    if not res or not res.choices:
        return ModelResponse()
    choice = res.choices[0]
    usage = _convert_usage(res.usage)
    return ModelResponse(content=choice.message.content or "", usage=usage)


def _parse_stream_chunk(chunk: Any) -> ModelResponse:
    """
    Parse the streaming API response from the LLM to `ModelResponse`.

    :param chunk: The response chunk from the model API.
    :return: Response in `ModelResponse`.
    """
    usage = _convert_usage(chunk.usage)
    if len(chunk.choices) == 0:
        # No more messages
        return ModelResponse(usage=usage)
    msg = chunk.choices[0]
    return ModelResponse(delta=msg.delta.content, usage=usage)


class GenericApiChatModel(ChatModel):
    """
    `ChatModel` implementation for providers that are OpenAI SDK compatible.
    """

    def __init__(self, provider: Provider, model_id: str, api_key: str, base_url: str):
        self._model_id = model_id
        self._provider = provider
        self._api_key = api_key
        self._base_url = base_url
        self._client = OpenAI(
            api_key=_sdk_api_key(provider, api_key),
            base_url=self._base_url,
            default_headers=_default_headers(provider, api_key),
        )

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        kwargs = _completion_kwargs(self._model_id, messages, options, False)
        try:
            res = self._client.chat.completions.create(**kwargs)
        except Exception as e:
            raise _openai_to_model_error(e) from e
        return _parse_completion_response(res)

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> Iterable[ModelResponse]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.
        """
        kwargs = _completion_kwargs(self._model_id, messages, options, True)
        try:
            stream = self._client.chat.completions.create(**kwargs)
        except Exception as e:
            raise _openai_to_model_error(e) from e

        # Iterate the message chunks in the stream
        for chunk in stream:
            yield _parse_stream_chunk(chunk)

    def get_info(self) -> tuple[Provider, str]:
        """Return info about the model."""
        return self._provider, self._model_id

    def close(self) -> None:
        self._client.close()


class AsyncGenericApiChatModel(AsyncChatModel):
    """
    `AsyncChatModel` implementation for providers that are OpenAI SDK compatible.
    """

    def __init__(self, provider: Provider, model_id: str, api_key: str, base_url: str):
        self._model_id = model_id
        self._provider = provider
        self._api_key = api_key
        self._base_url = base_url
        self._client = AsyncOpenAI(
            api_key=_sdk_api_key(provider, api_key),
            base_url=self._base_url,
            default_headers=_default_headers(provider, api_key),
        )

    async def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        kwargs = _completion_kwargs(self._model_id, messages, options, False)
        try:
            res = await self._client.chat.completions.create(**kwargs)
        except Exception as e:
            raise _openai_to_model_error(e) from e
        return _parse_completion_response(res)

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> AsyncIterator[ModelResponse]:
        """
        Generate a model response from the given messages.
        Uses streaming and returns the response in chunks.
        """

        async def gen() -> AsyncIterator[ModelResponse]:
            kwargs = _completion_kwargs(self._model_id, messages, options, True)
            try:
                stream = await self._client.chat.completions.create(**kwargs)
            except Exception as e:
                raise _openai_to_model_error(e) from e

            async for chunk in stream:
                yield _parse_stream_chunk(chunk)

        return gen()

    def get_info(self) -> tuple[Provider, str]:
        """Return info about the model."""
        return self._provider, self._model_id

    async def close(self) -> None:
        await self._client.close()


class DummyChatModel(ChatModel):
    """A dummy chat model for testing."""

    def __init__(self, model_id: str):
        self._model_id = model_id
        self._provider: Provider = "dummy"
        self.response = "This is a dummy response"

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        return ModelResponse(content=self.response, usage=Usage(0, 0, 0))

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> Iterable[ModelResponse]:
        for part in _split_keep_left(self.response, " "):
            yield ModelResponse(delta=part)
        yield ModelResponse(usage=Usage(0, 0, 0))

    def get_info(self) -> tuple[Provider, str]:
        return self._provider, self._model_id

    def close(self) -> None:
        pass


class DummyAsyncChatModel(AsyncChatModel):
    """A dummy async chat model for testing."""

    def __init__(self, model_id: str):
        self._model_id = model_id
        self._provider: Provider = "dummy"
        self.response = "This is a dummy response"

    async def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        return ModelResponse(content=self.response, usage=Usage(0, 0, 0))

    def generate_stream(
        self, messages: list[Message], options: GenerateOptions
    ) -> AsyncIterator[ModelResponse]:
        async def gen() -> AsyncIterator[ModelResponse]:
            for part in _split_keep_left(self.response, " "):
                yield ModelResponse(delta=part)
            yield ModelResponse(usage=Usage(0, 0, 0))

        return gen()

    def get_info(self) -> tuple[Provider, str]:
        return self._provider, self._model_id

    async def close(self) -> None:
        pass


class GenericApiClient:
    """API client for account level information."""

    def __init__(self, provider: Provider, api_key: str, base_url: str | None = None):
        self._provider = provider
        self._api_key = api_key
        self._base_url = _resolve_base_url(provider, base_url)
        self._client = OpenAI(
            api_key=_sdk_api_key(provider, api_key),
            base_url=self._base_url,
            default_headers=_default_headers(provider, api_key),
        )

    def list_models(self) -> list[str]:
        """Get all the available models from the Model API.

        :raises ModelError: If an error occurred.
        :return: List of all model IDs from the provider API.
        """
        try:
            models = self._client.models.list()
            return [m.id for m in models]
        except Exception as e:
            raise _openai_to_model_error(e) from e

    def verify_api_key(self) -> bool:
        """Return True if the key can access the provider API."""
        try:
            self.list_models()
            return True
        except ModelError as e:
            if e.kind == ModelErrorKind.Auth:
                return False
            raise e

    def check_model_access(self, model_id: str) -> bool:
        """Check if the API key has access to the given model id.

        :param model_id: The model id to check.
        :return: True if the model is accessible, False otherwise.
        """
        try:
            return any(m_id == model_id for m_id in self.list_models())
        except ModelError:
            return False

    @staticmethod
    def create_chat_model(
        provider: Provider,
        model_id: str,
        api_key: str,
        base_url: str | None = None,
    ) -> ChatModel:
        """Create a new chat model.

        :param provider: Provider name of the model.
        :param model_id: The model id of the model to create.
        :param api_key: The API key matching the provider.
        :param base_url: The base URL of the provider.
        :raises ValueError: If the provider or the model is not supported.
                            Or if the provided api key has no access to the model.
        :return: Created ChatModel instance.
        """
        init_fn = PROVIDERS.get(provider)
        if init_fn is None:
            raise ValueError(f"Unknown provider: {provider}")

        # Check if the api key has access to the given model
        client = GenericApiClient(provider, api_key, base_url)
        has_access = client.check_model_access(model_id)
        client.close()
        if not has_access:
            raise ValueError(f"Unknown model or no access: {model_id}")

        return init_fn(provider, model_id, api_key, base_url)

    def close(self) -> None:
        """Close the underlying HTTPX client."""
        self._client.close()


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
    if isinstance(e, (AuthenticationError, PermissionDeniedError)):
        return ModelError(ModelErrorKind.Auth, description=str(e), cause=e)
    if isinstance(e, RateLimitError):
        return ModelError(ModelErrorKind.RateLimit, description=str(e), cause=e)
    if isinstance(e, (APITimeoutError, APIConnectionError)):
        return ModelError(ModelErrorKind.Timeout, description=str(e), cause=e)
    if isinstance(e, (NotFoundError, APIConnectionError)):
        return ModelError(ModelErrorKind.NotFound, description=str(e), cause=e)
    if isinstance(e, (BadRequestError, APIConnectionError)):
        return ModelError(ModelErrorKind.BadRequest, description=str(e), cause=e)
    if isinstance(e, APIStatusError):
        status = getattr(e, "status_code", None)
        return ModelError(
            kind=ModelErrorKind.Unknown, description=str(e), cause=e, status=status
        )
    return ModelError(kind=ModelErrorKind.Unknown, description=str(e), cause=e)


class AuthScheme(str, Enum):
    """Expected API authentication scheme."""

    Bearer = "bearer"
    XApiKey = "x-api-key"


def _auth_scheme(provider: Provider) -> AuthScheme:
    """Return the auth scheme for the given provider."""
    if provider == "anthropic":
        return AuthScheme.XApiKey
    return AuthScheme.Bearer


def _sdk_api_key(provider: Provider, api_key: str) -> str | None:
    return api_key if _auth_scheme(provider) == AuthScheme.Bearer else None


def _default_headers(provider: Provider, api_key: str) -> dict[str, str]:
    """Provider-specific default headers for OpenAI-compatible APIs."""
    if provider == "anthropic":
        return {"X-Api-Key": api_key, "anthropic-version": "2023-06-01"}
    return {}


Provider = Literal["openai", "anthropic", "google", "dummy"]

ProviderInitFn = Callable[[Provider, str, str, str | None], ChatModel]
"""Function type for initializing `ChatModel` instances from different providers.

`ProviderInitFn(provider: Provider, model_id: str, api_key: str, base_url: str | None)`
"""

PROVIDERS: dict[Provider, ProviderInitFn] = {
    "openai": lambda provider, model_id, key, url: GenericApiChatModel(
        provider, model_id, key, _resolve_base_url("openai", url)
    ),
    "anthropic": lambda provider, model_id, key, url: GenericApiChatModel(
        provider, model_id, key, _resolve_base_url("anthropic", url)
    ),
    "google": lambda provider, model_id, key, url: GenericApiChatModel(
        provider, model_id, key, _resolve_base_url("google", url)
    ),
}
"""All the supported providers."""

_DEFAULT_BASE_URL_BY_PROVIDER: dict[Provider, str] = {
    "openai": "https://api.openai.com/v1",
    "anthropic": "https://api.anthropic.com/v1",
    "google": "https://generativelanguage.googleapis.com/v1beta/openai",
}
