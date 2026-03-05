from __future__ import annotations

from dataclasses import dataclass, asdict
from typing import Literal, Protocol, Callable
from openai import OpenAI


class ChatModel(Protocol):
    """An abstract model class for generating responses."""

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
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
    usage: Usage | None = None


@dataclass(frozen=True)
class ModelInfo:
    """Information about a model."""

    provider: Provider
    model_id: str
    label: str | None = None
    supports_temperature: bool = False


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
        models = self._models.get(provider, [])
        return models.get(model_id)

    def create(self, spec: ModelSpec) -> ChatModel:
        """
        Create a new model from a `ModelSpec`.
        Throws an error if the model is not supported.
        """
        info = self.get_model_info(spec.provider, spec.model_id)
        if info is None:
            raise ValueError(f"Unknown model: {spec.provider}/{spec.model_id}")

        init_fn = PROVIDERS.get(spec.provider)
        if init_fn is None:
            raise ValueError(f"Unknown provider: {spec.provider}")
        return init_fn(info, spec.api_key, spec.base_url)


class OpenAiModel(ChatModel):
    """`ChatModel` implementation for OpenAI models."""

    def __init__(self, info: ModelInfo, api_key: str, base_url: str | None = None):
        self._info = info
        self._api_key = api_key
        self._base_url = (base_url or "https://api.openai.com/v1").rstrip("/")
        self._client = OpenAI(api_key=self._api_key, base_url=self._base_url)

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        client = self._client
        msgs: list[dict[str, str]] = [asdict(m) for m in messages]
        temperature = options.temperature if self._info.supports_temperature else None

        res = client.chat.completions.create(
            model=self._info.model_id,
            messages=msgs,
            temperature=temperature,
            max_completion_tokens=options.max_tokens,
        )

        message_content = res.choices[0].message.content or ""
        usage = (
            Usage(
                completion_tokens=res.usage.completion_tokens,
                prompt_tokens=res.usage.prompt_tokens,
                total_tokens=res.usage.total_tokens,
            )
            if res.usage
            else None
        )
        return ModelResponse(content=message_content, usage=usage)

    def get_models(self) -> list[ModelInfo]:
        """Get all the available models from the Model API."""
        client = self._client
        return [
            ModelInfo(model_id=m.id, provider="openai") for m in client.models.list()
        ]


# TODO: save in the database
SUPPORTED_MODELS: dict[Provider, list[ModelInfo]] = {
    "openai": [
        ModelInfo(
            provider="openai",
            model_id="gpt-4.1-mini",
            label="GPT-4.1 Mini",
            supports_temperature=True,
        ),
    ],
}

# TODO: add more providers
Provider = Literal["openai"]

ProviderInitFn = Callable[[ModelInfo, str, str | None], ChatModel]

PROVIDERS: dict[Provider, ProviderInitFn] = {
    "openai": lambda info, key, url: OpenAiModel(info, key, url),
}
