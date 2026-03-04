from __future__ import annotations

from dataclasses import dataclass, asdict
from typing import Any, Literal, Protocol
from openai import OpenAI

Provider = Literal["openai"]


class ChatModel(Protocol):
    """An abstract model class for generating responses."""

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        ...

    def get_models(self) -> list[Any]:
        """Get all the available models from the Model API."""
        ...


@dataclass(frozen=True)
class Message:
    """A message sent to the model."""

    Role = Literal["system", "user"]

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
    label: str
    supports_temperature: bool = True


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
        self._models = models

    def get_models(self, provider: Provider | None = None) -> list[ModelInfo]:
        """Get all the supported models."""
        if not provider:
            out: list[ModelInfo] = []
            for models in self._models.values():
                out.extend(models)
            return out
        return list(self._models.get(provider, []))

    def get_model_info(self, provider: Provider, model_id: str) -> ModelInfo:
        """Get model info for a specific model."""
        for m in self._models.get(provider, []):
            if m.model_id == model_id:
                return m
        raise ValueError(f"Unsupported model: {provider}:{model_id}")

    def create(self, spec: ModelSpec) -> ChatModel:
        """
        Create a new model from a `ModelSpec`.
        Throws an error if the model is not supported.
        """
        info = self.get_model_info(spec.provider, spec.model_id)
        if spec.provider == "openai":
            return OpenAiModel(info, spec.api_key, spec.base_url)
        # TODO: add more providers
        raise ValueError(f"Unknown provider: {spec.provider}")


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
        response_text = res.choices[0].message.content or ""
        return ModelResponse(
            content=response_text,
            usage=Usage(
                completion_tokens=res.usage.completion_tokens,
                prompt_tokens=res.usage.prompt_tokens,
                total_tokens=res.usage.total_tokens,
            ),
        )

    def get_models(self) -> list[Any]:
        """Get all the available models from the Model API."""
        client = self._client
        return client.models.list().data


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
