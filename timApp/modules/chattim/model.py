from __future__ import annotations

import os
from dataclasses import dataclass, asdict
from typing import Any, Literal, Protocol
from openai import OpenAI

Provider = Literal["openai"]


class Model(Protocol):
    """An abstract model class."""

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        ...

    def get_models(self) -> list[Any]:
        """Get all the available models from the Model API."""
        ...


Role = Literal["system", "user"]


@dataclass(frozen=True)
class Message:
    role: Role
    content: str


@dataclass(frozen=True)
class GenerateOptions:
    temperature: float | None = 0.2
    max_tokens: int | None = None
    provider_params: dict[str, Any] | None = None


@dataclass(frozen=True)
class Usage:
    completion_tokens: int
    """Number of tokens in the generated completion."""

    prompt_tokens: int
    """Number of tokens in the prompt."""

    total_tokens: int
    """Total number of tokens used in the request (prompt + completion)."""


@dataclass(frozen=True)
class ModelResponse:
    text: str
    usage: Usage | None = None


@dataclass(frozen=True)
class ModelInfo:
    provider: Provider
    id: str
    label: str
    supports_temperature: bool = True


@dataclass(frozen=True)
class ModelSpec:
    provider: Provider
    model: str
    api_key: str | None = None
    base_url: str | None = None


class ModelRegistry:
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
            if m.id == model_id:
                return m
        raise ValueError(f"Unsupported model: {provider}:{model_id}")

    def create(self, spec: ModelSpec) -> Model:
        """
        Create a new model from a ModelSpec.
        Throws an error if the model is not supported.
        """
        info = self.get_model_info(spec.provider, spec.model)
        if spec.provider == "openai":
            return OpenAiModel(spec, info)
        # TODO: add more providers
        raise ValueError(f"Unknown provider: {spec.provider}")


# TODO: maybe save in the database
SUPPORTED_MODELS: dict[Provider, list[ModelInfo]] = {
    "openai": [
        ModelInfo(
            provider="openai",
            id="gpt-4.1-mini",
            label="GPT-4.1 Mini",
            supports_temperature=True,
        ),
    ],
}


class OpenAiModel(Model):
    def __init__(self, spec: ModelSpec, info: ModelInfo):
        self._spec = spec
        self._info = info
        self._api_key = spec.api_key or os.environ.get("OPENAI_API_KEY")
        if not self._api_key:
            raise ValueError("OPENAI_API_KEY not set")
        self._base_url = (spec.base_url or "https://api.openai.com/v1").rstrip("/")

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
        """Generate a model response from the given messages."""
        if options.temperature is not None and not self._info.supports_temperature:
            raise ValueError(
                f"Model does not support option 'temperature': {self._spec.model} "
            )
        client = OpenAI(api_key=self._api_key, base_url=self._base_url)
        msgs: list[dict[str, str]] = [asdict(m) for m in messages]

        res = client.chat.completions.create(model=self._spec.model, messages=msgs)
        return ModelResponse(
            text=res.choices[0].message.content or "",
            usage=Usage(
                completion_tokens=res.usage.completion_tokens,
                prompt_tokens=res.usage.prompt_tokens,
                total_tokens=res.usage.total_tokens,
            ),
        )

    def get_models(self) -> list[Any]:
        """Get all the available models from the Model API."""
        client = OpenAI(api_key=self._api_key, base_url=self._base_url)
        return client.models.list().data
