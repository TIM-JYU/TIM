from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Literal, Protocol

Provider = Literal["openai"]


class Model(Protocol):
    """An abstract model class."""

    def generate(
        self, messages: list[Message], options: GenerateOptions
    ) -> ModelResponse:
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
class ModelResponse:
    text: str
    usage: dict[str, int] | None = None


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
        _ = self.get_model_info(spec.provider, spec.model)
        if spec.provider == "openai":
            # TODO:
            # return OpenAIModel()
            pass
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
