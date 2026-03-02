from dataclasses import dataclass
from typing import Any, Literal, Protocol

Provider = Literal["openai"]


class Model(Protocol):
    def generate(self, messages, options: dict[str, Any]) -> dict[str, Any]:
        ...


@dataclass(frozen=True)
class ModelInfo:
    provider: Provider
    id: str
    label: str


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
        return self._models.get(provider, [])


# TODO: maybe save in database
SUPPORTED_MODELS: dict[Provider, list[ModelInfo]] = {
    "openai": [],
}
