import json
from dataclasses import dataclass

EMPTY_JSON = "{}"


@dataclass
class ParBasicData:
    attrs: dict[str, str]
    doc_id: int
    hash: str
    id: str
    md: str

    @property
    def attrs_str(self) -> str:
        """Returns the attributes as a JSON string."""

        # Performance optimization for avoiding json.dumps call in the most common case.
        if not self.attrs:
            return EMPTY_JSON

        return json.dumps(self.attrs, sort_keys=True)
