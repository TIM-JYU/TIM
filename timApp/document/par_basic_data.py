import json
from dataclasses import dataclass
from typing import Dict


@dataclass
class ParBasicData:
    attrs: Dict[str, str]
    doc_id: int
    hash: str
    id: str
    md: str

    @property
    def attrs_str(self) -> str:
        """Returns the attributes as a JSON string."""
        return json.dumps(self.attrs, sort_keys=True)
