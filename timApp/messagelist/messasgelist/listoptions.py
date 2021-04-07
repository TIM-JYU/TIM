from dataclasses import dataclass
from typing import List


@dataclass
class ListOptions:
    """All options regarding message lists."""
    listname: str
    domain: str
    archive: str
    # archiveType: str
    emails: List[str]
    ownerEmail: str
