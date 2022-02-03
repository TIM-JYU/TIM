from dataclasses import dataclass
from datetime import datetime
from typing import Union, Optional


@dataclass
class ExportedAnswer:
    content: str
    email: str
    points: int | float | None
    task: str
    time: datetime
    valid: bool
    doc: str
    host: str | None = None
