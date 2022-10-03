from dataclasses import dataclass
from datetime import datetime


@dataclass
class ExportedAnswer:
    content: str
    points: int | float | None
    task: str
    time: datetime
    valid: bool
    doc: str
    email: str | None = None
    username: str | None = None
    host: str | None = None
