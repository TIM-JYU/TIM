from dataclasses import dataclass
from datetime import datetime
from typing import Union, Optional


@dataclass
class ExportedAnswer:
    content: str
    email: str
    points: Union[int, float, None]
    task: str
    time: datetime
    valid: bool
    doc: str
    host: Optional[str] = None
