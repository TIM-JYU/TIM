from dataclasses import dataclass

from typing import Optional


@dataclass
class DocRenderResult:
    head_html: str
    content_html: str
    allowed_to_cache: bool
    override_theme: Optional[str]
