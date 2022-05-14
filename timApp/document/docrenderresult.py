from dataclasses import dataclass


@dataclass
class DocRenderResult:
    head_html: str
    content_html: str
    allowed_to_cache: bool
    override_theme: str | None
    hide_readmarks: bool = False
