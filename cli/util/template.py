import io
import json
from pathlib import Path
from typing import Any, Optional, Dict

from cli.util.logging import log_debug


class PyTemplate:
    delimiter: str = "$"
    start_delimiter = "{"
    end_delimiter = "}"

    def __init__(self, template_name: str) -> None:
        template_path = Path.cwd() / "cli" / "templates" / template_name
        self.template = template_path.read_text(encoding="utf-8")

    @staticmethod
    def _create_context(ctx: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        if ctx is None:
            ctx = {}

        def _jsonify(value: Any) -> str:
            return json.dumps(value)

        def _partial(dir_path: str, name: str) -> str:
            return PyTemplate(f"{dir_path}/{name}").render(ctx)

        full_ctx = {
            **ctx,
            "jsonify": _jsonify,
            "partial": _partial,
        }
        return full_ctx

    def render(self, ctx: Optional[Dict[str, Any]] = None) -> str:
        full_ctx = self._create_context(ctx)

        captures: list[tuple[int, int]] = []
        capture_start = -1
        found_delimiter = False
        delim_count = 0
        for i, char in enumerate(self.template):
            if char == self.delimiter and capture_start < 0:
                found_delimiter = True
            elif char != self.start_delimiter and found_delimiter:
                found_delimiter = False
            elif char == self.start_delimiter and found_delimiter and capture_start < 0:
                found_delimiter = False
                delim_count = 1
                capture_start = i + 1
            elif char == self.start_delimiter and capture_start >= 0:
                delim_count += 1
            elif char == self.end_delimiter and capture_start >= 0:
                delim_count -= 1
                if delim_count == 0:
                    captures.append((capture_start, i))
                    capture_start = -1

        result = io.StringIO()
        pos = 0
        for start, end in captures:
            # skip the delimiter + start delimiter
            result.write(self.template[pos : start - 2])
            val = self.template[start:end]
            try:
                val = eval(f"({val})", full_ctx)
            except (NameError, SyntaxError) as e:
                log_debug(f"Failed to interpolate {val}: {e}")
                val = f"${{{val}}}"
            result.write(val)
            pos = end + 1
        result.write(self.template[pos:])
        return result.getvalue()
