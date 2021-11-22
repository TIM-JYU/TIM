import os
import re
from dataclasses import dataclass
from pathlib import Path

from werkzeug.utils import secure_filename

from timApp.document.docentry import DocEntry, get_documents
from timApp.util.utils import cached_property

THEME_DIR = Path("static/stylesheets/themes")
OFFICIAL_THEME_PATH = "styles/official"


@dataclass(eq=True, frozen=True)
class Theme:
    filename: str

    def exists(self) -> bool:
        return self.get_path().exists()

    def get_path(self) -> Path:
        return get_theme_path(self.filename)

    @cached_property
    def description(self) -> str:
        with self.get_path().open("r", encoding="utf-8") as f:
            comment = f.readline()
            if comment.startswith("@charset"):
                comment = f.readline()
        m = re.match(r"/\* ([^*]+) \*/", comment)
        if m is not None:
            return m.groups()[0]
        else:
            return "No description."


def resolve_themes(short_names: list[str]) -> list[DocEntry]:
    return get_documents(
        filter_folder=OFFICIAL_THEME_PATH,
        search_recursively=False,
        custom_filter=DocEntry.name.in_(
            [f"{OFFICIAL_THEME_PATH}/{n}" for n in short_names]
        ),
    )


def get_theme_path(filename: str) -> Path:
    return THEME_DIR / f"{secure_filename(filename)}.scss"  # type: ignore[no-untyped-call]


def theme_exists(filename: str) -> bool:
    return get_theme_path(filename).exists()


def get_available_themes() -> list[Theme]:
    return [
        Theme(file[:-5]) for file in os.listdir(THEME_DIR) if file.endswith(".scss")
    ]
