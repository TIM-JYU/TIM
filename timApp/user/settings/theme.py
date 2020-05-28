import os
import re
from pathlib import Path
from typing import List
from dataclasses import dataclass, field

from werkzeug.utils import secure_filename
from timApp.util.utils import cached_property


THEME_DIR = Path('static/stylesheets/themes')


@dataclass(eq=True, frozen=True)
class Theme:
    filename: str

    def exists(self) -> bool:
        return self.get_path().exists()

    def get_path(self) -> Path:
        return get_theme_path(self.filename)

    @cached_property
    def description(self) -> str:
        with self.get_path().open('r', encoding='utf-8') as f:
            comment = f.readline()
            if comment.startswith('@charset'):
                comment = f.readline()
        m = re.match(r'/\* ([^*]+) \*/', comment)
        if m is not None:
            return m.groups()[0]
        else:
            return 'No description.'


def get_theme_path(filename: str) -> Path:
    return THEME_DIR / f'{secure_filename(filename)}.scss'  # type: ignore[no-untyped-call]


def theme_exists(filename: str) -> bool:
    return get_theme_path(filename).exists()


def get_available_themes() -> List[Theme]:
    return [Theme(file[:-5]) for file in os.listdir(THEME_DIR) if file.endswith('.scss')]
