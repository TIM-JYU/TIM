import os
import re
from pathlib import Path
from typing import List
from dataclasses import dataclass, field

from werkzeug.utils import secure_filename

THEME_DIR = Path('static/stylesheets/themes')


@dataclass
class Theme:
    filename: str = field(hash=True)
    description: str = field(init=False, default='No description')

    def exists(self):
        return self.get_path().exists()

    def get_path(self):
        return get_theme_path(self.filename)

    def load(self):
        with self.get_path().open('r', encoding='utf-8') as f:
            comment = f.readline()
            if comment.startswith('@charset'):
                comment = f.readline()
        m = re.match(r'/\* ([^*]+) \*/', comment)
        if m is not None:
            self.description = m.groups()[0]
        else:
            self.description = 'No description.'
        return self


def get_theme_path(filename: str) -> Path:
    return THEME_DIR / f'{secure_filename(filename)}.scss'


def theme_exists(filename: str) -> bool:
    return get_theme_path(filename).exists()


def get_available_themes() -> List[Theme]:
    return [Theme(file[:-5]).load() for file in os.listdir(THEME_DIR) if file.endswith('.scss')]
