import os
import re
from pathlib import Path
from typing import List

from werkzeug.utils import secure_filename

THEME_DIR = Path('static/stylesheets/themes')


class Theme:

    def __init__(self, filename: str):
        self.filename = secure_filename(filename)
        self.description = 'No description.'

    def exists(self):
        return self.get_path().exists()

    def get_path(self):
        return THEME_DIR / f'{self.filename}.scss'

    def load(self):
        with open(self.get_path(), 'r', encoding='utf-8') as f:
            comment = f.readline()
            if comment.startswith('@charset'):
                comment = f.readline()
        m = re.match(r'/\* ([^*]+) \*/', comment)
        if m is not None:
            self.description = m.groups()[0]
        else:
            self.description = 'No description.'
        return self

    def __eq__(self, other):
        if isinstance(other, Theme):
            return self.filename == other.filename
        return False

    def __hash__(self):
        return hash(self.filename)


def get_available_themes() -> List[Theme]:
    return [Theme(file[:-5]).load() for file in os.listdir(THEME_DIR) if file.endswith('.scss')]
