import os
import re

from typing import List
from werkzeug.utils import secure_filename

THEME_DIR = 'static/css'


class Theme:
    def __init__(self, filename: str):
        self.filename = secure_filename(filename)
        self.description = 'No description.'

    def exists(self):
        return os.path.exists(os.path.join(THEME_DIR, self.filename + '.scss'))

    def load(self):
        with open(os.path.join('static/css', self.filename + '.scss'), 'r', encoding='utf-8') as f:
            comment = f.readline()
            if comment.startswith('@charset'):
                comment = f.readline()
        m = re.match(r'/\* ([^*]+) \*/', comment)
        if m is not None:
            self.description = m.groups()[0]
        else:
            self.description = 'No description.'
        return self


def get_available_themes() -> List[Theme]:
    return [Theme(file[:-5]).load() for file in os.listdir(THEME_DIR) if file.endswith('.scss')]
