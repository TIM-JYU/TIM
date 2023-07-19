from functools import cache
from pathlib import Path


@cache
def get_files_path() -> Path:
    from timApp.tim_app import app

    return Path(app.config["FILES_PATH"])
