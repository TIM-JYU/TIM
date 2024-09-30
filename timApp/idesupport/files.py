import re
from dataclasses import dataclass, field


@dataclass
class SupplementaryFile:
    filename: str
    content: str | None = None
    source: str | None = None

    def to_json(self) -> dict[str, str | None]:
        return {
            "content": self.content,
            "file_name": self.filename,
            "source": self.source,
        }


def is_in_filename(files: list[dict[str, str]], regexp: str) -> bool:
    """
    Checks if regexp matches to some file
    :param files: list of files to study
    :return: True if found,
    """
    for file in files:
        name = file.get("filename")
        if name and re.match(regexp, name):
            return True
    return False
