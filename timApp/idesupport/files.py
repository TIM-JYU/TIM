"""
File types used for IDE support
"""

import re
from dataclasses import dataclass


@dataclass
class SupplementaryFile:
    """
    Supplementary file that can be provided in the task.
    Supplementary files are any extra files that are needed to run a task locally.
    """

    filename: str
    """File name"""

    content: str | None = None
    """File raw content, if provided"""

    source: str | None = None
    """If no content, URL of the file source"""

    def to_json(self) -> dict[str, str | None]:
        return {
            "content": self.content,
            "file_name": self.filename,
            "source": self.source,
        }


def is_in_filename(files: list[dict[str, str]], regexp: str) -> bool:
    """
    Checks if the regular expression matches to some file

    :param files: list of files to study
    :param regexp: regular expression to match

    :return: True if found, False otherwise
    """
    for file in files:
        name = file.get("filename")
        if name and re.match(regexp, name):
            return True
    return False
