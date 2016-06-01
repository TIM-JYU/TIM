import os
from typing import Generator, Tuple


def listdirs(dirname):
    return [item for item in os.listdir(dirname) if os.path.isdir(item)]


def listfiles(dirname):
    return [item for item in os.listdir(dirname) if not os.path.isdir(item)]


def get_subdirs(dirname: str) -> Generator[str, str, str]:
    for rel_name in os.listdir(dirname):
        abs_name = os.path.join(dirname, rel_name)
        if os.path.isdir(abs_name):
            yield rel_name

