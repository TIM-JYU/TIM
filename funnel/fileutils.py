import os
import random
import string
from typing import Generator, Tuple


def listdirs(dirname):
    return [item for item in os.listdir(dirname) if os.path.isdir(os.path.join(dirname, item))]


def listfiles(dirname):
    return [item for item in os.listdir(dirname) if not os.path.isdir(os.path.join(dirname, item))]


def listnormalfiles(dirname):
    return [item for item in os.listdir(dirname)
            if os.path.isfile(os.path.join(dirname, item)) and not os.path.islink(os.path.join(dirname, item))]


def get_subdirs(dirname: str) -> Generator[str, str, str]:
    for rel_name in os.listdir(dirname):
        abs_name = os.path.join(dirname, rel_name)
        if os.path.isdir(abs_name):
            yield rel_name


def get_random_filenames(directory: str, prefix='') -> Tuple[str, str]:
    while True:
        without_path = prefix + ''.join([random.choice(string.ascii_letters) for _ in range(16)])
        with_path = os.path.join(directory, without_path)
        if not os.path.isfile(with_path):
            return with_path, without_path

