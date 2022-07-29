import hashlib

from os import PathLike
from typing import Union

_remove_newlines_trans = str.maketrans({"\r": "", "\n": ""})


def hash_file(*paths: Union[str, PathLike]) -> str:
    """
    Compute sha1 hash of a file.

    :param paths: List of paths to files to hash.
    :return: sha1 hash of the file.
    """

    sha1 = hashlib.sha1()
    for path in paths:
        with open(path, "r") as f:
            data = f.read().translate(_remove_newlines_trans)
            sha1.update(data.encode("utf-8"))
    return sha1.hexdigest()
