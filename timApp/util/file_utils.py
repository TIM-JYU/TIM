import hashlib
from mimetypes import guess_extension
from os import PathLike

import magic  # type: ignore


def guess_image_type(p: PathLike | str | bytes) -> str | None:
    """
    Guess the image type of file.

    If the file is not an image, return None.

    :param p:  Path to file or bytes content of file.
    :return:  Image type or None.
    """
    mime = guess_image_mime(p)

    if not mime:
        return None

    ext = guess_extension(mime)

    if not ext:
        return None

    return ext.lstrip(".")


def guess_image_mime(p: PathLike | str | bytes) -> str | None:
    """
    Guess the image mime type of file.

    If the file is not an image, return None.

    :param p:  Path to file or bytes content of file.
    :return:  Image mime type or None.
    """
    m = magic.Magic(mime=True)

    if isinstance(p, bytes):
        mime = m.from_buffer(p)
    else:
        mime = m.from_file(p)

    if not mime.startswith("image/"):
        return None

    return mime


def compute_file_sha1(p: PathLike, chunk_size: int = 65536) -> str:
    """
    Compute sha1 hash of a file.

    :param p: Path to file to hash.
    :param chunk_size: Chunk size to read file.
    :return: sha1 hash of the file.
    """
    sha1 = hashlib.sha1()
    with open(p, "rb") as f:
        while read := f.read(chunk_size):
            sha1.update(read)
    return sha1.hexdigest()
