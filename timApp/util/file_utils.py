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
    m = magic.Magic(mime=True)

    if isinstance(p, bytes):
        mime = m.from_buffer(p)
    else:
        mime = m.from_file(p)

    if not mime.startswith("image/"):
        return None

    ext = guess_extension(mime)

    if not ext:
        return None

    return ext.lstrip(".")
