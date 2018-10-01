from typing import Tuple

"""Document version number.

Format is major, minor.

Major version is incremented whenever a paragraph is added, inserted or deleted from the document. The minor version is reset to
zero.
Minor version is incremented whenever a paragraph is modified.
"""
Version = Tuple[int, int]


def ver_to_str(v: Version):
    return ','.join((str(x) for x in v))
