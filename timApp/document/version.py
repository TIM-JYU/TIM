"""Document version number.

Format is major, minor.

Adding, inserting, or deleting a paragraph increases the major version and
resets the minor version to zero.
Modifying a paragraph increases the minor version.
"""

Version = tuple[int, int]


def ver_to_str(v: Version):
    return ",".join(str(x) for x in v)
