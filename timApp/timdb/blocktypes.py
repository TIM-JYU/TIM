import collections

BlockType = collections.namedtuple('blocktypes', ('DOCUMENT', 'COMMENT', 'NOTE', 'ANSWER', 'IMAGE', 'READING', 'FOLDER', 'FILE', 'UPLOAD', 'VELPGROUP', 'ANNOTATION'))
blocktypes = BlockType(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)


def from_str(type_name: str):
    return blocktypes.__getattribute__(type_name.upper())
