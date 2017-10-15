import enum


class PreloadOption(enum.Enum):
    """Specifies how a Document should preload its paragraphs in memory when a single paragraph is accessed."""
    none = 1
    all = 2
