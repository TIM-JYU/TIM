from itertools import tee
from typing import Iterator, Tuple, Iterable, TypeVar, IO, AnyStr

_T = TypeVar("_T")


def pairwise(iterable: Iterable[_T]) -> Iterator[Tuple[_T, _T]]:
    """Returns an iterator of paired items, overlapping, from the original

    >>> take(4, pairwise(count()))
    [(0, 1), (1, 2), (2, 3), (3, 4)]

    """
    a, b = tee(iterable)
    next(b, None)
    yield from zip(a, b)


def iter_chunked(fp: IO[AnyStr], chunk_size: int = 8192) -> Iterator[bytes]:
    """Yields chunks of data from a file-like object"""
    while True:
        chunk = fp.read(chunk_size)
        if not chunk:
            break
        yield chunk
