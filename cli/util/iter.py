from itertools import tee
from typing import Iterator, Tuple, Iterable, TypeVar

_T = TypeVar("_T")


def pairwise(iterable: Iterable[_T]) -> Iterator[Tuple[_T, _T]]:
    """Returns an iterator of paired items, overlapping, from the original

    >>> take(4, pairwise(count()))
    [(0, 1), (1, 2), (2, 3), (3, 4)]

    """
    a, b = tee(iterable)
    next(b, None)
    yield from zip(a, b)
