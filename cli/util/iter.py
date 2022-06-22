from itertools import tee


def pairwise(iterable):
    """Returns an iterator of paired items, overlapping, from the original

    >>> take(4, pairwise(count()))
    [(0, 1), (1, 2), (2, 3), (3, 4)]

    """
    a, b = tee(iterable)
    next(b, None)
    yield from zip(a, b)
