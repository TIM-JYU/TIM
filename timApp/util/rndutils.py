"""
Functions to produce random lists.
For documentation, see: https://tim.jyu.fi/view/tim/ohjeita/satunnaistus
"""

import json
import secrets
import time
from dataclasses import dataclass
from random import Random
from typing import Union, Callable, TypeVar

MAX_RND_LIST_LEN = 100


@dataclass(frozen=True)
class SeedClass:
    seed: int
    extraseed: int = 0


SeedType = Union[str, int, SeedClass]


def fix_jso(jso: str) -> str:
    """If jso does not start with [ and two to make it list of lists."""
    if jso == "":
        return "[[1]]"
    if jso.startswith("["):
        return jso
    return "[[" + jso + "]]"


def sep_n_and_jso(jso: str) -> tuple[int, str]:
    """
    Separates repeat factor and json string from string. Separator is * or :
    If no repeat factor, return just json string.
    For example:
        "3*7" -> 3, [[7]]
        "3"   -> -1, [[3]]

    :param jso: string to check
    :return: repeat factor and json-str that stands for a list
    """
    idx = jso.find(":")
    if idx < 0:
        idx = jso.find("*")
    if idx < 0:
        return -1, fix_jso(jso)  # means no repeat factor
    n_str = jso[:idx]
    jso = jso[idx + 1 :]
    try:
        n = int(n_str)
        if n < 0:
            n = 0
    except ValueError:
        n = -1
    n = min(n, MAX_RND_LIST_LEN)
    return n, fix_jso(jso)


def get_sample_list(myrandom: Random, jso: str) -> list[int]:
    """
    Returns a list of unique ints from the given interval.

    :param myrandom: random number generator
    :param jso: string to find the values
    :return: list of unique ints
    """
    idx = jso.find(":")
    if idx < 0:
        idx = jso.find("*")
    if idx < 0:
        n_str = jso
        jso = ""
    else:
        n_str = jso[:idx]
        jso = jso[idx + 1 :]
    try:
        n = int(n_str)
    except ValueError:
        n = 1
    n = min(n, MAX_RND_LIST_LEN)

    ret = []
    if len(jso) == 0:  # s10
        ints = list(range(0, n, 1))
        myrandom.shuffle(ints)
        return ints

    if not jso.startswith("["):  # s10*50
        jso = "[" + jso + "]"

    r = json.loads(jso)

    if len(r) < 2:  # s10*[50]
        r.insert(0, 0)
    step = 1
    if len(r) > 2:
        step = r[2]

    if n == 1:  # handle s1: same as normal range
        ret = [myrandom.randrange(r[0], r[1] + 1, step)]
        return ret

    count = r[1] - r[0]
    if count > 500:
        raise ValueError(f"Too big range for s: {r[0]}-{r[1]}")
    ints = list(range(r[0], r[1] + 1, step))
    i = n
    while i >= len(ints):
        myrandom.shuffle(ints)
        ret.extend(ints)
        i -= len(ints)
    myrandom.shuffle(ints)
    ret.extend(ints[0:i])
    return ret


def get_int_list(myrandom: Random, jso: str) -> list[int]:
    """
    Returns list of random ints from given interval.

    :param myrandom: random number generator
    :param jso: string to find the values
    :return: list of random ints ints
    """
    ranges = json.loads(jso)
    if isinstance(ranges, int):  # only on item, rnd=6
        return [myrandom.randint(0, ranges)]
    ret = []
    for r in ranges:
        if isinstance(r, int):  # only on item, rnd=[6, 4]
            ret.append(myrandom.randint(0, r))
        else:
            if len(r) < 2:
                r.insert(0, 0)
            step = 1
            if len(r) > 2:
                step = r[2]
            ret.append(myrandom.randrange(r[0], r[1] + 1, step))
    return ret


def get_uniform_list(myrandom: Random, jso: str) -> list[float]:
    """
    Returns list of uniformly distributed random floats from given interval.

    :param myrandom: random number generator
    :param jso: string to find the values
    :return: list of random ints ints
    """
    ranges = json.loads(jso)
    if isinstance(ranges, float) or isinstance(ranges, int):  # only on item, rnd=6
        return [myrandom.uniform(0, ranges)]
    ret = []
    for r in ranges:
        if isinstance(ranges, float) or isinstance(
            ranges, int
        ):  # only on item, rnd=[6, 4]
            ret.append(myrandom.uniform(0, r))
        else:
            if len(r) < 2:
                r.insert(0, 0)
            ret.append(myrandom.uniform(r[0], r[1]))
    return ret


def sep_n_and_range(jso: str) -> tuple[int, str]:
    """
    Separates the count and the range part of an (i) instruction.

    Unlike sep_n_and_jso, a value without a separator is the range and the count
    defaults to one, because an i-list gives one value per attempt by default.
    For example:
        "3:[1,20]" -> 3, "[1,20]"
        "[1,7]"    -> 1, "[1,7]"
        "10"       -> 1, "10"

    :param jso: string to check
    :return: count of values per attempt and the string that stands for a range
    """
    idx = jso.find(":")
    if idx < 0:
        idx = jso.find("*")
    if idx < 0:
        return 1, jso
    n_str = jso[:idx]
    try:
        n = int(n_str)
    except ValueError:
        n = 1
    n = max(n, 1)
    return min(n, MAX_RND_LIST_LEN), jso[idx + 1 :]


def get_distinct_pool(jso: str) -> list[int]:
    """
    Returns the pool of unique values that a list cycles through.

    :param jso: string to find the values, for example "[1,7]", "[1,7,2]" or "10"
    :return: list of unique ints
    """
    if not jso:
        raise ValueError("No range for i")
    if not jso.startswith("["):  # i10
        jso = "[" + jso + "]"
    r = json.loads(jso)
    if len(r) < 2:  # i[50]
        r.insert(0, 0)
    step = 1
    if len(r) > 2:  # i[1,7,2]
        step = r[2]
    if step == 0:
        raise ValueError("Zero step for i")
    if abs(r[1] - r[0]) > 500:
        raise ValueError(f"Too big range for i: {r[0]}-{r[1]}")
    # Like s, both ends of the range belong to it.
    pool = list(range(r[0], r[1] + (1 if step > 0 else -1), step))
    if not pool:
        raise ValueError(f"Empty range for i: {r[0]}-{r[1]}")
    return pool


def shuffle_pool(base_seed: SeedType, cycle: int, pool: list[int]) -> list[int]:
    """
    Returns the pool shuffled for one cycle, not looking at any other cycle.

    :param base_seed: seed that stays the same from one attempt to the next
    :param cycle: how many full rounds of the pool were used before this one
    :param pool: values to shuffle
    :return: shuffled copy of pool
    """
    ints = list(pool)
    myrandom = Random()
    myrandom.seed(a=f"{base_seed}:{cycle}")
    myrandom.shuffle(ints)
    return ints


def get_distinct_cycle(base_seed: SeedType, cycle: int, pool: list[int]) -> list[int]:
    """
    Returns the order in which one cycle uses the values of the pool.

    Each cycle is shuffled anew, but a cycle never starts with the value the
    cycle before it ended with, so no value is given twice in a row over the
    wrap.

    :param base_seed: seed that stays the same from one attempt to the next
    :param cycle: how many full rounds of the pool were used before this one
    :param pool: values to put in order
    :return: values of pool in the order this cycle uses them
    """
    size = len(pool)
    if cycle <= 0 or size < 2:
        return shuffle_pool(base_seed, cycle, pool)
    if size == 2:  # Two values leave no room to choose
        return shuffle_pool(base_seed, 0, pool)
    ints = shuffle_pool(base_seed, cycle, pool)
    if ints[0] == shuffle_pool(base_seed, cycle - 1, pool)[-1]:
        i = 1 + cycle % (size - 2)  # neither the first nor the last slot
        ints[0], ints[i] = ints[i], ints[0]
    return ints


def get_distinct_list(base_seed: SeedType, index: int, jso: str) -> list[int]:
    """
    Returns one attempt's worth of values from a shuffled pool.

    The pool is walked in order, so a value comes up again only after every
    other value has been used. When the pool runs out, the walk wraps around to
    a new shuffle of the same values.

    :param base_seed: seed that stays the same from one attempt to the next
    :param index: number of attempts before this one, from SeedClass.extraseed
    :param jso: string to find the values
    :return: list of values for this attempt
    """
    n, jso = sep_n_and_range(jso)
    pool = get_distinct_pool(jso)
    size = len(pool)
    cycles: dict[int, list[int]] = {}
    ret = []
    for pos in range(index * n, index * n + n):
        cycle, slot = divmod(pos, size)
        order = cycles.get(cycle)
        if order is None:
            order = get_distinct_cycle(base_seed, cycle, pool)
            cycles[cycle] = order
        ret.append(order[slot])
    return ret


T = TypeVar("T")


def repeat_rnd(
    list_func: Callable[[Random, str], list[T]], myrandom: Random, jso: str
) -> list[T] | None:
    """

    :param list_func: function to produce random list
    :param myrandom: random number generator
    :param jso: string to parse instructions
    :return: list of random numbers
    """
    n, jso = sep_n_and_jso(jso)
    if n == 0:
        return None
    rnds = list_func(myrandom, jso)
    lr = len(rnds)
    if n < 0:
        n = lr
    if lr >= n:
        return rnds[0:n]

    ret = rnds
    i = n - lr
    while i > lr:
        rnds = list_func(myrandom, jso)
        ret.extend(rnds)
        i -= lr
    ret.extend(rnds[0:i])
    return ret


# Mypy needs capital "Tuple" here.
State = tuple[int, ...]


def get_rnds(
    attrs: dict,
    name: str = "rnd",
    rnd_seed: SeedType | None = None,
    state: State | None = None,
) -> tuple[list[float] | list[int] | None, SeedType | None, State | None]:
    """
    Returns list of random numbers based on attribute name (def: rnd) and rnd_seed.

    :param attrs: dict of attributes
    :param name: name in attribute dict to use as instructions for the random numbers
    :param rnd_seed: random number initialization seed, if seed is None, use time
    :param state of last used generator
    :return: list of random numbers and used seed
    """
    if attrs is None:
        return None, rnd_seed, state

    jso: str = attrs.get(name, "")
    if not jso:
        return None, rnd_seed, state

    # How many attempts came before this one.
    # Only i-lists use it; without it they stay on the first value.
    index = rnd_seed.extraseed if isinstance(rnd_seed, SeedClass) else 0

    seed_to_use = rnd_seed
    attrs_seed = attrs.get("seed", None)
    if attrs_seed is not None:
        if attrs_seed == "" or attrs_seed == "time":
            # seed_to_use = int(time.perf_counter() * 1000)
            seed_to_use = secrets.randbits(64)
        elif attrs_seed == "answernr":
            if isinstance(rnd_seed, SeedClass):
                seed_to_use = rnd_seed.seed + rnd_seed.extraseed
        else:
            seed_to_use = attrs_seed

    if isinstance(seed_to_use, SeedClass):
        seed_to_use = seed_to_use.seed

    if isinstance(seed_to_use, str):
        seed_to_use = myhash(seed_to_use)

    # noinspection PyBroadException
    if seed_to_use is None:
        # seed_to_use = int(time.perf_counter() * 1000)
        seed_to_use = secrets.randbits(64)

    # An i-list has to see the same pool on every attempt. seed="answernr" mixes
    # the attempt number into seed_to_use, which would put the pool in a new order
    # every time and undo the point of walking it.
    stable_seed = seed_to_use
    if attrs_seed == "answernr" and isinstance(rnd_seed, SeedClass):
        stable_seed = rnd_seed.seed
    # The name is mixed in so that two i-lists in the same block walk different
    # orders. s and u are kept apart by the shared generator state instead, which
    # an i-list does not use.
    distinct_list_seed = f"{stable_seed}:{name}"

    myrandom = Random()
    myrandom.seed(a=seed_to_use)
    if state:
        myrandom.setstate(state)

    if jso.startswith("s"):  # s10:[1,7,2], s10, s10:50, s10:[0,50]
        return get_sample_list(myrandom, jso[1:]), seed_to_use, myrandom.getstate()
    if jso.startswith("u"):  # u[[0,1],[100,110],[-30,-20],[0.001,0.002]], u6
        return (
            repeat_rnd(get_uniform_list, myrandom, jso[1:]),
            seed_to_use,
            myrandom.getstate(),
        )

    if jso.startswith("i"):  # i[1,7], i[1,7,2], i3:[1,20], i10
        return (
            get_distinct_list(distinct_list_seed, index, jso[1:]),
            seed_to_use,
            myrandom.getstate(),
        )

    ret = repeat_rnd(get_int_list, myrandom, jso)
    return ret, seed_to_use, myrandom.getstate()


def get_rands_as_dict(
    attrs: dict, rnd_seed: SeedType | None, state: State | None = None
) -> tuple[dict | None, SeedType | None, State | None]:
    """
    Returns a dict of random numbers variables (each is a list of random numbers).

    :param attrs: dict where may be attribute rndnames:"rnd1,rnd2,..,rndn".  Of no names, "rnd"
                  is assumed
    :param rnd_seed: seed to initialize the generator
    :param state of last used generator
    :return: dict of random variables
    """
    if attrs is None:
        return None, rnd_seed, state
    names = attrs.get("rndnames", "rnd").split(",")
    ret: dict = {}
    # get_rnds gives back a plain seed number, so passing that on would leave every
    # name but the first without the attempt counter, and their i-lists would sit on
    # the first value. Give each name the same SeedClass instead.
    counter_seed = rnd_seed if isinstance(rnd_seed, SeedClass) else None
    for name in names:
        rnds, rnd_seed, state = get_rnds(attrs, name, counter_seed or rnd_seed, state)
        if rnds is None:
            continue
        ret[name] = rnds
    if not ret:
        return None, rnd_seed, state
    ret["seed"] = rnd_seed
    return ret, rnd_seed, state


def get_rands_as_str(
    attrs: dict, rnd_seed: SeedType | None, state: State | None = None
) -> tuple[str, SeedType | None, State | None]:
    """
    Returns a Jinja2 str of random numbers variables (each is a list of random numbers).

    :param attrs: dict where may be attribute rndnames:"rnd1,rnd2,..,rndn".  Of no names, "rnd"
                  is assumed
    :param rnd_seed: seed to initialize the generator
    :param state of last used generator
    :return: Jinja 2 str of random variables
    """
    if attrs is None:
        return "", rnd_seed, state
    rands, rnd_seed, state = get_rands_as_dict(attrs, rnd_seed, state)
    if rands is None:
        return "", rnd_seed, state
    ret = ""
    for name, rnds in rands.items():
        if rnds is None:
            continue
        ret += "{% set " + name + "=" + str(rnds) + " %}\n"
    return ret, rnd_seed, state


def myhash(s: str) -> int:
    """
    Simple hash function to give always same hash for same input.

    :param s: string to hash
    :return: simple hash
    """
    csum = 0
    for c in s:
        csum += ord(c)
    return csum
