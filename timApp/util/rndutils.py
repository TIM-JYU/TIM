"""
Functions to produce random lists.
For documentation, see: https://tim.jyu.fi/view/tim/ohjeita/satunnaistus
"""

import json
import secrets
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
    """
    Wraps jso in double brackets if it does not start with '['.

    :param jso: string to wrap
    :return: jso wrapped as a list of lists
    """
    if jso == "":
        return "[[1]]"
    if jso.startswith("["):
        return jso
    return "[[" + jso + "]]"


def sep_n_and_jso(jso: str) -> tuple[int, str]:
    """
    Separates the repeat factor and JSON string from a string.
    The separator is '*' or ':'.
    If there is no repeat factor, returns -1 as the repeat factor.

    For example:
        "3*7" -> 3, "[[7]]"
        "3"   -> -1, "[[3]]"

    :param jso: string to parse
    :return: repeat factor and JSON string representing a list of lists
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


def get_range_from(jso: str) -> tuple[int, list[int], int]:
    """
     Returns the count, range bounds, and step parsed from jso.

    :param jso: one of:
       "5" =>      5, [0,4], 1
       "10*3"      => 10, [0, 3], 1
       "10*[3]"    => 10, [0, 3], 1
       "8*[2,5]"   => 8, [2,5], 1
       "9*[2,5,3]" => 9, [2,5], 3
    :return: count, range bounds, and step
    """
    idx = jso.find(":")
    if idx < 0:
        idx = jso.find("*")
    if idx < 0:
        count_str = jso
        jso = ""
    else:
        count_str = jso[:idx]
        jso = jso[idx + 1 :]
    try:
        count = int(count_str)
    except ValueError:
        count = 1

    if len(jso) == 0:  # s10 => 10, [0, 9], 1
        return count, [0, count - 1], 1

    if not jso.startswith("["):  # s10*50
        jso = "[" + jso + "]"

    r = json.loads(jso)

    if len(r) < 2:  # s10*[50]
        r.insert(0, 0)
    step = 1
    if len(r) > 2:
        step = r[2]
    return count, r, step


def get_params(p: str) -> dict[str, int]:
    """
    Converts a string such as "w:4,c:1" to a dictionary.

    :param p: string containing parameter values
    :return: dictionary of parameter values
    """
    params = {}
    if p:
        params = {
            key: int(value) for item in p.split(",") for key, value in [item.split(":")]
        }
    return params


def get_windowed_sequence(
    myrandom: Random, jso: str, params: dict[str, int]
) -> list[int]:
    """
    Returns a list of unique ints from the given interval.

    :param myrandom: random number generator
    :param jso: string containing the interval parameters
    :param params: dict containing sequence parameters
    :return: list of unique ints satisfying the window constraints
    """
    from timApp.util.windowed_sequence import generate

    count, r, step = get_range_from(jso)

    n = len(range(r[0], r[1] + 1, step))

    values = {"c": 0, "d": n, "w": 3}
    if params:
        values.update(params)

    window = values.get("w", 3)
    distinct = values.get("d", n)
    circular = bool(values.get("c", False))

    ret = generate(myrandom, n, window, distinct, count, circular)

    if ret is None:
        raise ValueError("Could not generate windowed sequence with given constraints")

    if r[0] != 0 or step != 1:
        first = r[0]
        for i in range(len(ret)):
            ret[i] = ret[i] * step + first
    return ret


def get_sample_list(myrandom: Random, jso: str) -> list[int]:
    """
    Returns a list of unique ints from the given interval.

    :param myrandom: random number generator
    :param jso: string containing the interval parameters
    :return: list of unique ints
    """

    count, r, step = get_range_from(jso)
    ret = []

    if count == 1:  # handle s1*5: same as normal range
        ret = [myrandom.randrange(r[0], r[1] + 1, step)]
        return ret

    n = r[1] + 1 - r[0]
    if n > 500:
        raise ValueError(f"Too big range for s: {r[0]}-{r[1]}")
    ints = list(range(r[0], r[1] + 1, step))
    i = count
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
    :param jso: string containing the interval parameters
    :return: list of random ints
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
    Returns list of uniformly distributed random
    floats from the given interval.

    :param myrandom: random number generator
    :param jso: string containing the interval parameters
    :return: list of random floats
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


T = TypeVar("T")


def repeat_rnd(
    list_func: Callable[[Random, str], list[T]], myrandom: Random, jso: str
) -> list[T] | None:
    """
    Produces a random list of the requested length by repeating the lists
    produced by list_func. If the requested length is negative, the length
    of the first generated list is used.

    :param list_func: function to produce a random list
    :param myrandom: random number generator
    :param jso: string containing the requested length and instructions
    :return: random list of the requested length, or None if the length is 0
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
    rnd_seed: SeedType | int | None = None,
    state: State | None = None,
) -> tuple[list[float] | list[int] | None, SeedType | int | None, State | None]:
    """
    Returns a list of random numbers based on the attribute name (default: rnd)
    and rnd_seed.

    For attributes, see:

        https://tim.jyu.fi/view/tim/ohjeita/satunnaistus

    :param attrs: dict of attributes
    :param name: name in the attribute dict to use as instructions
                 for the random numbers
    :param rnd_seed: random number initialization seed; if None, use the current time
    :param state: state of the last used generator
    :return: list of random numbers, used seed, and generator state
    """
    if attrs is None:
        return None, rnd_seed, state

    params = get_params(attrs.get(name + "_params", ""))

    no_same = False  # do not give same number
    ret_len = params.get("l", 0)
    order_nr = 0
    jso = attrs.get(name, "")
    if not jso:
        jso = attrs.get("!" + name, "")
        if not jso:
            return None, rnd_seed, state
        no_same = True

    seed_to_use = rnd_seed
    attrs_seed = attrs.get("seed", None)
    if attrs_seed is not None:
        if attrs_seed == "" or attrs_seed == "time":
            # seed_to_use = int(time.perf_counter() * 1000)
            seed_to_use = secrets.randbits(64)
        elif attrs_seed == "answernr":
            if isinstance(rnd_seed, SeedClass):
                if not no_same:
                    seed_to_use = rnd_seed.seed + rnd_seed.extraseed
                else:
                    order_nr = rnd_seed.extraseed
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

    myrandom = Random()
    myrandom.seed(a=seed_to_use)
    if state:
        myrandom.setstate(state)

    def rotate_left_to(seq: list | None, left_rot: int, r_len: int) -> None:
        """
        Rotates the list left_rot steps to the left and,
        if r_len > 0, truncates the list to r_len items.
        :param seq: list to rotate
        :param left_rot: number of steps
        :param r_len: number of items to keep, 0 = all
        :return: None, the list is changed in place
        """
        if not seq:
            return
        left_rot %= len(seq)
        if left_rot > 0:
            seq[:] = seq[left_rot:] + seq[:left_rot]
        if r_len > 0:
            del seq[r_len:]

    ret_list: list[int] | list[float] | None

    if jso.startswith("s"):  # s10:[1,7,2], s10, s10:50, s10:[0,50]
        ret_list = get_sample_list(myrandom, jso[1:])
        rotate_left_to(ret_list, order_nr, ret_len)
        return ret_list, seed_to_use, myrandom.getstate()

    if jso.startswith("w"):  # w10:[1,7,2], w10, w10:50, w10:[0,50]
        ret_list = get_windowed_sequence(myrandom, jso[1:], params)
        rotate_left_to(ret_list, order_nr, ret_len)
        return ret_list, seed_to_use, myrandom.getstate()

    if jso.startswith("u"):  # u[[0,1],[100,110],[-30,-20],[0.001,0.002]], u6
        ret_list = repeat_rnd(get_uniform_list, myrandom, jso[1:])
        rotate_left_to(ret_list, order_nr, ret_len)
        return ret_list, seed_to_use, myrandom.getstate()

    ret_list = repeat_rnd(get_int_list, myrandom, jso)
    rotate_left_to(ret_list, order_nr, ret_len)
    return ret_list, seed_to_use, myrandom.getstate()


def get_rands_as_dict(
    attrs: dict, rnd_seed: SeedType | None, state: State | None = None
) -> tuple[dict | None, SeedType | None, State | None]:
    """
    Returns a dict of random number variables
    (each variable is a list of random numbers).

    :param attrs: dict that may contain the key rndnames:"rnd1,rnd2,..,rndn".
                  If no names are given, "rnd" is assumed.
    :param rnd_seed: seed to initialize the random number generator
    :param state: state of the previously used generator
    :return: dict of random variables
    """
    if attrs is None:
        return None, rnd_seed, state
    names = attrs.get("rndnames", "rnd").split(",")
    ret: dict = {}
    for name in names:
        rnds, rnd_seed, state = get_rnds(attrs, name, rnd_seed, state)
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
    Returns a Jinja2 string of random number variables
    (each variable is a list of random numbers).

    :param attrs: dict that may contain the key rndnames:"rnd1,rnd2,..,rndn".
                  If no names are given, "rnd" is assumed.
    :param rnd_seed: seed to initialize the random number generator
    :param state: state of the previously used generator
    :return: Jinja2 string of random variables, seed, and generator state
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
