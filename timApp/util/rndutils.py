"""
Functions to produce random lists.
For documentation, see: https://tim.jyu.fi/view/tim/ohjeita/satunnaistus
"""

import json
import secrets
from dataclasses import dataclass
from random import Random
from typing import Union, Callable, TypeVar, TypeAlias

MAX_RND_LIST_LEN = 100


@dataclass(frozen=True)
class SeedClass:
    seed: int
    extraseed: int = 0
    ask_new: bool | None = None


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


def get_range_and_step(jso: str) -> tuple[list[int], int]:
    """
    Returns the range and step parsed from jso.

    :param jso: one of:
       "5" =>      [0, 4], 1
       "3"      => [0, 3], 1
       "[3]"    => [0, 3], 1
       "[2,5]"   => [2, 5], 1
       "[2,5,3]" => [2, 5], 3
    :return: range bounds and step
    """
    if not jso.startswith("["):
        jso = "[" + jso + "]"
    r = json.loads(jso)
    if len(r) < 2:
        r.insert(0, 0)
    step = 1
    if len(r) > 2:
        step = r[2]
    return r, step


def get_count_range_and_step(
    jso: str, def_count: int = -1
) -> tuple[int, list[int], int]:
    """
     Returns the count, range bounds, and step parsed from jso.

    :param jso: one of:
       "5" =>      5, [0, 4], 1   or 1, [0, 4], 1 if def_count = 1
       "10*3"      => 10, [0, 3], 1
       "10*[3]"    => 10, [0, 3], 1
       "8*[2,5]"   => 8, [2, 5], 1
       "9*[2,5,3]" => 9, [2, 5], 3
       "[2,5]"     => 4, [2, 5], 1  or  1, [2, 5], 1 if def_count = 1
       "[2,5,3]"   => 2, [2, 5], 3  or  1, [2, 5], 3 if def_count = 1
    :param def_count: default count if no value is given, -1 = get from range
    :return: count, range bounds, and step
    """
    idx = jso.find(":")
    if idx < 0:
        idx = jso.find("*")
    if idx < 0:
        if jso.startswith("["):
            count_str = "-1"
        else:
            count_str = jso
            jso = ""
    else:
        count_str = jso[:idx]
        jso = jso[idx + 1 :]

    try:
        end = int(count_str)
        count = end
    except ValueError:
        end = 1
        count = 1
    if idx < 0 <= def_count:
        count = def_count

    if len(jso) == 0:  # s10 => 10, [0, 9], 1
        return count, [0, end - 1], 1

    r, step = get_range_and_step(jso)
    if end < 0:
        count = len(range(r[0], r[1] + 1, step))
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


def adjust_to_range(values: list[int], first: int, step: int) -> list[int]:
    """
    Adjust values to correspond to a range with the given start and step.

    The values are modified in place.

    :param values: Values to adjust.
    :param first: First value of the range.
    :param step: Step between consecutive values.
    :return: The modified values.
    """
    if first != 0 or step != 1:
        for i in range(len(values)):
            values[i] = values[i] * step + first
    return values


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

    count, r, step = get_count_range_and_step(jso, 1)

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

    ret = adjust_to_range(ret, r[0], step)
    return ret


def get_next_windowed_sequence(
    myrandom: Random,
    jso: str,
    params: dict[str, int],
    nr: int,
    rnd_save: dict | None = None,
) -> tuple[list[int], list[int] | None]:
    """
    Returns a list of unique ints from the given interval.

    :param myrandom: random number generator
    :param jso: string containing the interval parameters
    :param params: dict containing sequence parameters
    :param nr: answer number
    :param rnd_save: dict containing the saved state of the generator
    :return: list of unique ints satisfying the window constraints
             and dict to save to database
    """
    from timApp.util.windowed_sequence import generate_next, get_old

    count, r, step = get_count_range_and_step(jso, 1)

    n = len(range(r[0], r[1] + 1, step))

    values = {"c": 1, "d": n, "w": 3}
    if params:
        values.update(params)

    window = values.get("w", 3)
    distinct = values.get("d", n)
    old_r: list[int] = []
    old_nr = -2
    if rnd_save is not None:
        saved_r = rnd_save.get("r", [])
        if isinstance(saved_r, list):
            old_r = saved_r
        old_nr = rnd_save.get("nr", 0)
        seed = rnd_save.get("seed", None)
        if seed is not None:
            myrandom.seed(a=seed)

    ret: list[int] | None
    new_r: list[int] | None

    if nr == old_nr:
        old_ret = get_old(old_r, nr, count)
        return old_ret, None

    ret, new_r = generate_next(
        myrandom,
        n,
        window,
        distinct,
        count,
        nr,
        old_r,
    )

    if ret is None:
        raise ValueError("Could not generate windowed sequence with given constraints")

    adjust_to_range(ret, r[0], step)
    return ret, new_r


def get_sample_list(myrandom: Random, jso: str) -> list[int]:
    """
    Returns a list of unique ints from the given interval.

    :param myrandom: random number generator
    :param jso: string containing the interval parameters
    :return: list of unique ints
    """

    count, r, step = get_count_range_and_step(jso)
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


def sep_n_and_range(jso: str) -> tuple[int, str]:
    """
    Separates the count and the range part of an (i) instruction.

    Unlike sep_n_and_jso, a value without a separator is the range and the count
    defaults to one, because an i-list gives one value per attempt by default.
    A bare number is then the size of that range, as in s: i10 walks 0-9.
    For example:
        "3:[1,20]" -> 3, "[1,20]"
        "[1,7]"    -> 1, "[1,7]"
        "10"       -> 1, "[0,9]"

    :param jso: string to check
    :return: count of values per attempt and the string that stands for a range
    """
    idx = jso.find(":")
    if idx < 0:
        idx = jso.find("*")
    if idx < 0:
        try:
            return 1, f"[0,{int(jso) - 1}]"
        except ValueError:
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
    r, step = get_range_and_step(jso)
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
    if i > 0:
        rnds = list_func(myrandom, jso)
        ret.extend(rnds[0:i])
    return ret


# Mypy needs capital "Tuple" here.
State = tuple[int, ...]

GetRndsResult: TypeAlias = tuple[
    list[float] | list[int] | None,
    SeedType | int | None,
    State | None,
    dict | None,
]


def get_rnds(
    attrs: dict,
    name: str = "rnd",
    rnd_seed: SeedType | int | None = None,
    state: State | None = None,
    rnd_save: dict | None = None,
) -> GetRndsResult:
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
    :param rnd_save: saved dict for the last used generator
    :return: list of random numbers, used seed, and generator state
    """
    if attrs is None:
        return None, rnd_seed, state, None

    # How many attempts came before this one.
    # Only i-lists use it; without it, they stay on the first value.
    index = 0

    if isinstance(rnd_seed, SeedClass):
        index = rnd_seed.extraseed
        ask_new = rnd_seed.ask_new
    else:
        ask_new = True

    # Is this already saved?
    if rnd_save is not None:
        old_nr = rnd_save.get("nr", -2)
        if index == old_nr:
            old_list = rnd_save.get("v", None)
            if old_list is not None:
                return old_list, rnd_seed, state, None

    if ask_new:  # maybe there is new rnd to replace the old one
        jso = attrs.get(name + "_new", "")
        if not jso:
            jso = attrs.get("!" + name + "_new", "")
        if jso:
            name = name + "_new"

    params = get_params(attrs.get(name + "_params", ""))

    no_same = False  # do not give same number
    ret_len = params.get("l", 0)
    order_nr = 0
    jso = attrs.get(name, "")
    if not jso:
        jso = attrs.get("!" + name, "")
        if not jso:
            return None, rnd_seed, state, None
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

    def save(
        values: list[int] | list[float] | None,
        seed: int | str,
        gen_state: State,
        rnd_save_state: dict | None = None,
        save_as_default: bool = False,
    ) -> tuple[
        list[float] | list[int] | None,
        str | int | SeedClass | None,
        tuple[int, ...] | None,
        dict | None,
    ]:
        """
        Prepares save state for the result depending on from params
        :param values: list to return and possibly to save
        :param seed: seed used to make the list
        :param gen_state: random generator state
        :param rnd_save_state: dict to save as the new rnd_save or None
        :param save_as_default: if True, save as default rnd_save
        """
        need_to_save = params.get("s", save_as_default)
        if not need_to_save or values is None:
            return values, seed, gen_state, None
        state_to_save = rnd_save_state if rnd_save_state is not None else {}
        state_to_save["v"] = values
        state_to_save["nr"] = index
        return values, seed, gen_state, state_to_save

    ret_list: list[int] | list[float] | None

    if jso.startswith("s"):  # s10:[1,7,2], s10, s10:50, s10:[0,50]
        ret_list = get_sample_list(myrandom, jso[1:])
        rotate_left_to(ret_list, order_nr, ret_len)
        return save(ret_list, seed_to_use, myrandom.getstate(), None)

    if jso.startswith("w"):  # w10:[1,7,2], w10, w10:50, w10:[0,50]
        if no_same:
            ret_list = get_windowed_sequence(myrandom, jso[1:], params)
            rotate_left_to(ret_list, order_nr, ret_len)
            return save(ret_list, seed_to_use, myrandom.getstate())

        ret_list, new_r = get_next_windowed_sequence(
            myrandom, jso[1:], params, index, rnd_save
        )
        new_rnd_save = None
        if new_r is not None:
            new_rnd_save = {"r": new_r}
        return save(ret_list, seed_to_use, myrandom.getstate(), new_rnd_save, True)

    if jso.startswith("u"):  # u[[0,1],[100,110],[-30,-20],[0.001,0.002]], u6
        ret_list = repeat_rnd(get_uniform_list, myrandom, jso[1:])
        rotate_left_to(ret_list, order_nr, ret_len)
        return save(ret_list, seed_to_use, myrandom.getstate(), None)

    if jso.startswith("i"):  # i[1,7], i[1,7,2], i3:[1,20], i10
        ret_list = get_distinct_list(distinct_list_seed, index, jso[1:])
        return save(ret_list, seed_to_use, myrandom.getstate(), None)

    ret = repeat_rnd(get_int_list, myrandom, jso)
    rotate_left_to(ret, order_nr, ret_len)
    return save(ret, seed_to_use, myrandom.getstate(), None)


def get_rands_as_dict(
    attrs: dict,
    rnd_seed: SeedType | None,
    state: State | None = None,
    rnd_saves: dict | None = None,
) -> tuple[dict | None, SeedType | None, State | None, dict | None]:
    """
    Returns a dict of random number variables
    (each variable is a list of random numbers).

    :param attrs: dict that may contain the key rndnames:"rnd1,rnd2,..,rndn".
                  If no names are given, "rnd" is assumed.
    :param rnd_seed: seed to initialize the random number generator
    :param state: state of the previously used generator
    :param rnd_saves: dict of saved random number generator statuses,
                      one for each name
    :return: dict of random variables
    """
    if attrs is None:
        return None, rnd_seed, state, None
    names = attrs.get("rndnames", "rnd").split(",")
    ret: dict = {}
    new_rnd_saves: dict[str, dict] = {}
    # get_rnds gives back a plain seed number, so passing that on would leave every
    # name but the first without the attempt counter, and their i-lists would sit on
    # the first value. Give each name the same SeedClass instead.
    counter_seed = rnd_seed if isinstance(rnd_seed, SeedClass) else None
    for name in names:
        rnd_save = rnd_saves.get(name, None) if rnd_saves else None
        rnds, rnd_seed, state, rnd_save = get_rnds(
            attrs, name, counter_seed or rnd_seed, state, rnd_save
        )
        if rnds is None:
            continue
        ret[name] = rnds
        if rnd_save is not None:
            new_rnd_saves[name] = rnd_save
    if not ret:
        return None, rnd_seed, state, None
    ret["seed"] = rnd_seed
    return ret, rnd_seed, state, new_rnd_saves or None


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
    (rands, rnd_seed, state, rnd_save) = get_rands_as_dict(attrs, rnd_seed, state)
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
