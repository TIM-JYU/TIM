import random
from typing import TypeVar

from tim_common.utils import Missing


def qst_rand_array(
    max_count: int,
    randoms: int,
    seed_word: str,
    random_seed: int = 0,
    locks: int | list[int] | None = None,
) -> list[int]:
    """
    Get array of count integers between 1 and max_count (incl.) using word and extra number as seed.

    :param max_count: highest possible number (incl.) and max return list length
    :param randoms: how many random numbers to fill the array with
    :param seed_word: input word to generate random seed
    :param random_seed: extra number to edit the seed
    :param locks: positions that can't be shuffled, indexing starting from 1. Any position over max_count will be
    interpreted as max_count
    :return: shuffled array of integers of up to max_count values
    """
    if locks is None:
        locks = []
    if isinstance(locks, int):
        locks = [locks]
    for i, val in enumerate(locks):
        if val > max_count:
            locks[i] = max_count
        elif val < 1:
            locks[i] = 1
    locks = list(set(locks))
    locks.sort()
    total = randoms + len(locks)
    if total > max_count:
        total = max_count
    ret: list[int] = []
    seed_array = []
    orig = list(range(1, max_count + 1))
    for i, val in enumerate(locks):
        if len(orig) == 0:
            break
        if val > max_count:
            orig.pop(len(orig) - 1)
        try:
            orig.pop(val - 1 - i)
        except IndexError:
            pass
    # Temp seed generator
    for char in seed_word:
        try:
            seed_array.append(int(char, 36))
        except ValueError:
            pass
    seed = int("".join(map(str, seed_array)))
    qst_random = random.Random(seed + random_seed)
    qst_random.shuffle(orig)
    for i in range(1, total + 1):
        if len(locks) >= total - len(ret):
            ret.append(locks[0])
            locks.pop(0)
        elif len(locks) > 0 and locks[0] == i:
            ret.append(i)
            locks.pop(0)
        else:
            ret.append(orig.pop(0))
    return ret


T = TypeVar("T")


def qst_set_array_order(arr: list[T], order_array: list[int]) -> list[T]:
    """
    pick items from arr in order given by order_array
    indices start from 1
    """
    ret = []
    for val in order_array:
        try:
            ret.append(arr[val - 1])
        except IndexError:
            pass
    return ret


def qst_pick_expls(orig_expls: dict[str, T], order_array: list[int]) -> dict[str, T]:
    """
    pick items from dict where keys are str converted integers in order given by order_array
    indices start from 1
    """
    if orig_expls is None:
        orig_expls = {}
    ret = {}
    for i, val in enumerate(order_array):
        pos = str(val)
        picked = orig_expls.get(pos)
        if picked is not None:
            ret[str(i + 1)] = picked
    return ret


def create_points_table(points: str) -> list[dict[str, float]]:
    points_table = []
    if points and points != "":
        points = str(points)
        points_split = points.split("|")
        for row in points_split:
            row_points = row.split(";")
            row_points_dict = {}
            for col in row_points:
                if col != "":
                    col_points = col.split(":", 2)
                    if len(col_points) == 1:
                        row_points_dict[col_points[0]] = 1.0
                    else:
                        try:
                            row_points_dict[col_points[0]] = float(
                                col_points[1].replace(",", ".")
                            )
                        except ValueError:
                            pass
            points_table.append(row_points_dict)
    return points_table


def calculate_points_from_json_answer(
    single_answers: list[list[str]],
    points_table: list[dict[str, float]] | None,
    default_points: float | None | Missing = 0,
) -> float:
    points = 0.0
    if not isinstance(default_points, float) and not isinstance(default_points, int):
        default_points = 0
    if points_table is None:
        points_table = [{}] * len(single_answers)
    for oneAnswer, point_row in zip(single_answers, points_table):
        for oneLine in oneAnswer:
            if oneLine in point_row:
                points += point_row[oneLine]
            else:
                points += default_points
    return points


def qst_filter_markup_points(
    points: str, question_type: str, rand_arr: list[int]
) -> str:
    """
    filter markup's points field based on pre-generated array
    """
    # TODO: Use constants
    if question_type == "true-false" or question_type == "matrix":
        # point format 1:1;2:-0.5|1:-0.5;2:1 where | splits rows input, ; column input
        arr = points.split("|")
        arr = qst_set_array_order(arr, rand_arr)
        ret = "|".join(arr)
    else:
        # point format 1:1;2:-0.5;3:-0.5 where ; splits row input
        tab = create_points_table(points)[0]
        tab = qst_pick_expls(tab, rand_arr)
        ret = ";".join(str(key) + ":" + str(val) for [key, val] in tab.items())
    return ret


def qst_handle_randomization(jso: dict) -> None:
    """
    Check if markup calls for randomization, or previous state contains randomization data
    Update answer options, explanations and points accordingly.

    :param jso: request json to modify
    """
    markup = jso["markup"]
    rand_arr = None
    prev_state = jso.get("state", None)
    if prev_state and isinstance(prev_state, dict):
        rand_arr = prev_state.get("order")
        jso["state"] = prev_state.get("c")
    rows = markup.get("rows", [])
    if (
        not prev_state and rand_arr is None
    ):  # no previous answer, check markup for new order
        rcount = markup.get("randomizedRows", 0)
        # TODO: try to convert string
        if not isinstance(rcount, int):
            rcount = 0
        if rcount is None:
            rcount = 0
        if rcount > 0:
            # markup['rows'] = qst_randomize_rows(rows,rcount,jso['user_id'])
            random_seed = markup.get("randomSeed", 0)
            # TODO: use random seed generation within qst_rand_array if seed was string
            if not isinstance(random_seed, int):
                random_seed = 0
            locks = markup.get("doNotMove", [])
            # TODO: MarkupModel should handle these checks?
            if locks is None:
                locks = []
            if isinstance(locks, int):
                locks = [locks]
            for val in locks:
                if not isinstance(val, int):
                    locks = []
                    break
            if random_seed is None:
                random_seed = 0
            seed_string = str(jso.get("user_id", "")) + str(jso.get("taskID", ""))
            rand_arr = qst_rand_array(
                len(rows), rcount, seed_string, random_seed, locks
            )
    if rand_arr is not None:  # specific order found in prev.ans or markup
        markup["rows"] = qst_set_array_order(rows, rand_arr)
        markup["expl"] = qst_pick_expls(markup.get("expl"), rand_arr)
        points = markup.get("points")
        if points:
            question_type = markup.get("questionType")
            points = qst_filter_markup_points(points, question_type, rand_arr)
            markup["points"] = points
