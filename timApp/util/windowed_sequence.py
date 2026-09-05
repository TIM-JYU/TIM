from dataclasses import dataclass
from random import Random
import time

"""
Generate sequences with constraints on sliding windows and distinct groups.

Authors:
    vesal
    ChatGPT (GPT-5.6 Luna)

This module is somewhat complex, and possibly more complex than strictly
necessary. The complexity comes mainly from generating sequences efficiently
while satisfying several interacting constraints.

The main externally callable function is ``generate()``. It is the public
interface for generating a sequence.

The rest of the code is primarily internal implementation. In particular,
there is an internal main program that exercises ``generate()`` with various
parameter combinations and checks that the generated sequences satisfy the
required constraints.

If this code is modified, the internal tests should be run. It is important
not only to check that the tests pass, but also to inspect the generated
results to make sure they are still sensible. A change can produce technically
valid sequences while nevertheless changing the intended behaviour of the
generator.
"""


@dataclass
class GenerateState:
    """
    State specific to one call of generate().

    The state is passed to the internal generation functions instead of
    recreating those functions as closures on every call to generate().
    """

    myrandom: Random
    n: int
    window: int
    distinct: int
    count: int
    circular: bool
    all_values: set[int]
    full_count: int
    final_start: int


def check_circular(result: list[int], window: int) -> bool:
    """
    Check the sliding-window constraint treating the sequence
    as a circular sequence.

    :param result: Sequence to check.
    :param window: Size of the circular window.

    :returns: True if every circular window contains distinct values,
        otherwise False.
    """
    length = len(result)

    if length < window:
        return False

    for i in range(length):
        values = [result[(i + j) % length] for j in range(window)]

        if len(set(values)) != window:
            return False

    return True


def fix_parameters(
    n: int,
    window: int,
    distinct: int,
) -> tuple[int, int, int]:
    """
    Adjust parameters to satisfy:
        1 <= window <= distinct <= n
    :param  n: number of ints
    :param window: Size of the sliding window.
    :param distinct: Size of distinct groups
    """
    if n < 2:
        n = 2

    if window < 1:
        window = 1
    elif window > n:
        window = n

    if distinct < window:
        distinct = window
    elif distinct > n:
        distinct = n

    return n, window, distinct


def check(
    result: list[int] | None,
    n: int,
    window: int,
    distinct: int,
    circular: bool = False,
) -> str | None:
    """
    Check whether a generated sequence satisfies its constraints.

    :param result: Sequence to check.
    :param n: Number of possible values, 0..n-1.
    :param window: Maximum linear window size.
    :param distinct: Size of each complete group whose values
            must be different.
    :param circular: If True, also check the circular constraint.
    :returns: None if the sequence is valid.
        Otherwise, return a message describing relaxed constraints
        or an error.
    """
    if result is None:
        return "result is None"

    n, window, distinct = fix_parameters(n, window, distinct)
    window = min(window, len(result))

    # Check the allowed value range.
    for value in result:
        if value < 0 or value >= n:
            return f"value outside allowed range: {value}"

    # ------------------------------------------------------------
    # Check complete distinct groups.
    # ------------------------------------------------------------

    full_count = (len(result) // distinct) * distinct

    for i in range(0, full_count, distinct):
        group = result[i : i + distinct]

        if len(set(group)) != distinct:
            return "group does not contain enough different values: " f"{group}"

    # ------------------------------------------------------------
    # Find the effective linear window.
    #
    # Complete groups before the final group must still obey the
    # original window. Only the final group may use a smaller w.
    # ------------------------------------------------------------

    if len(result) % distinct == 0:
        # The final group is complete, but it is still the group
        # that generate() was allowed to optimize.
        final_start = max(0, len(result) - distinct)
    else:
        # The final group is incomplete.
        final_start = full_count

    effective_window = 1

    for w in range(window, 0, -1):
        valid = True
        values = []

        # Windows completely before the final group must use the
        # original window.
        for i in range(max(0, final_start - window + 1)):
            values = result[i : i + window]

            if len(set(values)) != window:
                valid = False
                break

        if not valid:
            return f"same number in window before final group: {values}"

        # Check windows touching the final group using w.
        for i in range(
            max(0, final_start - w + 1),
            len(result) - w + 1,
        ):
            values = result[i : i + w]

            if len(set(values)) != w:
                valid = False
                break

        if valid:
            effective_window = w
            break

    # ------------------------------------------------------------
    # Find the effective circular window.
    # ------------------------------------------------------------

    effective_circular_window = 1

    if circular:
        for c in range(window, 0, -1):
            if check_circular(result, c):
                effective_circular_window = c
                break

    # ------------------------------------------------------------
    # Report relaxed constraints.
    # ------------------------------------------------------------

    messages = []

    if effective_window < window:
        messages.append(f"w {window} => {effective_window}")

    if circular and effective_circular_window < window:
        messages.append(f"c {window} => " f"{effective_circular_window}")

    if messages:
        return "; ".join(messages)

    return None


def get_available(
    state: GenerateState,
    prefix: list[int],
    current_window: int,
) -> set[int]:
    """
    Return values that can be appended to prefix.

    Windows which start before the final group must satisfy the
    original window. Windows starting inside the final group use
    current_window.

    :param state: Generation state for the current generate() call.
    :param prefix: Part of the sequence constructed so far.
    :param current_window: Window currently being considered.
    :returns: Values that can be appended to prefix.
    """
    position = len(prefix)

    avail = set(state.all_values)

    # Windows starting before the final group use the
    # original window size.
    if position < state.final_start + state.window - 1:
        start = max(0, position - (state.window - 1))
        avail -= set(prefix[start:position])

    # Windows starting in the final group use the possibly
    # relaxed current window size.
    if position >= state.final_start:
        start = max(
            state.final_start,
            position - (current_window - 1),
        )
        avail -= set(prefix[start:position])

    # The distinct constraint applies to complete groups.
    if position < state.full_count:
        group_start = (position // state.distinct) * state.distinct
        avail -= set(prefix[group_start:position])

    return avail


def generate_linearly(
    state: GenerateState,
    length: int,
) -> list[int]:
    """
    Generate a sequence of the requested length without applying
    the circular constraint.

    :param state: Generation state for the current generate() call.
    :param length: Number of values to generate.
    :returns: A sequence satisfying the linear constraints, or None.
    """
    result: list[int] = []

    while len(result) < length:
        available = get_available(state, result, state.window)
        result.append(state.myrandom.choice(tuple(available)))
        # print(available, result)

    return result


def circular_ok(
    state: GenerateState,
    prefix: list[int],
    value: int,
    cir: int,
) -> bool:
    """
    Check circular windows that become complete when value is
    appended to prefix.

    For example, with count=6 and cir=4, after adding the last
    value the sequence is:

        [a b c d e f]

    The circular windows crossing the end are:

        [d e f a]
        [e f a b]
        [f a b c]

    These are checked immediately when the last value is added.
    Before that they are incomplete and cannot yet be rejected.

    :param state: Generation state for the current generate() call.
    :param prefix: Part of the sequence constructed so far.
    :param value: Value being considered for the next position.
    :param cir: Circular window currently being considered.
    :returns: True if the value satisfies the circular constraint,
        otherwise False.
    """
    pos = len(prefix)

    # Only the last value can complete windows crossing
    # from the end of the sequence back to the beginning.
    if pos != state.count - 1:
        return True

    sequence = prefix + [value]

    for start in range(state.count - cir + 1, state.count):
        circ_values = [
            sequence[(start + offset) % state.count] for offset in range(cir)
        ]

        if len(set(circ_values)) != cir:
            return False

    return True


def complete(
    state: GenerateState,
    prefix: list[int],
    win: int,
    cir: int,
) -> list[int] | None:
    """
    Complete the sequence using linear window win and
    circular window cir.

    Backtracking is used so that a bad random choice does
    not cause an otherwise possible (w, c) combination
    to be rejected.

    :param state: Generation state for the current generate() call.
    :param prefix: Part of the sequence constructed so far.
    :param win: Size of the linear window.
    :param cir: Size of the circular window.
    :returns: A completed sequence, or None if no solution can be found.
    """
    if len(prefix) == state.count:
        if not state.circular:
            return prefix

        if check_circular(prefix, cir):
            return prefix

        return None

    avail = get_available(state, prefix, win)

    if state.circular:
        avail = {value for value in avail if circular_ok(state, prefix, value, cir)}

    if not avail:
        return None

    values = list(avail)
    state.myrandom.shuffle(values)

    for value in values:
        cand = complete(state, prefix + [value], win, cir)
        if cand is not None:
            return cand

    return None


def generate(
    myrandom: Random,
    n: int,
    window: int,
    distinct: int,
    count: int,
    circular: bool = False,
) -> list[int] | None:
    """
    Generate a sequence in which no window-sized subsequence
    contains repeated values, subject to additional group and
    circular window constraints. Every group of the specified
    size (distinct) must contain different values from 0 to n-1.
    The sequence is generated randomly up to the final group,
    which may be incomplete (if count % distinct != 0).

    The sequence is generated normally up to the final group.
    The final group is then searched using progressively relaxed
    linear and circular window sizes.

    For example, with

        n = 5, window = 3, distinct = 4, count = 10, circular = True

    one may get a result like

        [0, 3, 4, 1,   3, 0, 2, 1,   4, 2]

    which satisfies the rules:
      - The first 8 values form 2 complete groups of 4,
        each containing different values from 0 to 4.
      - No window of size 3 contains repeated values.
      - The circular windows of size 3:
          [4, 2, 0] and [2, 0, 3]
        also contain different values.

    If all rules cannot be satisfied, the generator tries to
    relax the linear and circular window constraints.

    For example, with

        n = 5, window = 5, distinct = 5, count = 11, circular = True

    it is not possible to satisfy the circular constraint with
    a window size of 5. Example:

        [0, 1, 2, 4, 3, 0, 1, 2, 4, 3, ?]

    If the linear window size is kept at 5,
    the circular window size must be relaxed to 1 because the
    last value must be 0.

    If the linear window size is also relaxed, the circular
    window size can remain at 3. One possible result is:

        [0, 1, 2, 4, 3, 0, 1, 2, 4, 3, 2]

    which gives (w=linear window, c=circular window):

        w 5 => 3; c 5 => 3

    The generator does not guarantee that the best possible
    circular window is found; it returns the first solution
    found for the current search strategy.

    The constraint pairs are tried in this order (window, circular):

        w, c
        w, c-1
        w-1, c-1
        w-1, c-2
        ...

    The first successful result is returned.

    :param myrandom: random number generator
    :param n: Number of possible values, 0..n-1.
    :param window: Maximum linear window size.
    :param distinct: Size of each complete group whose values
        must be different.
    :param count: Number of values to generate.
    :param circular: If True, also require the circular window
            constraint. With False the lats value may even be
            same as first value
    :return: A generated list, or None if no solution can be found.
    """
    n, window, distinct = fix_parameters(n, window, distinct)

    if count <= 0:
        return []

    all_values = set(range(n))

    # Number of complete groups.
    full_count = (count // distinct) * distinct

    # The final complete group, or the final incomplete part,
    # is the part that may need relaxed window constraints.
    final_start = max(0, full_count - distinct)
    """
    if count % distinct == 0:
        # The final group is complete.
        final_start = max(0, full_count - distinct)
    else:
        # The final group is incomplete.
        final_start = full_count
    """

    state = GenerateState(
        myrandom=myrandom,
        n=n,
        window=window,
        distinct=distinct,
        count=count,
        circular=circular,
        all_values=all_values,
        full_count=full_count,
        final_start=final_start,
    )

    # ------------------------------------------------------------
    # Without circular optimization, simply complete the sequence
    # with the original window.
    # ------------------------------------------------------------

    if not circular:
        return generate_linearly(state, count)

    # ------------------------------------------------------------
    # Generate the part before the final group.
    #
    # This part must always use the original window.
    # - window = give some space for optimization in the final group
    # ------------------------------------------------------------

    fixed = generate_linearly(state, final_start)

    # ------------------------------------------------------------
    # Try progressively relaxed (w, c) pairs.
    #
    # Example for window, circular = 5:
    #
    #   5,5
    #   5,4
    #   4,4
    #   4,3
    #   3,3
    #   3,2
    #   2,2
    #   2,1
    #   1,1
    #
    # The first successful pair is returned.
    # ------------------------------------------------------------

    w = window
    c = window
    g_state = myrandom.getstate()

    while w > 0:
        candidate = complete(state, fixed[:], w, c)

        if candidate is not None:
            return candidate

        if w == c:
            c -= 1
        else:
            w -= 1

        # Ensure that the same random choices are made for each (w, c) pair.
        myrandom.setstate(g_state)

    return None


def main() -> None:
    # BYCODEBEGIN
    n = 5
    window = 5
    distinct = 4
    count = 7
    circular = bool(1)
    myrandom = Random(1644)
    # BYCODEEND
    results: list[list[int] | None] = []

    start = time.perf_counter()

    for _ in range(20):
        # myrandom = Random(1644)
        result = generate(
            myrandom,
            n,
            window,
            distinct,
            count,
            circular,
        )
        # count -= 1
        results.append(result)

    generate_time = time.perf_counter() - start

    start = time.perf_counter()

    for result in results:
        error = check(
            result,
            n,
            window,
            distinct,
            circular,
        )

        print(result, error or "")

    check_time = time.perf_counter() - start

    print()
    print(f"Generation time: {generate_time:.6f} s")
    print(f"Check time:      {check_time:.6f} s")
    print(f"Total time:      {generate_time + check_time:.6f} s")


if __name__ == "__main__":
    main()
