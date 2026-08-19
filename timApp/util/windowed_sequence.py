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
        return True

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
    :param  n: numebr of ints
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
            return f"same number in window before final group: " f"{values}"

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
    final_start = max(
        0,
        full_count - distinct,
    )

    result: list[int] = []

    def get_available(
        prefix: list[int],
        current_window: int,
    ) -> set[int]:
        """
        Return values that can be appended to prefix.

        Windows which start before the final group must satisfy
        the original window. Windows starting inside the final
        group use current_window.

        :param prefix: Part of the sequence constructed so far.
        :param current_window: Window currently being considered.
        :returns: Values that can be appended to prefix.
        """
        position = len(prefix)

        avail = set(all_values)

        # The new value may complete an original window that starts
        # before the final group.
        if position >= window - 1:
            window_start = position - window + 1

            if window_start < final_start:
                avail -= set(prefix[-(window - 1) :])

        # The new value may also complete the relaxed window.
        if position >= current_window - 1:
            window_start = position - current_window + 1

            if window_start >= final_start:
                avail -= set(prefix[-(current_window - 1) :])

        # The distinct constraint applies to complete groups.
        if position < full_count:
            group_start = (position // distinct) * distinct
            avail -= set(prefix[group_start:position])

        return avail

    def circular_ok(
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
        """
        pos = len(prefix)

        # Only the last value can complete windows crossing
        # from the end of the sequence back to the beginning.
        if pos != count - 1:
            return True

        sequence = prefix + [value]

        for start in range(count - cir + 1, count):
            circ_values = [sequence[(start + offset) % count] for offset in range(cir)]

            if len(set(circ_values)) != cir:
                return False

        return True

    def complete(
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

        :param prefix: Part of the sequence constructed so far.
        :param win: Size of the linear window.
        :param cir: Size of the circular window.
        """
        if len(prefix) == count:
            if not circular:
                return prefix

            if check_circular(prefix, cir):
                return prefix

            return None

        avail = get_available(prefix, win)

        if circular:
            avail = {value for value in avail if circular_ok(prefix, value, cir)}

        if not avail:
            return None

        values = list(avail)
        myrandom.shuffle(values)

        for value in values:
            cand = complete(
                prefix + [value],
                win,
                cir,
            )

            if cand is not None:
                return cand

        return None

    # ------------------------------------------------------------
    # Generate the part before the final group.
    #
    # This part must always use the original window.
    # ------------------------------------------------------------

    while len(result) < final_start:
        available = get_available(
            result,
            window,
        )

        if not available:
            return None

        result.append(myrandom.choice(tuple(available)))

    fixed = result[:]

    # ------------------------------------------------------------
    # Without circular optimization, simply complete the sequence
    # with the original window.
    # ------------------------------------------------------------

    if not circular:
        return complete(
            fixed,
            window,
            window,
        )

    # ------------------------------------------------------------
    # Try progressively relaxed (w, c) pairs.
    #
    # Example for window = 5:
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

    while w > 0:
        candidate = complete(
            fixed[:],
            w,
            c,
        )

        if candidate is not None:
            return candidate

        if w == c:
            c -= 1
        else:
            w -= 1

    return None


def main() -> None:
    # BYCODEBEGIN
    n = 5
    window = 3
    distinct = 4
    count = 10
    circular = True
    myrandom = Random(1615)
    # BYCODEEND

    results: list[list[int] | None] = []

    start = time.perf_counter()

    for _ in range(20):
        result = generate(
            myrandom,
            n,
            window,
            distinct,
            count,
            circular,
        )

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
