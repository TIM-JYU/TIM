# -*- coding: utf-8 -*-
"""Functions for dealing taking time."""
import inspect
import time
from functools import wraps
from typing import Callable, Any

timing_last = time.perf_counter()
timing_last_t = time.time()
timing_last_z = time.perf_counter()
timing_last_t_z = time.time()


# print(timing_last, timing_last_t)


def taketime(s1: str = "", s2: str = "", n: int = 0, zero: bool = False) -> None:
    return  # comment this to take times, uncomment for production and tests
    global timing_last
    global timing_last_t
    global timing_last_z
    global timing_last_t_z
    t22 = time.perf_counter()
    t22t = time.time()
    if zero:
        timing_last_z = t22
        timing_last_t_z = t22t

    print("%-20s %-15s %6d - %7.4f %7.4f %7.4f %7.4f" % (
    s1, s2, n, (t22 - timing_last), (t22t - timing_last_t), (t22 - timing_last_z), (t22t - timing_last_t_z)))
    timing_last = t22
    timing_last_t = t22t


def with_timing(*print_args: str) -> Callable:
    """A decorator for printing timing info for a function. Usage example::

        @with_timing('name')
        def f(name, some_other_param):
            ...

    :param print_args: Names of parameters whose values should be printed to help differentiate calls.
    """

    def with_timing_dec(f: Callable) -> Callable:
        sig_params = inspect.signature(f).parameters

        @wraps(f)
        def wrapper(*args: Any, **kwargs: Any) -> Callable:
            time_before = time.time()
            result = f(*args, **kwargs)
            all_params = {
                k: args[n] if n < len(args) else kwargs[k]
                for n, k in enumerate(sig_params.keys())
            }
            args_str = ', '.join(f'{name}={all_params[name]}' for name in print_args)
            print(f'{f.__name__}: {time.time() - time_before:.4g} {args_str}')
            return result

        return wrapper

    return with_timing_dec
