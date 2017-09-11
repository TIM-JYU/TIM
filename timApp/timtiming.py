# -*- coding: utf-8 -*-
"""Functions for dealing taking time."""
import time

timing_last = time.clock()
timing_last_t = time.time()
# print(timing_last, timing_last_t)


def taketime(s1="", s2="", n=0):
    return  # comment this to take times, uncomment for production and tests
    global timing_last
    global timing_last_t
    t22 = time.clock()
    t22t = time.time()
    print("%-20s %-10s %6d - %7.4f %7.4f" % (s1, s2, n, (t22 - timing_last), (t22t - timing_last_t)))
    timing_last = t22
    timing_last_t = t22t
