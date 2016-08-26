# -*- coding: utf-8 -*-
"""Functions for dealing takin time."""
import time

timing_last = time.clock()


def taketime(s1="", s2="", n=0):
    global timing_last
    t22 = time.clock()
    print("%-15s %-10s %6d - %7.4f" % (s1, s2, n, (t22 - timing_last)))
    timing_last = t22
