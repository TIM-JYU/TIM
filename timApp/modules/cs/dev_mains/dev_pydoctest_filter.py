import sys
import os

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from filters_to_convert_test_output import convert_pydoctest_verbose

if __name__ == "__main__":
    # Esimerkki tiedosto testattavaksi
    verbose_output = """
Trying:
    double(-1)
Expecting:
    -2
**********************************************************************
File "/temp/py/double.py", line 7, in double.double
Failed example:
    double(-1)
Expected:
    -2
Got:
    2
Trying:
    double(0)
Expecting:
    0
**********************************************************************
File "/temp/py/double.py", line 9, in double.double
Failed example:
    double(0)
Expected:
    0
Got:
    2
Trying:
    double(4)
Expecting:
    8
**********************************************************************
File "/temp/py/double.py", line 11, in double.double
Failed example:
    double(4)
Expected:
    8
Got:
    2
1 item had no tests:
    double
**********************************************************************
1 item had failures:
   3 of   3 in double.double
3 tests in 2 items.
0 passed and 3 failed.
***Test Failed*** 3 failures.
"""

    # Suodata tuloste
    compact_output = convert_pydoctest_verbose(verbose_output)

    print("\n=== COMPACT OUTPUT ===")
    print(compact_output)
