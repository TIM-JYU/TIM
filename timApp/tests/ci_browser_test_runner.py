"""
Special browser test runner for CI.

Right now, CI tests are run in a Docker container on CI.
This appears to cause problems with Selenium + Flask in Python's own unittest.
See https://github.com/TIM-JYU/TIM/issues/2559

The solution is to run each test in a separate, unforked process and force each process to properly shut down and close
all collections.
Moreover, we

* run each test max. 5 times before failing, and
* add max. 3 minute timeout for each test.

"""
import os
import subprocess

if __name__ == "__main__":
    # Find all test_ files in browser folder
    test_files = [f for f in os.listdir("tests/browser") if f.startswith("test_")]
    MAX_TRIES = 5
    for test_file in test_files:
        # Run each test file
        cur_try = 0
        while True:
            try:
                res = subprocess.run(
                    [
                        "python3",
                        "-m",
                        "unittest",
                        "discover",
                        "-v",
                        "tests/browser",
                        f"{test_file}",
                    ],
                    timeout=3 * 60,
                )

                if res.returncode != 0:
                    cur_try += 1
                    if cur_try >= MAX_TRIES:
                        print(f"{test_file} failed {MAX_TRIES} times")
                        exit(1)
                    print(f"{test_file} failed, retrying")
                    continue
                break

            except subprocess.TimeoutExpired as e:
                print(f"Timed out, retrying: {e}")
                cur_try += 1
                if cur_try >= MAX_TRIES:
                    print("Timed out, giving up")
                    exit(1)
