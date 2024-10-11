import os
from argparse import ArgumentParser
from string import Template
from time import sleep
from typing import Optional, Tuple

from cli.config import get_config
from cli.docker.run import run_compose
from cli.util.errors import CLIError
from cli.util.logging import log_debug
from cli.util.proc import sh_join

info = {"help": "Run unit tests"}


class Arguments:
    target: str
    down: bool
    up: bool
    new_screenshots: bool
    coverage: bool
    chunk: Optional[Tuple[int, int]]


BROWSER_TEST_SCRIPT = Template(
    """
import os
import subprocess

chunk = $chunk
test_files = sorted([f for f in os.listdir("tests/browser") if f.startswith("test_")])

if chunk:
    chunk_current, chunk_total = chunk
    chunk_size = len(test_files) // chunk_total
    overflow = 0
    if chunk_current == chunk_total:
        overflow = len(test_files) % chunk_total

    test_files = test_files[(chunk_current - 1) * chunk_size : (chunk_current) * chunk_size + overflow]

MAX_TRIES = 3
for test_file in test_files:
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
"""
)


def run(args: Arguments) -> None:
    config = get_config()
    if args.up or config.profile != "test":
        run_compose(["up", "-d", "--quiet-pull"], "test")
        # Wait for the containers to be up for a small moment
        sleep(5)

    if args.target == "all":
        test_parameters = ["discover", "-v", "tests/", "test_*.py", "."]
    elif "." in args.target:
        test_parameters = [f"tests.{args.target}"]
    else:
        test_parameters = ["discover", "-v", f"tests/{args.target}", "test_*.py", "."]

    base_command = ["python3"] if not args.coverage else ["coverage", "run"]
    test_command = [*base_command, "-m", "unittest", *test_parameters]

    # Browser tests can be flaky (in part of lackluster flask support for Selenium, in part of the tests).
    # It's better to retry it a few times
    if args.target == "browser":
        test_command = [
            "python3",
            "-c",
            BROWSER_TEST_SCRIPT.safe_substitute(
                chunk=f"({args.chunk[0]}, {args.chunk[1]})" if args.chunk else "None"
            ),
        ]

    test_command_joined = sh_join(test_command)
    log_debug(f"test_command: {test_command_joined}")

    env = {
        "TEST_COMMAND": test_command_joined,
        "SKIP_JSRUNNER_START": os.environ.get("SKIP_JSRUNNER_START", "false"),
    }
    if args.new_screenshots:
        env["NEW_SCREENSHOTS"] = "1"

    res = run_compose(
        ["up", "--exit-code-from", "tests", "--attach", "tests", "tests"],
        "test",
        override_profile=False,
        extra_env=env,
    )
    if args.down:
        run_compose(["down", "-t", "0"])
    exit(res.returncode)


def parse_chunk(chunk: str) -> Optional[Tuple[int, int]]:
    if not chunk:
        return None

    try:
        current, total = map(int, chunk.split("/"))
        if current <= 0 or total <= 0:
            raise CLIError("--chunk: values must be positive integers.")
        if current > total:
            raise CLIError(
                "--chunk: current chunk must be less than or equal to the total number of chunks."
            )
        return current, total
    except ValueError:
        raise CLIError(
            "--chunk must be in format N/M where N is the current chunk to test and M is the total number of chunks."
        )


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--dc-up",
        help="Run docker-compose up before running tests. This is useful for ensuring all services are online.",
        action="store_true",
        dest="up",
    )
    parser.add_argument(
        "--no-dc-down",
        help="Don't stop the all containers after the tests have finished.",
        action="store_false",
        dest="down",
    )
    parser.add_argument(
        "--new-screenshots",
        help="Create new screenshots for all browser tests.",
        action="store_true",
        dest="new_screenshots",
    )
    parser.add_argument(
        "--coverage",
        help="Run tests with coverage",
        action="store_true",
        dest="coverage",
    )
    parser.add_argument(
        "--chunk",
        help="Run tests in chunks for tests that support it. Useful for parallelizing tests. Allowed format: N/M where N is the current chunk and M is the total number of chunks.",
        default="",
        type=parse_chunk,
        metavar="N/M",
    )
    parser.add_argument(
        "target",
        help="Run tests in the given group. "
        "Run a specific test module or test function. Format is <module>[.<function>]. "
        "Special value 'all' runs all tests.",
    )
