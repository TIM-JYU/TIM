from argparse import ArgumentParser

from cli.docker.run import run_compose

info = {"help": "Run unit tests"}


class Arguments:
    target: str
    down: bool


def cmd(args: Arguments) -> None:
    if args.target == "all":
        test_parameters = "discover -v tests/ 'test_*.py' ."
    elif "." in args.target:
        test_parameters = f"tests.{args.target}"
    else:
        test_parameters = f"discover -v tests/{args.target} 'test_*.py' ."

    res = run_compose(
        ["up", "--exit-code-from", "tests", "--abort-on-container-exit", "tests"],
        "test",
        override_profile=False,
        extra_env={
            "TEST_COMMAND": f"python3 -m unittest {test_parameters}",
        },
    )
    if args.down:
        run_compose(["down", "-t", "0"])
    exit(res.returncode)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--no-dc-down",
        help="Don't stop the all containers after the tests have finished.",
        action="store_false",
        dest="down",
    )
    parser.add_argument(
        "target",
        help="Run tests in the given group. "
        "Run a specific test module or test function. Format is <module>[.<function>]. "
        "Special value 'all' runs all tests.",
    )
    parser.set_defaults(run=cmd)
