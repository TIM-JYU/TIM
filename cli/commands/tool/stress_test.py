import textwrap
from argparse import ArgumentParser

from cli.docker.run import run_compose

info = {
    "help": "Run stresstest on the TIM instance",
    "description": """
Run stresstest on the TIM instance by making n simultaneous wget calls.
        """,
}


class Arguments:
    calls: int
    doc_path: str


def run(args: Arguments) -> None:
    script_to_run = textwrap.dedent(
        rf"""
        dir=/tmp/stresstest/{args.doc_path}
        mkdir -p ${{dir}}
        rm ${{dir}}/* 2>/dev/null
        time (
        eval "for i in {{1..{args.calls}}};do wget --timeout=0 tim:5000/view/{args.doc_path} -O ${{dir}}/stresstest.\$i & done" 2>/dev/null
        wait
        )
        echo "Errors: $(ls -la ${{dir}} | grep -c "  0")"
        """
    )
    res = run_compose(["exec", "csplugin", "/bin/bash", "-c", script_to_run])
    exit(res.returncode)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--calls",
        help="Number of calls to make (Default: 1)",
        type=int,
        default=1,
    )
    parser.add_argument(
        "doc_path",
        help="Document path or document ID to stress test",
    )
