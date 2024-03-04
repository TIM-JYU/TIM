import shlex
import subprocess
from typing import Any, List, Optional

from cli.util.logging import log_debug


# Port from Python 3.8:
def sh_join(split_command: List[str]) -> str:
    return " ".join(shlex.quote(arg) for arg in split_command)


def run_cmd(
    args: List[str],
    check: bool = True,
    capture_output: Optional[bool] = None,
    **kwargs: Any,
) -> subprocess.CompletedProcess:
    log_debug(f"cmd: {sh_join(args)}")
    return subprocess.run(
        args,
        check=check,
        capture_output=capture_output if capture_output else False,
        **kwargs,
    )
