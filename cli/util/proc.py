import shlex
import subprocess
from typing import Any, List

from cli.util.logging import log_debug


def cmd(
    args: List[str], check: bool = True, **kwargs: Any
) -> subprocess.CompletedProcess:
    log_debug(f"cmd: {shlex.join(args)}")
    return subprocess.run(args, check=check, **kwargs)
