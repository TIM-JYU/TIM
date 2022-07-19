import argparse
import importlib
import os
import subprocess
import traceback
from typing import Any, List, Dict

from cli.util.errors import CLIError
from cli.util.iter import pairwise
from cli.util.logging import log_error, enable_verbose, log_debug


def main() -> None:
    commands_path = os.path.realpath(
        os.path.join(os.path.dirname(__file__), "commands")
    )
    import_commands: List[List[str]] = []
    for dir_path, dir_names, filenames in os.walk(commands_path):
        if "__pycache__" in dir_path:
            continue
        for filename in filenames:
            filename_no_ext, _ = os.path.splitext(filename)
            if filename_no_ext == "__init__":
                continue
            relative_path = os.path.relpath(dir_path, commands_path).strip(".")
            module_parts = [p for p in relative_path.split(os.path.sep) if p]
            module_parts.append(filename_no_ext)
            import_commands.append(module_parts)

    main_parser = argparse.ArgumentParser(
        prog="tim", description="Manage the current TIM instance"
    )
    main_parser.add_argument(
        "--verbose",
        "-v",
        help="Enable verbose logging",
        action="store_true",
        dest="logging_verbose",
    )
    subparsers = main_parser.add_subparsers(
        title="commands", description="Available commands", help="Additional help"
    )

    subparsers_tree: Dict[str, Any] = {
        ".": subparsers,
    }
    for module_parts in import_commands:
        module_name = ".".join(module_parts)
        search_path = [".", *module_parts[:-1]]
        cur_module_path = []
        for prev, cur in pairwise(search_path):
            cur_module_path.append(cur)
            if cur not in subparsers_tree:
                init_module_path = ".".join(cur_module_path)
                init_module = importlib.import_module(
                    f"cli.commands.{init_module_path}"
                )
                info = getattr(init_module, "info", {})
                subparsers_tree[cur] = (
                    subparsers_tree[prev]
                    .add_parser(cur.replace("_", "-"), **info)
                    .add_subparsers(
                        title="commands",
                        description="Available commands",
                        help="Additional help",
                    )
                )
        module = importlib.import_module(f"cli.commands.{module_name}")
        command_info = getattr(module, "info", {})
        init_func = getattr(module, "init", None)
        command_name = module_parts[-1].replace("_", "-")
        parser = subparsers_tree[search_path[-1]].add_parser(
            command_name, **command_info
        )
        if init_func:
            init_func(parser)

    args = main_parser.parse_args()
    if args.logging_verbose:
        enable_verbose()
    if hasattr(args, "run"):
        try:
            args.run(args)
        except CLIError as e:
            log_error(e.message)
            exit(e.code)
        except KeyboardInterrupt:
            exit(0)
        except subprocess.CalledProcessError as e:
            log_debug(traceback.format_exc())
            log_error(str(e))
            exit(e.returncode)
    else:
        log_error("No command specified, use --help for more information")
        main_parser.print_help()


if __name__ == "__main__":
    main()
