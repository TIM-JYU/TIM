import argparse
import importlib
import os
from typing import Any, List, Dict

from util.iter import pairwise


def main():
    commands_path = os.path.realpath(
        os.path.join(os.path.dirname(__file__), "commands")
    )
    import_commands: List[List[str]] = []
    for dirpath, dirnames, filenames in os.walk(commands_path):
        if "__pycache__" in dirpath:
            continue
        for filename in filenames:
            filename_no_ext, _ = os.path.splitext(filename)
            if filename_no_ext == "__init__":
                continue
            relative_path = os.path.relpath(dirpath, commands_path).strip(".")
            module_parts = [p for p in relative_path.split(os.path.pathsep) if p]
            module_parts.append(filename_no_ext)
            import_commands.append(module_parts)

    main_parser = argparse.ArgumentParser(prog="tim")
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
                init_module = importlib.import_module(f"commands.{init_module_path}")
                info = getattr(init_module, "info", {})
                subparsers_tree[cur] = (
                    subparsers_tree[prev]
                    .add_parser(cur, **info)
                    .add_subparsers(
                        title="commands",
                        description="Available commands",
                        help="Additional help",
                    )
                )
        module = importlib.import_module(f"commands.{module_name}")
        command_info = getattr(module, "info", {})
        init_func = getattr(module, "init", None)
        parser = subparsers_tree[search_path[-1]].add_parser(
            module_parts[-1], **command_info
        )
        if init_func:
            init_func(parser)

    args = main_parser.parse_args()
    if hasattr(args, "func"):
        args.func(args)
    else:
        print("No command specified")
        main_parser.print_help()


if __name__ == "__main__":
    main()
