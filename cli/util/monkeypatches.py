from argparse import ArgumentParser, Namespace
from typing import List, Optional, Sequence, Tuple


def patch_argparse_add_parser_info() -> None:
    """
    Monkeypatch the argparse.ArgumentParser.parse_known_args method
    to add the current parser's info to the namespace as `last_parser` attribute.
    The injected parser is latest that was successfully matched.

    This is useful for e.g. printing help for the last matched (sub)parser.
    """
    parser = ArgumentParser.parse_known_args

    def parse_known_args_new(
        self: ArgumentParser,
        args: Optional[Sequence[str]] = None,
        namespace: Optional[Namespace] = None,
    ) -> Tuple[Namespace, List[str]]:
        if namespace is None:
            namespace = Namespace()
        setattr(namespace, "last_parser", self)
        return parser(self, args, namespace)

    ArgumentParser.parse_known_args = parse_known_args_new  # type: ignore
