"""
Functions for calling pandoc and constructing the calls
"""
import subprocess


def call_pandoc(markdown: str) -> bytes:
    """
    Calls for a new system subprocess to run pandoc.

    :param markdown: The documents markdown
    :return: LaTeX produced by pandoc
    """

    def construct_call() -> [str]:
        """
        Constructs the subprocess call with the given options

        """

        call = []
        call.append("pandoc")
        for opt in get_print_options():
            call.append(opt)
        return call

    try:
        return subprocess.check_output(construct_call())
    except OSError as e:
        return bytes("Requested command does not exist", 'utf-8')


def get_print_options() -> [str]:
    # TODO: actually get the printing options,
    # needs to have the printing opts implemented in the system first
    return ["-t latex"]

