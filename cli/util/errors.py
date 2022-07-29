class CLIError(Exception):
    """Base class for all CLI errors."""

    def __init__(self, message: str, code: int = 1) -> None:
        self.message = message
        self.code = code
