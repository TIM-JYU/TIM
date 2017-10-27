class TimDbException(Exception):
    """The exception that is thrown when an error occurs during a TimDb operation."""
    pass

class PreambleException(TimDbException):
    """Thrown when an error occurs related to preamble processing."""
    pass
