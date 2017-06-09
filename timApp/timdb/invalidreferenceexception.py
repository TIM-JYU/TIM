from timApp.timdb.timdbexception import TimDbException


class InvalidReferenceException(TimDbException):
    """The exception that is thrown when a referenced paragraph or area is not found."""
    pass
