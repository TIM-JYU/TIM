class DocExistsError(Exception):
    def __init__(self, doc_id):
        self.doc_id = doc_id

    def __str__(self):
        return f"Document already exists: {self.doc_id}"


class ValidationException(Exception):
    pass


class ValidationWarning(ValidationException):
    pass


class AttributesAtEndOfCodeBlockException(ValidationException):
    pass
