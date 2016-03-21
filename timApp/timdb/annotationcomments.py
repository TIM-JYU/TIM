from contracts import contract
from timdb.timdbbase import TimDbBase

class AnnotationAnswers(TimDbBase):
    @contract
    def add_comment(self):
        return