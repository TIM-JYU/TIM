from contracts import contract
from timdb.timdbbase import TimDbBase

class Annotation(TimDbBase):
    @contract
    def create_annotation(self):
        return