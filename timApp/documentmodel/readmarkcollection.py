from typing import List

from timApp.timdb.tim_models import ReadParagraph


class ReadMarkCollection:

    def __init__(self) -> None:
        self.marks: List[ReadParagraph] = []

    def add(self, r: ReadParagraph, modified=False):
        self.marks.append(r)
        r.modified = modified

    def class_str(self):
        return ' '.join(self.yield_classes())

    def yield_classes(self):
        yield 'readline'
        yield from (r.type.class_str() + ('-modified' if r.modified else '') for r in self.marks)
