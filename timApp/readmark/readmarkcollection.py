from dataclasses import dataclass, field
from typing import List

from timApp.readmark.readparagraph import ReadParagraph


@dataclass
class ReadMarkCollection:
    marks: List[ReadParagraph] = field(default_factory=list)

    def add(self, r: ReadParagraph, modified=False):
        self.marks.append(r)
        r.modified = modified

    def class_str(self):
        s = 'readline'
        for c in (r.type.class_str() + ('-modified' if r.modified else '') for r in self.marks):
            s += ' ' + c
        return s
