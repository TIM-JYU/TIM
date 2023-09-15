from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db


class Question(db.Model):
    question_id: Mapped[int] = mapped_column(primary_key=True)
    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"))
    par_id: Mapped[str]
    question_title: Mapped[str]
    answer: Mapped[Optional[str]]
    questionjson: Mapped[Optional[str]]
    points: Mapped[Optional[str]]
    expl: Mapped[Optional[str]]
