from typing import Optional

from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db


class Question(db.Model):
    __tablename__ = "question"

    question_id: Mapped[int] = mapped_column(primary_key=True)
    doc_id: Mapped[int] = mapped_column(db.ForeignKey("block.id"))
    par_id: Mapped[str]
    question_title: Mapped[str]
    answer: Mapped[Optional[str]]
    questionjson: Mapped[Optional[str]]
    points: Mapped[Optional[str]]
    expl: Mapped[Optional[str]]
