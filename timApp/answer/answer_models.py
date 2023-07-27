from typing import TYPE_CHECKING, Optional

from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db

if TYPE_CHECKING:
    from timApp.item.block import Block
    from timApp.answer.answer import Answer


class AnswerTag(db.Model):
    """Tags for an Answer.

    TODO: Answer should be a Block and the tags would then come from the tag table.
    """

    __tablename__ = "answertag"

    id: Mapped[int] = mapped_column(primary_key=True)
    answer_id: Mapped[int] = mapped_column(db.ForeignKey("answer.id"))
    tag: Mapped[str]


class AnswerUpload(db.Model):
    """Associates uploaded files (Block with type BlockType.AnswerUpload) with Answers."""

    __tablename__ = "answerupload"

    upload_block_id: Mapped[int] = mapped_column(
        db.ForeignKey("block.id"), primary_key=True
    )
    answer_id: Mapped[int] = mapped_column(db.ForeignKey("answer.id"))

    block: Mapped["Block"] = db.relationship(back_populates="answerupload")
    answer: Mapped["Answer"] = db.relationship(back_populates="uploads")

    def __init__(self, block, answer=None):
        self.block = block
        self.answer = answer


class UserAnswer(db.Model):
    """Associates Users with Answers."""

    __tablename__ = "useranswer"

    id: Mapped[int] = mapped_column(primary_key=True)
    answer_id: Mapped[Optional[int]] = mapped_column(db.ForeignKey("answer.id"))
    user_id: Mapped[Optional[int]] = mapped_column(db.ForeignKey("useraccount.id"))

    __table_args__ = (db.UniqueConstraint("answer_id", "user_id"),)
