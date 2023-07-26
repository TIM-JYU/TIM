from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class AnswerTag(db.Model):
    """Tags for an Answer.

    TODO: Answer should be a Block and the tags would then come from the tag table.
    """

    __tablename__ = "answertag"
    

    id = mapped_column(db.Integer, primary_key=True)
    answer_id = mapped_column(db.Integer, db.ForeignKey("answer.id"), nullable=False)
    tag = mapped_column(db.Text, nullable=False)


class AnswerUpload(db.Model):
    """Associates uploaded files (Block with type BlockType.AnswerUpload) with Answers."""

    __tablename__ = "answerupload"
    

    upload_block_id = mapped_column(
        db.Integer, db.ForeignKey("block.id"), primary_key=True
    )
    answer_id = mapped_column(db.Integer, db.ForeignKey("answer.id"))

    block = db.relationship("Block", back_populates="answerupload")
    answer = db.relationship("Answer", back_populates="uploads")

    def __init__(self, block, answer=None):
        self.block = block
        self.answer = answer


class UserAnswer(db.Model):
    """Associates Users with Answers."""

    __tablename__ = "useranswer"
    

    id = mapped_column(db.Integer, primary_key=True)
    answer_id = mapped_column(db.Integer, db.ForeignKey("answer.id"), nullable=False)
    user_id = mapped_column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    __table_args__ = (db.UniqueConstraint("answer_id", "user_id"),)
