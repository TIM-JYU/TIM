from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class Question(db.Model):
    __tablename__ = "question"
    

    question_id = mapped_column(db.Integer, primary_key=True)
    doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), nullable=False)
    par_id = mapped_column(db.Text, nullable=False)
    question_title = mapped_column(db.Text, nullable=False)
    answer = mapped_column(db.Text)
    questionjson = mapped_column(db.Text)
    points = mapped_column(db.Text)
    expl = mapped_column(db.Text)
