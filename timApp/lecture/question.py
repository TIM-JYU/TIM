from timApp.timdb.sqa import db


class Question(db.Model):
    __tablename__ = "question"
    __allow_unmapped__ = True
    
    question_id = db.Column(db.Integer, primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey("block.id"), nullable=False)
    par_id = db.Column(db.Text, nullable=False)
    question_title = db.Column(db.Text, nullable=False)
    answer = db.Column(db.Text)
    questionjson = db.Column(db.Text)
    points = db.Column(db.Text)
    expl = db.Column(db.Text)
