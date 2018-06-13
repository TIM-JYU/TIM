from sqlalchemy import func

from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.timdb.sqa import db


class ReadParagraph(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'readparagraph'
    id = db.Column(db.Integer, primary_key=True)
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), nullable=False)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    par_id = db.Column(db.Text, nullable=False)
    type = db.Column(db.Enum(ReadParagraphType), nullable=False)
    par_hash = db.Column(db.Text, nullable=False)
    timestamp = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    __table_args__ = (db.Index('readparagraph_doc_id_par_id_idx', 'doc_id', 'par_id'),
                      db.Index('readparagraph_doc_id_usergroup_id_idx', 'doc_id', 'usergroup_id'),)

    usergroup = db.relationship('UserGroup', back_populates='readparagraphs')
