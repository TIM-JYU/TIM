from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class RowOwnerInfo(db.Model):
    """
    Information about the owner of a TimTable row. Includes document and paragraph
    id for determining the TimTable instance.
    """

    __tablename__ = "rowownerinfo"
    

    doc_id = mapped_column(db.Integer, primary_key=True)
    par_id = mapped_column(db.Text, primary_key=True)
    unique_row_id = mapped_column(db.Integer, primary_key=True)
    usergroup_id = mapped_column(
        db.Integer, db.ForeignKey("usergroup.id"), primary_key=False
    )

    # usergroup = db.relationship('UserGroup', back_populates='rowOwnerInfo')
    # block = db.relationship('Block', back_populates='tags')

    def __json__(self):
        return ["doc_id", "par_id", "unique_row_id", "usergroup_id"]
