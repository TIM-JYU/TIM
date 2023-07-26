from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class UserGroupDoc(db.Model):
    """Each UserGroup can have at most one administrative document. The rights of that document determine who can see
    and edit the members of the UserGroup.
    """

    __tablename__ = "usergroupdoc"
    

    group_id = mapped_column(db.Integer, db.ForeignKey("usergroup.id"), primary_key=True)
    doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
