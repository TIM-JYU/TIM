from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db


class UserGroupDoc(db.Model):
    """Each UserGroup can have at most one administrative document. The rights of that document determine who can see
    and edit the members of the UserGroup.
    """

    group_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), primary_key=True)
    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
