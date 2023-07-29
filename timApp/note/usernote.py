from typing import Optional, TYPE_CHECKING

from sqlalchemy import func, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz, DbModel

if TYPE_CHECKING:
    from timApp.user.usergroup import UserGroup
    from timApp.item.block import Block


class UserNote(DbModel):
    """A comment/note that has been posted in a document paragraph."""

    __tablename__ = "usernotes"

    id: Mapped[int] = mapped_column(primary_key=True)
    """Comment id."""

    usergroup_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"))
    """The UserGroup id who posted the comment."""

    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"))
    """The document id in which this comment was posted."""

    par_id: Mapped[str]
    """The paragraph id in which this comment was posted."""

    par_hash: Mapped[str]
    """The paragraph hash at the time this comment was posted."""

    content: Mapped[str]
    """Comment content."""

    created: Mapped[datetime_tz] = mapped_column(default=func.now())
    """Comment creation timestamp."""

    modified: Mapped[Optional[datetime_tz]]
    """Comment modification timestamp."""

    access: Mapped[str]
    """Who can see this comment. So far valid values are 'everyone' and 'justme'."""

    tags: Mapped[str]
    """Tags for the comment."""

    html: Mapped[Optional[str]]
    """Comment HTML cache."""

    usergroup: Mapped["UserGroup"] = relationship(back_populates="notes")
    block: Mapped["Block"] = relationship()

    @property
    def is_public(self) -> bool:
        return self.access == "everyone"

    def to_json(self):
        tr = self.block.translation
        d = tr if tr else self.block.docentries[0]
        return {
            "id": self.id,
            "doc_id": self.doc_id,
            "doc_title": d.title,
            "par_id": self.par_id,
            "par_hash": self.par_hash,
            "content": self.content,
            "created": self.created,
            "modified": self.modified,
            "access": self.access,
            "usergroup": self.usergroup,
            "url": d.url + "#" + self.par_id,
        }


def get_comment_by_id(c_id: int) -> UserNote | None:
    return db.session.get(UserNote, c_id)
