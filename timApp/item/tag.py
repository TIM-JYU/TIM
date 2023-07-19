from enum import Enum, unique

from timApp.timdb.sqa import db


@unique
class TagType(Enum):
    """Type for a Tag."""

    Regular = 1
    """A regular tag."""

    CourseCode = 2
    """The Tag is a course code."""

    Subject = 3
    """The Tag is the name for a subject."""


class Tag(db.Model):
    """A tag with associated document id, tag name, type and expiration date."""

    __tablename__ = "tag"
    __allow_unmapped__ = True
    
    block_id = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    name = db.Column(db.Text, primary_key=True)
    type = db.Column(db.Enum(TagType), nullable=False)
    expires = db.Column(db.DateTime(timezone=True))

    block = db.relationship("Block", back_populates="tags")

    def __json__(self):
        return ["block_id", "name", "type", "expires"]

    @property
    def has_tag_special_chars(self):
        """
        Checks whether the tag name has characters other than (lower or upper case) a-ö,
        numbers 0-9, slashes, underscores, spaces or other allowed characters.
        characters.
        :return:
        """
        return set(self.name.lower()) - set(
            "abcdefghijklmnopqrstuvwxyzåäöü0123456789$€£#+*!@%&().:;/- _"
        )

    def get_group_name(self) -> str | None:
        if self.name.startswith(GROUP_TAG_PREFIX):
            return self.name[len(GROUP_TAG_PREFIX) :]
        return None


GROUP_TAG_PREFIX = "group:"
