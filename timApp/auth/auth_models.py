from __future__ import annotations

from datetime import datetime, timedelta
from typing import TYPE_CHECKING, Optional, List

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.auth.accesstype import AccessType
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.item.distribute_rights import Right
    from timApp.item.block import Block
    from timApp.user.usergroup import UserGroup

from timApp.timdb.sqa import include_if_loaded, db
from timApp.util.utils import get_current_time


class AccessTypeModel(db.Model):
    """A kind of access that a UserGroup may have to a Block."""

    __tablename__ = "accesstype"

    id: Mapped[int] = mapped_column(primary_key=True)
    """Access type identifier."""

    name: Mapped[str]
    """Access type name, such as 'view', 'edit', 'manage', etc."""

    accesses: Mapped[List["BlockAccess"]] = relationship(back_populates="atype")

    def __json__(self):
        return ["id", "name"]

    def to_enum(self):
        return AccessType(self.id)


class BlockAccess(db.Model):
    """A single permission. Relates a UserGroup with a Block along with an AccessType."""

    block_id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    usergroup_id: Mapped[int] = mapped_column(
        ForeignKey("usergroup.id"), primary_key=True
    )
    type: Mapped[int] = mapped_column(ForeignKey("accesstype.id"), primary_key=True)
    accessible_from: Mapped[Optional[datetime_tz]]
    accessible_to: Mapped[Optional[datetime_tz]]
    duration: Mapped[Optional[timedelta]]
    duration_from: Mapped[Optional[datetime_tz]]
    duration_to: Mapped[Optional[datetime_tz]]
    require_confirm: Mapped[Optional[bool]]
    restricted: Mapped[Optional[bool]]

    block: Mapped["Block"] = relationship(back_populates="accesses")
    usergroup: Mapped["UserGroup"] = relationship(back_populates="accesses")
    atype: Mapped["AccessTypeModel"] = relationship(back_populates="accesses")

    @property
    def duration_now(self):
        return get_duration_now(self, get_current_time())

    @property
    def group_collection_key(self):
        return self.block_id, self.type

    @property
    def block_collection_key(self):
        return self.usergroup_id, self.type

    @property
    def future(self):
        return (
            self.accessible_from is not None
            and get_current_time() < self.accessible_from
        )

    @property
    def access_type(self):
        return AccessType(self.type)

    @property
    def expired(self):
        return (
            self.accessible_to is not None and get_current_time() > self.accessible_to
        )

    @property
    def unlockable(self):
        return (
            self.accessible_from is None
            and self.duration is not None
            and not self.duration_future
            and not self.duration_expired
            and not self.require_confirm
        )

    @property
    def duration_future(self):
        return self.duration_from and get_current_time() < self.duration_from

    @property
    def duration_expired(self):
        return self.duration_to and get_current_time() >= self.duration_to

    @property
    def time_until_access_start(self) -> float | None:
        if self.accessible_from is None:
            return None
        return (self.accessible_from - get_current_time()).total_seconds()

    @property
    def seconds_left(self):
        if self.accessible_to is None:
            return None
        return (self.accessible_to - get_current_time()).total_seconds()

    @property
    def info_str(self):
        r = []
        if self.duration:
            r.append(f"duration={self.duration}")
        if self.duration_from:
            r.append(f"duration_from={self.duration_from.isoformat()}")
        if self.duration_to:
            r.append(f"duration_to={self.duration_to.isoformat()}")
        if self.require_confirm:
            r.append(f"require_confirm={self.require_confirm}")
        if self.accessible_from and self.future:
            r.append(f"accessible_from={self.accessible_from.isoformat()}")
        if self.expired:
            r.append("expired")
        elif self.accessible_to:
            r.append(f"accessible_to={self.accessible_to.isoformat()}")
        if self.restricted:
            r.append(f"restricted={self.restricted}")
        attrs = ",".join(r)
        return f"{self.access_type.name}({attrs})"

    def do_confirm(self) -> None:
        do_confirm(self, get_current_time())

    def __hash__(self):
        return hash(
            (
                self.block_id,
                self.usergroup_id,
                self.type,
                self.accessible_from,
                self.accessible_to,
                self.duration,
                self.duration_from,
                self.duration_to,
            )
        )

    def __eq__(self, other: BlockAccess):
        return (
            self.block_id == other.block_id
            and self.usergroup_id == other.usergroup_id
            and self.type == other.type
        )

    def __ne__(self, other):
        return not self == other

    def __repr__(self):
        return f"BlockAccess(usergroup={self.usergroup_id} perm={self.info_str})"

    def to_json(self):
        return {
            "block_id": self.block_id,
            "usergroup_id": self.usergroup_id,
            "type": self.type,
            "accessible_from": self.accessible_from,
            "accessible_to": self.accessible_to,
            "duration": self.duration,
            "duration_from": self.duration_from,
            "duration_to": self.duration_to,
            "require_confirm": self.require_confirm,
            "restricted": self.restricted,
            **include_if_loaded("usergroup", self),
        }


def get_duration_now(a: BlockAccess | Right, curr_time: datetime):
    if a.duration and a.accessible_to:
        return min(a.duration, a.accessible_to - curr_time)
    return a.duration


def do_confirm(a: BlockAccess | Right, curr_time: datetime):
    a.require_confirm = False
    # When confirming a distributable Right, it is possible that accessible_from is set.
    # In that case, we don't want to reset it.
    if not a.duration and not a.accessible_from:
        a.accessible_from = curr_time
