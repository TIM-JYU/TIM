import json
from typing import Any, Optional, List, TYPE_CHECKING

from sqlalchemy import func, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship, DynamicMapped

from timApp.answer.answer_models import UserAnswer, AnswerUpload
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import include_if_loaded, db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.velp.annotation_model import Annotation
    from timApp.plugin.plugintype import PluginType


class AnswerSaver(db.Model):
    """Holds information about who has saved an answer. For example, in teacher view, "Save teacher's fix"
    would store the teacher in this table.
    """

    answer_id: Mapped[int] = mapped_column(ForeignKey("answer.id"), primary_key=True)
    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), primary_key=True)


class Answer(db.Model):
    """An answer to a task."""

    id: Mapped[int] = mapped_column(primary_key=True)
    """Answer identifier."""

    task_id: Mapped[str] = mapped_column(index=True)
    """Task id to which this answer was posted. In the form "doc_id.name", for example "2.task1"."""

    origin_doc_id: Mapped[Optional[int]] = mapped_column(ForeignKey("block.id"))
    """The document in which the answer was saved"""

    plugin_type_id: Mapped[Optional[int]] = mapped_column(ForeignKey("plugintype.id"))
    """Plugin type the answer was saved on"""

    content: Mapped[str]
    """Answer content."""

    points: Mapped[Optional[float]]
    """Points."""

    answered_on: Mapped[datetime_tz] = mapped_column(default=func.now())
    """Answer timestamp."""

    valid: Mapped[bool]
    """Whether this answer is valid."""

    last_points_modifier: Mapped[Optional[int]] = mapped_column(
        ForeignKey("usergroup.id")
    )
    """The UserGroup who modified the points last. Null if the points have been given by the task automatically."""

    plugin_type: Mapped["PluginType"] = relationship(lazy="select")
    uploads: Mapped[List["AnswerUpload"]] = relationship(
        back_populates="answer", lazy="dynamic"
    )
    users: DynamicMapped["User"] = relationship(
        secondary=UserAnswer.__table__, back_populates="answers", lazy="dynamic"
    )
    users_all: Mapped[List["User"]] = relationship(
        secondary=UserAnswer.__table__,
        back_populates="answers_alt",
        order_by="User.real_name",
        lazy="select",
        overlaps="users",
    )
    annotations: Mapped[List["Annotation"]] = relationship(back_populates="answer")
    saver: Mapped["User"] = relationship(lazy="select", secondary=AnswerSaver.__table__)

    @property
    def content_as_json(self) -> dict:
        return json.loads(self.content)

    def get_answer_number(self) -> int:
        u: User | None = self.users.first()
        if not u:
            return 1
        return u.get_answers_for_task(self.task_id).filter(Answer.id <= self.id).count()

    @property
    def task_name(self) -> str:
        return self.parsed_task_id.task_name

    def to_json(self) -> dict:
        return {
            "id": self.id,
            "task_id": self.task_id,
            "content": self.content,
            "points": self.points,
            "answered_on": self.answered_on,
            "valid": self.valid,
            "last_points_modifier": self.last_points_modifier,
            "origin_doc_id": self.origin_doc_id,
            **include_if_loaded("plugin_type", self, "plugin"),
            **include_if_loaded("users_all", self, "users"),
        }

    @property
    def parsed_task_id(self) -> TaskId:
        return TaskId.parse(
            self.task_id,
            require_doc_id=True,
            allow_block_hint=False,
            allow_custom_field=False,
            allow_type=False,
        )

    @property
    def has_many_collaborators(self) -> bool:
        return len(self.users_all) > 1


AnswerData = dict[str, Any]
