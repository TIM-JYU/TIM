import json
from typing import Any

from sqlalchemy import func
from sqlalchemy.orm import mapped_column

from timApp.answer.answer_models import UserAnswer
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db, include_if_loaded


class AnswerSaver(db.Model):
    """Holds information about who has saved an answer. For example, in teacher view, "Save teacher's fix"
    would store the teacher in this table.
    """

    __tablename__ = "answersaver"
    

    answer_id = mapped_column(db.Integer, db.ForeignKey("answer.id"), primary_key=True)
    user_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), primary_key=True
    )


class Answer(db.Model):
    """An answer to a task."""

    __tablename__ = "answer"
    

    id = mapped_column(db.Integer, primary_key=True)
    """Answer identifier."""

    task_id = mapped_column(db.Text, nullable=False, index=True)
    """Task id to which this answer was posted. In the form "doc_id.name", for example "2.task1"."""

    origin_doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), nullable=True)
    """The document in which the answer was saved"""

    plugin_type_id = mapped_column(
        db.Integer, db.ForeignKey("plugintype.id"), nullable=True
    )
    """Plugin type the answer was saved on"""

    content = mapped_column(db.Text, nullable=False)
    """Answer content."""

    points = mapped_column(db.Float)
    """Points."""

    answered_on = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=func.now()
    )
    """Answer timestamp."""

    valid = mapped_column(db.Boolean, nullable=False)
    """Whether this answer is valid."""

    last_points_modifier = mapped_column(db.Integer, db.ForeignKey("usergroup.id"))
    """The UserGroup who modified the points last. Null if the points have been given by the task automatically."""

    plugin_type = db.relationship("PluginType", lazy="select") # : PluginType | None
    uploads = db.relationship("AnswerUpload", back_populates="answer", lazy="dynamic")
    users = db.relationship(
        "User", secondary=UserAnswer.__table__, back_populates="answers", lazy="dynamic"
    )
    users_all = db.relationship(
        "User",
        secondary=UserAnswer.__table__,
        back_populates="answers_alt",
        order_by="User.real_name",
        lazy="select",
        overlaps="users",
    )
    annotations = db.relationship("Annotation", back_populates="answer")
    saver = db.relationship(
        "User", lazy="select", secondary=AnswerSaver.__table__, uselist=False
    )

    @property
    def content_as_json(self) -> dict:
        return json.loads(self.content)

    def get_answer_number(self) -> int:
        u = self.users.first()
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
