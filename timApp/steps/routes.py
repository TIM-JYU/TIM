from typing import Optional
from sqlalchemy.orm import mapped_column, Mapped
from timApp.timdb.sqa import db, run_sql
from tim_common.marshmallow_dataclass import dataclass


@dataclass
class StepsPhase:
    phase_title: str = ""
    content: str = ""


@dataclass
class StepsContent:
    steps_content: list[StepsPhase]


class Steps(db.Model):
    id: Mapped[int]
    # document_id: Mapped[int] = mapped_column(ForeignKey("docentry.id"))
    user_group: Mapped[int]
    # main_steps_id: Mapped[int] = mapped_column(ForeignKey("steps.id"), nullable=True)
    name: Mapped[str] = mapped_column(primary_key=True)
    steps_content: Mapped[str] = mapped_column(nullable=True)
    current_phase: Mapped[int]

    @staticmethod
    def find_by_id(steps_id: int) -> Optional["Steps"]:
        return db.session.get(Steps, steps_id)

    def to_json(self):
        return {
            "id": self.id,
            # "document_id": self.document_id,
            "user_group": self.user_group,
            "main_steps_id": self.main_steps_id,
            "name": self.name,
            "steps_content": self.steps_content,
            "current_phase": self.current_phase,
        }
