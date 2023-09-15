import json
from datetime import datetime, timezone
from typing import Optional, TYPE_CHECKING, List

from sqlalchemy import select, func, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, DynamicMapped, relationship

from timApp.lecture.lectureusers import LectureUsers
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.util.utils import get_current_time

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.lecture.askedquestion import AskedQuestion
    from timApp.lecture.message import Message
    from timApp.lecture.runningquestion import RunningQuestion
    from timApp.lecture.useractivity import UserActivity


class Lecture(db.Model):
    lecture_id: Mapped[int] = mapped_column(primary_key=True)
    lecture_code: Mapped[Optional[str]]
    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"))
    lecturer: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    start_time: Mapped[datetime_tz]
    end_time: Mapped[Optional[datetime_tz]]
    password: Mapped[Optional[str]]
    options: Mapped[Optional[str]]

    users: DynamicMapped["User"] = relationship(
        secondary=LectureUsers.__table__,
        back_populates="lectures",
        lazy="dynamic",
    )
    asked_questions: DynamicMapped["AskedQuestion"] = relationship(
        back_populates="lecture",
        lazy="dynamic",
    )
    messages: DynamicMapped["Message"] = relationship(
        back_populates="lecture", lazy="dynamic"
    )
    running_questions: Mapped[List["RunningQuestion"]] = relationship(
        back_populates="lecture",
        lazy="select",
    )
    useractivity: Mapped[List["UserActivity"]] = relationship(
        back_populates="lecture", lazy="select"
    )
    owner: Mapped["User"] = relationship(back_populates="owned_lectures")

    @staticmethod
    def find_by_id(lecture_id: int) -> Optional["Lecture"]:
        return db.session.get(Lecture, lecture_id)

    @staticmethod
    def find_by_code(lecture_code: str, doc_id: int) -> Optional["Lecture"]:
        return (
            run_sql(select(Lecture).filter_by(lecture_code=lecture_code, doc_id=doc_id))
            .scalars()
            .first()
        )

    @staticmethod
    def get_all_in_document(
        doc_id: int, time: datetime | None = None
    ) -> list["Lecture"]:
        if not time:
            time = datetime.min.replace(tzinfo=timezone.utc)
        return (
            run_sql(
                select(Lecture)
                .filter_by(doc_id=doc_id)
                .filter(Lecture.end_time > time)
                .order_by(Lecture.lecture_code.asc())
            )
            .scalars()
            .all()
        )

    @property
    def options_parsed(self):
        if not hasattr(self, "_options_parsed"):
            self._options_parsed = json.loads(self.options)
        return self._options_parsed

    @property
    def max_students(self):
        m = self.options_parsed.get("max_students")
        if m is not None:
            try:
                m = int(m)
            except ValueError:
                m = None
        return m

    @property
    def is_full(self):
        max_students = self.max_students
        if max_students is None:
            return False
        cnt = db.session.scalar(
            select(func.count())
            .select_from(LectureUsers)
            .filter_by(lecture_id=self.lecture_id)
        )
        return cnt >= max_students

    @property
    def is_running(self):
        time_now = get_current_time()
        return self.start_time <= time_now < self.end_time

    def to_json(self, show_password=False):
        return {
            "lecture_id": self.lecture_id,
            "lecture_code": self.lecture_code,
            "doc_id": self.doc_id,
            "lecturer": self.lecturer,
            "start_time": self.start_time,
            "end_time": self.end_time,
            "options": self.options_parsed,
            "is_access_code": self.password != "",
            # don't expose password to client directly unless explicitly requested with the parameter
            "password": self.password if show_password else None,
            "is_full": self.is_full,
        }
