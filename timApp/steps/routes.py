from typing import Optional

from sqlalchemy import ForeignKey, select
from sqlalchemy.orm import mapped_column, Mapped
from timApp.timdb.sqa import db, run_sql
from timApp.util.flask.responsehelper import json_response

from flask import Blueprint, Response

from timApp.auth.sessioninfo import (
    get_current_user_object,
    logged_in,
    get_current_user_group,
)
from timApp.user.user import User, UserGroup
from timApp.document.docentry import DocEntry
from timApp.util.flask.requesthelper import RouteException, NotExist

from dataclasses import dataclass
from flask import request

steps_blueprint = Blueprint("steps", __name__, url_prefix="/steps")


@dataclass
class Step:
    phase_title: str = ""
    content: str = ""


@dataclass
class StepsContent:
    steps_content: list[Step]


class StepsPhase(db.Model):
    id: Mapped[int] = mapped_column(primary_key=True)
    document_id: Mapped[int]
    user_working_group: Mapped[int]  # käyttäjän työryhmä
    user_group: Mapped[int] = mapped_column(
        ForeignKey("usergroup.id")
    )  # käyttäjän henkilökohtainen ryhmä
    # main_steps_id: Mapped[int] = mapped_column(ForeignKey("steps.id"), nullable=True)
    name: Mapped[str]
    # steps_content: Mapped[str] = mapped_column(nullable=True)
    current_phase: Mapped[int]

    @staticmethod
    def find_by_id(steps_id: int) -> "StepsPhase":
        return db.session.get(StepsPhase, steps_id)

    @staticmethod
    def find_by_task(doc_id: int, name: str, user_group: int) -> Optional["StepsPhase"]:
        step: StepsPhase | None = (
            run_sql(
                select(StepsPhase)
                .filter_by(document_id=doc_id, name=name, user_group=user_group)
                .limit(1)
            )
            .scalars()
            .first()
        )
        return step

    # TODO: Fix this to search actually where usergroup is as asked
    @staticmethod
    def find_all_by_user_group(user_group: UserGroup) -> list[Optional["StepsPhase"]]:
        return db.session.get(StepsPhase, user_group.name)

    def change_current_step(self, new_step: int = 0) -> "StepsPhase":
        self.current_phase = new_step
        db.session.commit()
        return self

    @staticmethod
    def create(name: str, doc_id: int, current_phase: int = 0) -> "StepsPhase":
        steps = StepsPhase(
            name=name,
            document_id=doc_id,
            user_working_group=get_current_user_group(),
            user_group=get_current_user_group(),
            current_phase=current_phase,
        )
        db.session.add(steps)
        db.session.commit()
        return steps

    def to_json(self) -> dict:
        return {
            "id": self.id,
            "document_id": self.document_id,
            "user_working_group": self.user_working_group,
            "user_group": self.user_group,
            # "main_steps_id": self.main_steps_id,
            "name": self.name,
            # "steps_content": self.steps_content,
            "current_phase": self.current_phase,
        }


@steps_blueprint.get("/<int:steps_id>")
@steps_blueprint.get("")
def get_steps_data(steps_id: int | None = None) -> Response:
    """
    Provide user profile details according requested user.
    :param userid: ID of the user
    :return: JSON data, containing user profile details
    """

    user: User = get_current_user_object()

    if user is None:
        raise RouteException("No user in current session was found.")

    if steps_id is None:
        steps = StepsPhase.find_all_by_user_group(user.groups[0])
        return json_response(steps)

    steps = StepsPhase.find_by_id(steps_id)
    if steps is None:
        raise NotExist("Steps element does not exist.")

    return json_response(steps.to_json())


@steps_blueprint.post("/create")
def create_steps() -> Response:
    if not logged_in():
        raise RouteException("You have to be logged in to create steps.")

    request_data = request.get_json()

    document_info = DocEntry.find_by_id(int(request_data.document_id))
    if document_info is None:
        raise RouteException("No profile-document found.")
    # verify_edit_access(document_info)

    steps_data = {
        "name": request_data.name,
        "steps_content": request_data.steps_content,
        "current_phase": request_data.current_phase,
    }

    StepsPhase.create(**steps_data)
    response = Response()
    return response
