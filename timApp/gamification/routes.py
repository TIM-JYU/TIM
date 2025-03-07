from dataclasses import dataclass

from flask import Response, request
from sqlalchemy import select

from timApp.gamification.badges import Badge, BadgeGiven
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.util.flask.responsehelper import (
    ok_response,
    json_response,
    empty_response,
    error_generic,
)
from timApp.util.flask.typedblueprint import TypedBlueprint

badges_blueprint = TypedBlueprint("badges", __name__)


@dataclass
class BadgeModel:
    id: int
    title: str
    description: str
    color: str
    shape: str
    image: int
    context_group: int
    active: bool
    created_by: int
    created: datetime_tz
    modified_by: int | None
    modified: datetime_tz | None
    deleted_by: int | None
    deleted: datetime_tz | None
    restored_by: int | None
    restored: datetime_tz | None


@badges_blueprint.get("/all_badges_including_nonactive")
def all_badges_including_nonactive() -> Response:
    badges = run_sql(select(Badge)).scalars().all()
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


@badges_blueprint.get("/all_badges")
def get_badges() -> Response:
    badges = run_sql(select(Badge).filter_by(active=True)).scalars().all()
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


# TODO: Handle errors.
@badges_blueprint.get("/all_badges_in_context/<context_group>")
def get_badges_in_context(context_group: str) -> Response:
    badges = (
        run_sql(
            select(Badge).filter(Badge.active).filter_by(context_group=context_group)
        )
        .scalars()
        .all()
    )
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


# TODO: Handle errors.
@badges_blueprint.get("/badge/<badge_id>")
def get_badge(badge_id: int) -> Response:
    badge = run_sql(select(Badge).filter_by(id=badge_id)).scalars().first()
    if badge is None:
        return error_generic("there's no badge with id: " + str(badge_id), 404)
    badge_json = badge.to_json()
    return json_response(badge_json)


# TODO: Delete this in the final implementation.
@badges_blueprint.get("/create_badge_hard")
def create_badge_hard() -> Response:
    badge = Badge(
        active=True,
        context_group="group1",
        title="Hard worker",
        color="red",
        shape="hexagon",
        image=6,
        description="You have worked really hard!",
        created_by=2,
        created=datetime_tz.now(),
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


# TODO: Delete this in the final implementation.
@badges_blueprint.get(
    "/create_badge_simple/<created_by>/<context_group>/<title>/<color>/<shape>/<image>/<description>"
)
def create_badge_simple(
    created_by: int,
    context_group: str,
    title: str,
    color: str,
    shape: str,
    image: int,
    description: str,
) -> Response:
    badge = Badge(
        active=True,
        context_group=context_group,
        title=title,
        color=color,
        shape=shape,
        image=image,
        description=description,
        created_by=created_by,
        created=datetime_tz.now(),
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


# TODO: Make this work.
@badges_blueprint.post("/create_badge")
def create_badge() -> Response:
    badge = Badge(
        active=True,
        context_group=request.args.get("context_group"),
        title=request.args.get("title"),
        color=request.args.get("color"),
        shape=request.args.get("shape"),
        image=request.args.get("image"),
        description=request.args.get("description"),
        created_by=request.args.get("created_by"),
        created=datetime_tz.now(),
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


# TODO: Delete this in the final implementation.
# TODO: Handle errors.
@badges_blueprint.get("/modify_badge_hard/<badge_id>")
def modify_badge_hard(badge_id: int) -> Response:
    badge = {
        "active": True,
        "context_group": "group1",
        "title": "Constant worker",
        "color": "teal",
        "shape": "circle",
        "image": 6,
        "description": "You have worked constantly!",
        "modified_by": 3,
        "modified": datetime_tz.now(),
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


# TODO: Delete this in the final implementation.
# TODO: Handle errors.
@badges_blueprint.get(
    "/modify_badge_simple/<badge_id>/<modified_by>/<context_group>/<title>/<color>/<shape>/<image>/<description>"
)
def modify_badge_simple(
    modified_by: int,
    context_group: str,
    badge_id: int,
    title: str,
    color: str,
    shape: str,
    image: int,
    description: str,
) -> Response:
    badge = {
        "active": True,
        "context_group": context_group,
        "title": title,
        "color": color,
        "shape": shape,
        "image": image,
        "description": description,
        "modified_by": modified_by,
        "modified": datetime_tz.now(),
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


# TODO: Make this work.
# TODO: Handle errors.
@badges_blueprint.get("/modify_badge")
def modify_badge() -> Response:
    badge = {
        "active": True,
        "context_group": request.args.get("context_group"),
        "title": request.args.get("title"),
        "color": request.args.get("color"),
        "shape": request.args.get("shape"),
        "image": request.args.get("image"),
        "description": request.args.get("description"),
        "modified_by": request.args.get("modified_by"),
        "modified": datetime_tz.now(),
    }
    Badge.query.filter_by(id=request.args.get("id")).update(badge)
    db.session.commit()
    return ok_response()


# TODO: Delete this in the final implementation.
# TODO: Handle errors.
@badges_blueprint.get("/delete_badge/<badge_id>")
def delete_badge(badge_id: int) -> Response:
    BadgeGiven.query.filter_by(badge_id=badge_id).delete()
    Badge.query.filter_by(id=badge_id).delete()
    db.session.commit()
    return ok_response()


# TODO: Handle errors.
@badges_blueprint.get("/deactivate_badge/<badge_id>/<deleted_by>")
def deactivate_badge(badge_id: int, deleted_by: int) -> Response:
    badge = {
        "active": False,
        "deleted_by": deleted_by,
        "deleted": datetime_tz.now(),
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


# TODO: Handle errors.
@badges_blueprint.get("/reactivate_badge/<badge_id>/<restored_by>")
def reactivate_badge(badge_id: int, restored_by: int) -> Response:
    badge = {
        "active": True,
        "restored_by": restored_by,
        "restored": datetime_tz.now(),
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


# TODO: Handle errors.
@badges_blueprint.get("/groups_badges/<group_id>")
def get_groups_badges(group_id: int) -> Response:
    groups_badges_given = (
        run_sql(
            select(BadgeGiven).filter(
                BadgeGiven.active and BadgeGiven.group_id == group_id
            )
        )
        .scalars()
        .all()
    )

    badge_ids_and_msgs = {}

    # TODO: If badge_id is not supposed to be unique for group_id, refactor this to use str-key with some running number.
    for badgeGiven in groups_badges_given:
        badge_ids_and_msgs[badgeGiven.badge_id] = badgeGiven.message

    groups_badges = (
        run_sql(
            select(Badge)
            .filter_by(active=True)
            .filter(Badge.id.in_(badge_ids_and_msgs.keys()))
        )
        .scalars()
        .all()
    )

    badges_json = []

    for badge in groups_badges:
        badges_json.append(badge.to_json())

    return json_response(badges_json)


# TODO: Handle errors.
@badges_blueprint.get("/give_badge/<given_by>/<group_id>/<badge_id>/<message>")
def give_badge(given_by: int, group_id: int, badge_id: int, message: str) -> Response:
    badge_given = BadgeGiven(
        active=True,
        group_id=group_id,
        badge_id=badge_id,
        message=message,
        given_by=given_by,
        given=datetime_tz.now(),
    )
    db.session.add(badge_given)
    db.session.commit()
    return ok_response()


# TODO: Handle errors.
@badges_blueprint.get("/withdraw_badge/<badge_given_id>/<withdrawn_by>")
def withdraw_badge(badge_given_id: int, withdrawn_by: int) -> Response:
    badge_given = {
        "active": False,
        "withdrawn_by": withdrawn_by,
        "withdrawn": datetime_tz.now(),
    }
    BadgeGiven.query.filter_by(id=badge_given_id).update(badge_given)
    db.session.commit()
    return ok_response()


# TODO: Handle errors.
@badges_blueprint.get("/undo_withdraw_badge/<badge_given_id>/<undo_withdrawn_by>")
def undo_withdraw_badge(badge_given_id: int, undo_withdrawn_by: int) -> Response:
    badge_given = {
        "active": True,
        "undo_withdrawn_by": undo_withdrawn_by,
        "undo_withdrawn": datetime_tz.now(),
    }
    BadgeGiven.query.filter_by(id=badge_given_id).update(badge_given)
    db.session.commit()
    return ok_response()
