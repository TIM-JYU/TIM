# from dataclasses import dataclass

from flask import Response, request
from sqlalchemy import select

from timApp.gamification.badges import Badge, BadgeGiven
from timApp.timdb.sqa import db, run_sql
from timApp.util.flask.responsehelper import (
    ok_response,
    json_response,
    empty_response,
    error_generic,
)
from timApp.util.flask.typedblueprint import TypedBlueprint

badges_blueprint = TypedBlueprint("badges", __name__)


# @dataclass
# class BadgeModel:
#     id: int
#     active: bool
#     context_group: str
#     title: str
#     color: str
#     shape: str
#     image: int
#     description: str
#
#     def to_json(self) -> dict[str, int | bool | str]:
#         return {
#             "id": self._data[0].id,
#             "active": self._data[0].active,
#             "context_group": self._data[0].context_group,
#             "title": self._data[0].title,
#             "color": self._data[0].color,
#             "shape": self._data[0].shape,
#             "image": self._data[0].image,
#             "description": self._data[0].description,
#         }


@badges_blueprint.get("/all_badges_including_nonactive")
def all_badges_including_nonactive() -> Response:
    badges = run_sql(select(Badge)).all()
    badges_json = []
    for badge in badges:
        badges_json.append(
            {
                "id": badge._data[0].id,
                "active": badge._data[0].active,
                "context_group": badge._data[0].context_group,
                "title": badge._data[0].title,
                "color": badge._data[0].color,
                "shape": badge._data[0].shape,
                "image": badge._data[0].image,
                "description": badge._data[0].description,
            }
        )
    return json_response(badges_json)


@badges_blueprint.get("/all_badges")
def get_badges() -> Response:
    badges = run_sql(select(Badge).filter_by(active=True)).all()
    badges_json = []
    for badge in badges:
        badges_json.append(
            {
                "id": badge._data[0].id,
                "active": badge._data[0].active,
                "context_group": badge._data[0].context_group,
                "title": badge._data[0].title,
                "color": badge._data[0].color,
                "shape": badge._data[0].shape,
                "image": badge._data[0].image,
                "description": badge._data[0].description,
            }
        )
    return json_response(badges_json)


@badges_blueprint.get("/all_badges_in_context/<context_group>")
def get_badges_in_context(context_group: str) -> Response:
    badges = run_sql(select(Badge).filter_by(context_group=context_group)).all()
    badges_json = []
    for badge in badges:
        badges_json.append(
            {
                "id": badge._data[0].id,
                "active": badge._data[0].active,
                "context_group": badge._data[0].context_group,
                "title": badge._data[0].title,
                "color": badge._data[0].color,
                "shape": badge._data[0].shape,
                "image": badge._data[0].image,
                "description": badge._data[0].description,
            }
        )
    return json_response(badges_json)


@badges_blueprint.get("/badge/<badge_id>")
def get_badge(badge_id: int) -> Response:
    badge = run_sql(select(Badge).filter_by(id=badge_id)).first()
    # return json_response(badge.to_json())
    # Badge.to_json(badge)
    if badge is None:
        return error_generic("there's no badge with id: " + str(badge_id), 404)
    badge_json = {
        "id": badge._data[0].id,
        "active": badge._data[0].active,
        "context_group": badge._data[0].context_group,
        "title": badge._data[0].title,
        "color": badge._data[0].color,
        "shape": badge._data[0].shape,
        "image": badge._data[0].image,
        "description": badge._data[0].description,
    }
    return json_response(badge_json)


# todo: Delete this in the final implementation.
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
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


# todo: Delete this in the final implementation.
@badges_blueprint.get(
    "/create_badge_simple/<context_group>/<title>/<color>/<shape>/<image>/<description>"
)
def create_badge_simple(
    context_group: str, title: str, color: str, shape: str, image: int, description: str
) -> Response:
    badge = Badge(
        active=True,
        context_group=context_group,
        title=title,
        color=color,
        shape=shape,
        image=image,
        description=description,
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


# todo: Make this work.
@badges_blueprint.get("/create_badge")
def create_badge() -> Response:
    badge = Badge(
        active=True,
        context_group=request.args.get("context_group"),
        title=request.args.get("title"),
        color=request.args.get("color"),
        shape=request.args.get("shape"),
        image=request.args.get("image"),
        description=request.args.get("description"),
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


# todo: Delete this in the final implementation.
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
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


# todo: Delete this in the final implementation.
@badges_blueprint.get(
    "/modify_badge_simple/<badge_id>/<context_group>/<title>/<color>/<shape>/<image>/<description>"
)
def modify_badge_simple(
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
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


# todo: Make this work.
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
    }
    Badge.query.filter_by(id=request.args.get("id")).update(badge)
    db.session.commit()
    return ok_response()


# todo: Delete this in the final implementation.
@badges_blueprint.get("/delete_badge/<badge_id>")
def delete_badge(badge_id: int) -> Response:
    BadgeGiven.query.filter_by(badge_id=badge_id).delete()
    Badge.query.filter_by(id=badge_id).delete()
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/deactivate_badge/<badge_id>")
def deactivate_badge(badge_id: int) -> Response:
    badge = {"active": False}
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/reactivate_badge/<badge_id>")
def reactivate_badge(badge_id: int) -> Response:
    badge = {"active": True}
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/groups_badges/<group_id>")
def get_groups_badges(group_id: int) -> Response:
    groups_badges_given = run_sql(
        select(BadgeGiven).filter(
            BadgeGiven.active == True and BadgeGiven.group_id == group_id
        )
    ).all()

    badge_ids_and_msgs = {}

    # todo: If badge_id is not supposed to be unique, refactor this to use str-key with some running number.
    for badge in groups_badges_given:
        badge_ids_and_msgs[badge._data[0].badge_id] = badge._data[0].message

    groups_badges = run_sql(
        select(Badge).filter(Badge.id.in_(badge_ids_and_msgs.keys()))
    ).all()

    badges_json = []

    for badge in groups_badges:
        badges_json.append(
            {
                "id": badge._data[0].id,
                "context_group": badge._data[0].context_group,
                "title": badge._data[0].title,
                "color": badge._data[0].color,
                "shape": badge._data[0].shape,
                "image": badge._data[0].image,
                "description": badge._data[0].description,
                "message": badge_ids_and_msgs[badge._data[0].id],
            }
        )

    return json_response(badges_json)


# todo: If badge_id is supposed to be unique:
#   - refactor this to handle multiple same badges given to same group.
#   - there will be problems with active-column in withdraw_badge and undo_withdraw_badge.
@badges_blueprint.get("/give_badge/<group_id>/<badge_id>/<message>")
def give_badge(group_id: int, badge_id: int, message: str) -> Response:
    badge_given = BadgeGiven(
        active=True, group_id=group_id, badge_id=badge_id, message=message
    )
    db.session.add(badge_given)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/withdraw_badge/<badge_given_id>")
def withdraw_badge(badge_given_id: int) -> Response:
    badge_given = {"active": False}
    BadgeGiven.query.filter_by(id=badge_given_id).update(badge_given)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/undo_withdraw_badge/<badge_given_id>")
def undo_withdraw_badge(badge_given_id: int) -> Response:
    badge_given = {"active": True}
    BadgeGiven.query.filter_by(id=badge_given_id).update(badge_given)
    db.session.commit()
    return ok_response()
