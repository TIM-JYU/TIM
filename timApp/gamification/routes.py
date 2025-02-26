from flask import Response, request
from sqlalchemy import select

from timApp.gamification.badges import Badge, BadgeGiven
from timApp.timdb.sqa import db, run_sql
from timApp.util.flask.responsehelper import ok_response, json_response, empty_response
from timApp.util.flask.typedblueprint import TypedBlueprint

badges_blueprint = TypedBlueprint("badges", __name__)


@badges_blueprint.get("/all_badges")
def get_badges() -> Response:
    badges = run_sql(select(Badge)).all()

    badges_json = []

    for badge in badges:
        badges_json.append(
            {
                "id": badge._data[0].id,
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
    badge_json = {
        "id": badge._data[0].id,
        "title": badge._data[0].title,
        "color": badge._data[0].color,
        "shape": badge._data[0].shape,
        "image": badge._data[0].image,
        "description": badge._data[0].description,
    }
    return json_response(badge_json)


@badges_blueprint.get("/create_badge_hard")
def create_badge_hard():
    badge = Badge(
        title="Hard worker",
        color="red",
        shape="hexagon",
        image=6,
        description="You have worked really hard!",
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


@badges_blueprint.get(
    "/create_badge_simple/<title>/<color>/<shape>/<image>/<description>"
)
def create_badge_simple(
    title: str, color: str, shape: str, image: int, description: str
) -> Response:
    badge = Badge(
        title=title,
        color=color,
        shape=shape,
        image=image,
        description=description,
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/create_badge")
def create_badge():
    badge = Badge(
        title=request.args.get("title"),
        color=request.args.get("color"),
        shape=request.args.get("shape"),
        image=request.args.get("image"),
        description=request.args.get("description"),
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/modify_badge_hard/<badge_id>")
def modify_badge_hard(badge_id: int):
    badge = {
        "title": "Constant worker",
        "color": "green",
        "shape": "square",
        "image": 8,
        "description": "You have worked constantly!",
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


@badges_blueprint.get(
    "/modify_badge_simple/<badge_id>/<title>/<color>/<shape>/<image>/<description>"
)
def modify_badge_simple(
    badge_id: int, title: str, color: str, shape: str, image: int, description: str
) -> Response:
    badge = {
        "title": title,
        "color": color,
        "shape": shape,
        "image": image,
        "description": description,
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/modify_badge")
def modify_badge():
    badge = {
        "title": request.args.get("title"),
        "color": request.args.get("color"),
        "shape": request.args.get("shape"),
        "image": request.args.get("image"),
        "description": request.args.get("description"),
    }
    Badge.query.filter_by(id=request.args.get("id")).update(badge)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/delete_badge/<badge_id>")
def delete_badge(badge_id: int):
    BadgeGiven.query.filter_by(badge_id=badge_id).delete()
    Badge.query.filter_by(id=badge_id).delete()
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/groups_badges/<group_id>")
def get_groups_badges(group_id: int) -> Response:
    groups_badges_given = run_sql(
        select(BadgeGiven).filter(BadgeGiven.group_id == group_id)
    ).all()

    badge_ids_json = []
    badge_ids_and_msgs_json = {}

    for badge in groups_badges_given:
        badge_ids_json.append(badge._data[0].badge_id)
        badge_ids_and_msgs_json[badge._data[0].badge_id] = badge._data[0].message

    groups_badges = run_sql(select(Badge).filter(Badge.id.in_(badge_ids_json))).all()

    badges_json = []

    for badge in groups_badges:
        badges_json.append(
            {
                "id": badge._data[0].id,
                "title": badge._data[0].title,
                "color": badge._data[0].color,
                "shape": badge._data[0].shape,
                "image": badge._data[0].image,
                "description": badge._data[0].description,
                "message": badge_ids_and_msgs_json[badge._data[0].id],
            }
        )

    return json_response(badges_json)


@badges_blueprint.get("/give_badge/<group_id>/<badge_id>/<message>")
def give_badge(group_id: int, badge_id: int, message: str) -> Response:
    badge_given = BadgeGiven(group_id=group_id, badge_id=badge_id, message=message)
    db.session.add(badge_given)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/withdraw_badge/<badge_given_id>")
def withdraw_badge(badge_given_id: int) -> Response:
    BadgeGiven.query.filter_by(id=badge_given_id).delete()
    db.session.commit()
    return ok_response()
