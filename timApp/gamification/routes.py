from flask import Response
from sqlalchemy import select

from timApp.gamification.badges import Badge, BadgeGiven
from timApp.timdb.sqa import db, run_sql
from timApp.util.flask.responsehelper import ok_response, json_response
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
                "description": badge._data[0].description,
                "color": badge._data[0].color,
                "shape": badge._data[0].shape,
                "image": badge._data[0].image,
            }
        )

    return json_response(badges_json)


@badges_blueprint.get("/create_badge")
def create_badge():
    badge = Badge(
        title="Hard worker",
        description="You have worked really hard!",
        color="red",
        shape="hexagon",
        image=6,
    )
    db.session.add(badge)
    db.session.commit()
    return ok_response()


# @badges_blueprint.get("/delete_badge")
# def delete_badge():


@badges_blueprint.get("/groups_badges/<group_id>")
def get_groups_badges(group_id: int) -> Response:
    groups_badges_given = run_sql(
        select(BadgeGiven).filter(BadgeGiven.group_id == group_id)
    ).all()

    badge_ids_json = []

    for badge in groups_badges_given:
        badge_ids_json.append(badge._data[0].badge_id)

    groups_badges = run_sql(select(Badge).filter(Badge.id.in_(badge_ids_json))).all()

    badges_json = []

    for badge in groups_badges:
        badges_json.append(
            {
                "id": badge._data[0].id,
                "title": badge._data[0].title,
                "description": badge._data[0].description,
                "color": badge._data[0].color,
                "shape": badge._data[0].shape,
                "image": badge._data[0].image,
            }
        )

    return json_response(badges_json)


@badges_blueprint.get("/give_badge/<group_id>/<badge_id>")
def give_badge(group_id: int, badge_id: int) -> Response:
    badge_given = BadgeGiven(group_id=group_id, badge_id=badge_id)
    db.session.add(badge_given)
    db.session.commit()
    return ok_response()


@badges_blueprint.get("/withdraw_badge/<badge_given_id>")
def withdraw_badge(badge_given_id: int) -> Response:
    try:
        badge_given = BadgeGiven(group_id=badge_given_id)
        withdrawn_badge = run_sql(
            select(BadgeGiven).filter(BadgeGiven.id == badge_given_id)
        ).first()
    except:
        return
    badge_given = BadgeGiven(group_id=group_id, badge_id=badge_id)
    db.session.add(badge_given)
    db.session.commit()
    return ok_response()
