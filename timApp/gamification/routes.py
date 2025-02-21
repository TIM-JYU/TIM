from sqlalchemy import select

from timApp.gamification.badges import Badge, BadgeGiven
from timApp.timdb.sqa import db, run_sql
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

badges_blueprint = TypedBlueprint("badges", __name__)


@badges_blueprint.get("/all_badges")
def get_badges() -> list[dict]:
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

    return badges_json


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


# @badges_blueprint.get("/groups_badges")
# def get_groups_badges() -> list[dict]:
#     groups_badges = run_sql(select(BadgeGiven).filter(BadgeGiven.group_id == 1)).all()
#
#     badge_ids_json = []
#
#     for badge in groups_badges:
#         badge_ids_json.append(
#             {
#                 "badge_id": badge._data[0].badge_id,
#             }
#         )
#
#     badges_json = []
#
#     for badge_id in badge_ids_json:
#         badges_json.append()
#
#     return badge_ids_json


@badges_blueprint.get("/give_badge")
def give_badge():
    badge_given = BadgeGiven(badge_id=8, group_id=1)
    db.session.add(badge_given)
    db.session.commit()
    return ok_response()
