"""Badge-related routes."""
from dataclasses import dataclass
from pathlib import Path

import filelock
from flask import Response, current_app
from sqlalchemy import select

from timApp.gamification.badges import Badge, BadgeGiven
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.util.flask.responsehelper import (
    ok_response,
    json_response,
    error_generic,
    to_json_str,
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


def log_badge_event(log_info: dict) -> None:
    """
    Logs all events that modifies badge or badgegiven tables. Log file can be
    found at ../timapp/tim_files/badge.log.
    :param log_info: info that is about to be logged in badge.log file
    :return:
    """
    path = Path(current_app.config["FILES_PATH"]) / current_app.config["BADGE_LOG_FILE"]
    with filelock.FileLock(f"/tmp/badge_log"):
        with path.open("a") as f:
            f.write(to_json_str(log_info) + "\n")


@badges_blueprint.get("/all_badges_including_inactive")
def all_badges_including_inactive() -> Response:
    """
    Fetches all badges including the inactive badges.
    :return: Badges in json response format
    """
    badges = run_sql(select(Badge)).scalars().all()
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


@badges_blueprint.get("/all_badges")
def get_badges() -> Response:
    """
    Fetches all badges except the inactive badges.
    :return: Badges in json response format
    """
    badges = run_sql(select(Badge).filter_by(active=True)).scalars().all()
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


# TODO: Handle errors.
@badges_blueprint.get("/all_badges_in_context/<context_group>")
def get_badges_in_context(context_group: str) -> Response:
    """
    Fetches all badges in specific context_group.
    :param context_group: Context group to get badges from
    :return: Badges in json response format
    """
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
    """
    Fetches a specific badge.
    :param badge_id: ID of the badge to get
    :return: Badge in json response format or error when there is no badge with that id
    """
    badge = run_sql(select(Badge).filter_by(id=badge_id)).scalars().first()
    if badge is None:
        return error_generic("there's no badge with id: " + str(badge_id), 404)
    badge_json = badge.to_json()
    return json_response(badge_json)


@badges_blueprint.post("/create_badge")
def create_badge(
    created_by: int,
    context_group: str,
    title: str,
    color: str,
    shape: str,
    image: int,
    description: str,
) -> Response:
    """
    Creates a new badge.
    :param created_by: ID of the usergroup who creates the badge
    :param context_group: Context group where the badge will be included
    :param title: Title of the badge
    :param color: Color of the badge
    :param shape: Shape of the badge
    :param image: Image of the badge
    :param description: Description of the badge
    :return: ok response
    """
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

    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "create_badge",
                "timestamp": badge.created,
                "executor": badge.created_by,
                "active": badge.active,
                "context_group": badge.context_group,
                "title": badge.title,
                "color": badge.color,
                "shape": badge.shape,
                "image": badge.image,
                "description": badge.description,
            }
        )

    return ok_response()


# TODO: Handle errors.
@badges_blueprint.post("/modify_badge")
def modify_badge(
    badge_id: int,
    modified_by: int,
    context_group: str,
    title: str,
    color: str,
    shape: str,
    image: int,
    description: str,
) -> Response:
    """
    Modifies a badge.
    :param badge_id: ID of the badge
    :param modified_by: ID of the usergroup who modifies the badge
    :param context_group: Context group where the badge will be included
    :param title: Title of the badge
    :param color: Color of the badge
    :param shape: Shape of the badge
    :param image: Image of the badge
    :param description: Description of the badge
    :return: ok response
    """
    badge = {
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

    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "modify_badge",
                "timestamp": badge["modified"],
                "id": badge_id,
                "executor": badge["modified_by"],
                "context_group": badge["context_group"],
                "title": badge["title"],
                "color": badge["color"],
                "shape": badge["shape"],
                "image": badge["image"],
                "description": badge["description"],
            }
        )

    return ok_response()


# TODO: Handle errors.
@badges_blueprint.post("/deactivate_badge")
def deactivate_badge(badge_id: int, deleted_by: int) -> Response:
    """
    Deactivates a badge.
    :param badge_id: ID of the badge
    :param deleted_by: ID of the usergroup who deactivates the badge
    :return: ok response
    """
    badge = {
        "active": False,
        "deleted_by": deleted_by,
        "deleted": datetime_tz.now(),
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()

    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "delete_badge",
                "timestamp": badge["deleted"],
                "id": badge_id,
                "executor": badge["deleted_by"],
            }
        )

    return ok_response()


# TODO: Handle errors.
@badges_blueprint.post("/reactivate_badge>")
def reactivate_badge(badge_id: int, restored_by: int) -> Response:
    """
    Reactivates a badge.
    :param badge_id: ID of the badge
    :param restored_by: ID of the usergroup who reactivates the badge
    :return: ok response
    """
    badge = {
        "active": True,
        "restored_by": restored_by,
        "restored": datetime_tz.now(),
    }
    Badge.query.filter_by(id=badge_id).update(badge)
    db.session.commit()

    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "restore_badge",
                "timestamp": badge["restored"],
                "id": badge_id,
                "executor": badge["restored_by"],
            }
        )

    return ok_response()


# TODO: Handle errors.
@badges_blueprint.get("/groups_badges/<group_id>")
def get_groups_badges(group_id: int) -> Response:
    """
    Fetches badges that are given to a usergroup.
    :param group_id: ID of the usergroup
    :return: Badges in json response format
    """
    groups_badges_given = (
        run_sql(
            select(BadgeGiven)
            .filter(BadgeGiven.active)
            .filter(BadgeGiven.group_id == group_id)
        )
        .scalars()
        .all()
    )

    badge_ids = []
    badge_ids_badgegiven_ids_and_msgs: dict[str, tuple] = {}

    for badgeGiven in groups_badges_given:
        key_extension = 0
        while (
            str(badgeGiven.badge_id) + "_" + str(key_extension)
        ) in badge_ids_badgegiven_ids_and_msgs.keys():
            key_extension += 1
        badge_ids.append(badgeGiven.badge_id)
        badge_ids_badgegiven_ids_and_msgs[
            str(badgeGiven.badge_id) + "_" + str(key_extension)
        ] = (badgeGiven.id, badgeGiven.message)

    groups_badges = (
        run_sql(select(Badge).filter_by(active=True).filter(Badge.id.in_(badge_ids)))
        .scalars()
        .all()
    )

    badges_json = []

    for badge in groups_badges:
        key_extension = 0
        for badge_id in badge_ids:
            if badge_id == badge.id:
                badge_json = badge.to_json()
                badge_json["badgegiven_id"] = badge_ids_badgegiven_ids_and_msgs[
                    str(badge.id) + "_" + str(key_extension)
                ][0]
                badge_json["message"] = badge_ids_badgegiven_ids_and_msgs[
                    str(badge.id) + "_" + str(key_extension)
                ][1]
                key_extension += 1
                badges_json.append(badge_json)

    return json_response(badges_json)


# TODO: Handle errors.
@badges_blueprint.post("/give_badge")
def give_badge(given_by: int, group_id: int, badge_id: int, message: str) -> Response:
    """
    Gives a badge to a usergroup.
    :param given_by: ID of the usergroup who gives the badge
    :param group_id: ID of the usergroup that the badge is given
    :param badge_id: ID of the badge that is given to the usergroup
    :param message: Message to give to the usergroup when the badge is given
    :return: ok response
    """
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

    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "give_badge",
                "timestamp": badge_given.given,
                "executor": badge_given.given_by,
                "active": badge_given.active,
                "badge_id": badge_given.badge_id,
                "group_id": group_id,
                "message": badge_given.message,
            }
        )

    return ok_response()


# TODO: Handle errors.
@badges_blueprint.post("/withdraw_badge")
def withdraw_badge(badge_given_id: int, withdrawn_by: int) -> Response:
    """
    Withdraws a badge from a usergroup.
    :param badge_given_id: ID of the badgegiven
    :param withdrawn_by: ID of the usergroup that withdraws the badge
    :return: ok response
    """
    badge_given = {
        "active": False,
        "withdrawn_by": withdrawn_by,
        "withdrawn": datetime_tz.now(),
    }
    BadgeGiven.query.filter_by(id=badge_given_id).update(badge_given)
    db.session.commit()

    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "withdraw_badge",
                "timestamp": badge_given["withdrawn"],
                "id": badge_given_id,
                "executor": badge_given["withdrawn_by"],
                "active": badge_given["active"],
            }
        )

    return ok_response()


# TODO: Handle errors.
@badges_blueprint.get("/undo_withdraw_badge")
def undo_withdraw_badge(badge_given_id: int, undo_withdrawn_by: int) -> Response:
    """
    Undoes a badge withdrawal from a usergroup.
    :param badge_given_id: ID of the badgegiven
    :param undo_withdrawn_by: ID of the usergroup that undoes the badge withdrawal
    :return: ok response
    """
    badge_given = {
        "active": True,
        "undo_withdrawn_by": undo_withdrawn_by,
        "undo_withdrawn": datetime_tz.now(),
    }
    BadgeGiven.query.filter_by(id=badge_given_id).update(badge_given)
    db.session.commit()

    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "undo_withdraw_badge",
                "timestamp": badge_given["undo_withdrawn"],
                "id": badge_given_id,
                "executor": badge_given["undo_withdrawn_by"],
                "active": badge_given["active"],
            }
        )

    return ok_response()
