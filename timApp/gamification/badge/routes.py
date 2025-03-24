"""badge-related routes."""
from dataclasses import dataclass
from pathlib import Path

from flask import Response, current_app
from sqlalchemy import select, or_

from timApp.auth.accesshelper import AccessDenied, verify_teacher_access
from timApp.document.docentry import DocEntry
from timApp.gamification.badge.badges import Badge, BadgeGiven
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.flask.requesthelper import NotExist
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
    found at ../timapp/tim_logs/badge.log.
    :param log_info: info that is about to be logged in badge.log file
    :return:
    """
    path = Path(current_app.config["BADGE_LOG_PATH"])
    with path.open("a") as f:
        f.write(to_json_str(log_info) + "\n")


def check_context_group_access(user_id: int, context_group: str) -> None:
    """
    Checks whether a user has access to a context group.
    :param user_id: Current user's ID
    :param context_group: Context group to check
    :return:
    """
    context_usergroups = (
        run_sql(select(UserGroup).filter(UserGroup.name == context_group))
        .scalars()
        .all()
    )
    context_group_ids = []
    for context_usergroup in context_usergroups:
        context_group_ids.append(context_usergroup.id)
    allowed_member = (
        run_sql(
            select(UserGroupMember).filter(
                UserGroupMember.user_id == user_id,
                UserGroupMember.usergroup_id.in_(context_group_ids),
                or_(
                    UserGroupMember.membership_end > datetime_tz.now(),
                    UserGroupMember.membership_end == None,
                ),
            )
        )
        .scalars()
        .first()
    )

    # TODO: Handle this error in frontend.
    if not allowed_member:
        raise AccessDenied(f"You don't have access to context group {context_group}.")
        # return error_generic("access denied", 403)
    # verify_context_group_access()


# TODO: Not in use. Remove?
@badges_blueprint.get("/all_badges_including_inactive")
def all_badges_including_inactive() -> Response:
    """
    Fetches all badges including the inactive badges. Sorted by created timestamp.
    :return: Badges in json response format
    """
    badges = run_sql(select(Badge).order_by(Badge.created)).scalars().all()
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


@badges_blueprint.get("/all_badges")
def get_badges() -> Response:
    """
    Fetches all badges except the inactive badges. Sorted by created timestamp.
    :return: Badges in json response format
    """
    badges = (
        run_sql(select(Badge).filter_by(active=True).order_by(Badge.created))
        .scalars()
        .all()
    )
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


# TODO: Handle errors.
@badges_blueprint.get("/all_badges_in_context/<user_id>/<doc_id>/<context_group>")
def all_badges_in_context(user_id: int, doc_id: int, context_group: str) -> Response:
    """
    Fetches all badges in specific context_group. Sorted by created timestamp.
    :param doc_id: Current document's ID
    :param user_id: Current user's ID
    :param context_group: Context group to get badges from
    :return: Badges in json response format
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    check_context_group_access(user_id, context_group)
    badges = (
        run_sql(
            select(Badge)
            .filter(Badge.active)
            .filter_by(context_group=context_group)
            .order_by(Badge.created)
        )
        .scalars()
        .all()
    )
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


# TODO: Not in use. Remove?
# TODO: Handle errors.
@badges_blueprint.get("/badge/<badge_id>")
def get_badge(badge_id: int) -> Response:
    """
    Fetches a specific badge.
    :param badge_id: ID of the badge to get
    :return: badge in json response format or error when there is no badge with that id
    """
    badge = run_sql(select(Badge).filter_by(id=badge_id)).scalars().first()
    if badge is None:
        return error_generic("there's no badge with id: " + str(badge_id), 404)
    badge_json = badge.to_json()
    return json_response(badge_json)


@badges_blueprint.post("/create_badge")
def create_badge(
    created_by: int,
    doc_id: int,
    context_group: str,
    title: str,
    color: str,
    shape: str,
    image: int,
    description: str,
) -> Response:
    """
    Creates a new badge.
    :param doc_id: Current document's ID
    :param created_by: ID of the useraccount who creates the badge
    :param context_group: Context group where the badge will be included
    :param title: Title of the badge
    :param color: Color of the badge
    :param shape: Shape of the badge
    :param image: Image of the badge
    :param description: Description of the badge
    :return: ok response
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    check_context_group_access(created_by, context_group)
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
    doc_id: int,
    context_group: str,
    title: str,
    color: str,
    shape: str,
    image: int,
    description: str,
) -> Response:
    """
    Modifies a badge.
    :param doc_id: Current document's ID
    :param badge_id: ID of the badge
    :param modified_by: ID of the useraccount who modifies the badge
    :param context_group: Context group where the badge will be included
    :param title: Title of the badge
    :param color: Color of the badge
    :param shape: Shape of the badge
    :param image: Image of the badge
    :param description: Description of the badge
    :return: ok response
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    check_context_group_access(modified_by, context_group)
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
def deactivate_badge(
    badge_id: int, deleted_by: int, doc_id: int, context_group: str
) -> Response:
    """
    Deactivates a badge.
    :param doc_id: Current document's ID
    :param context_group: Context group where the badge is included
    :param badge_id: ID of the badge
    :param deleted_by: ID of the useraccount who deactivates the badge
    :return: ok response
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    check_context_group_access(deleted_by, context_group)
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


# TODO: Not yet in use.
# TODO: Handle errors.
@badges_blueprint.post("/reactivate_badge")
def reactivate_badge(
    badge_id: int, restored_by: int, doc_id: int, context_group: str
) -> Response:
    """
    Reactivates a badge.
    :param doc_id: Current document's ID
    :param context_group: Context group where the badge is included
    :param badge_id: ID of the badge
    :param restored_by: ID of the useraccount who reactivates the badge
    :return: ok response
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    check_context_group_access(restored_by, context_group)
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
            .order_by(BadgeGiven.given)
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

    groups_badges_ordered = []
    for badge_id in badge_ids:
        for groups_badge in groups_badges:
            if groups_badge.id == badge_id:
                groups_badges_ordered.append(groups_badge)

    badges_json = []
    for badge in groups_badges_ordered:
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
def give_badge(
    given_by: int,
    doc_id: int,
    context_group: str,
    group_id: int,
    badge_id: int,
    message: str,
) -> Response:
    """
    Gives a badge to a usergroup.
    :param doc_id: Current document's ID
    :param context_group: Context group where the badge is included
    :param given_by: ID of the useraccount who gives the badge
    :param group_id: ID of the usergroup that the badge is given
    :param badge_id: ID of the badge that is given to the usergroup
    :param message: Message to give to the usergroup when the badge is given
    :return: ok response
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    check_context_group_access(given_by, context_group)
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
def withdraw_badge(
    badge_given_id: int, withdrawn_by: int, doc_id: int, context_group: str
) -> Response:
    """
    Withdraws a badge from a usergroup.
    :param doc_id: Current document's ID
    :param context_group: Context group where the badge is included
    :param badge_given_id: ID of the badgegiven
    :param withdrawn_by: ID of the useraccount that withdraws the badge
    :return: ok response
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    check_context_group_access(withdrawn_by, context_group)
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


# TODO: Not yet in use.
# TODO: Handle errors.
@badges_blueprint.get("/undo_withdraw_badge")
def undo_withdraw_badge(
    badge_given_id: int, undo_withdrawn_by: int, doc_id: int, context_group: str
) -> Response:
    """
    Undoes a badge withdrawal from a usergroup.
    :param doc_id: Current document's ID
    :param context_group: Context group where the badge is included
    :param badge_given_id: ID of the badgegiven
    :param undo_withdrawn_by: ID of the useraccount that undoes the badge withdrawal
    :return: ok response
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    check_context_group_access(undo_withdrawn_by, context_group)
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


# TODO: Handle errors.
@badges_blueprint.get("/subgroups/<group_name_prefix>")
def get_subgroups(group_name_prefix: str) -> Response:
    """
    Fetces usergroups that have a name that starts with the given prefix but is not the exact prefix.
    Sorted by name.
    :param group_name_prefix: Prefix of the usergroups
    :return: List of usergroups
    """
    subgroups = (
        run_sql(
            select(UserGroup)
            .filter(
                UserGroup.name.like(group_name_prefix + "%"),
                UserGroup.name != group_name_prefix,
            )
            .order_by(UserGroup.name)
        )
        .scalars()
        .all()
    )
    subgroups_json = []
    for subgroup in subgroups:
        subgroups_json.append(subgroup.to_json())
    return json_response(subgroups_json)


# TODO: Handle errors.
@badges_blueprint.get("/users_subgroups/<user_id>/<group_name_prefix>")
def get_users_subgroups(user_id: int, group_name_prefix: str) -> Response:
    """
    Fetches usergroups that user with given user_id belongs. Fetched usergroups also
    have a name that starts with the given prefix but is not the exact prefix. Sorted by name.
    :param user_id: ID of the user
    :param group_name_prefix: Prefix of the usergroups
    :return: List of usergroups
    """
    usergroup_memberships = (
        run_sql(
            select(UserGroupMember).filter(
                UserGroupMember.user_id == user_id,
                or_(
                    UserGroupMember.membership_end > datetime_tz.now(),
                    UserGroupMember.membership_end == None,
                ),
            )
        )
        .scalars()
        .all()
    )
    users_usergroup_ids = []
    for usergroup_membership in usergroup_memberships:
        users_usergroup_ids.append(usergroup_membership.usergroup_id)
    users_subgroups = (
        run_sql(
            select(UserGroup)
            .filter(
                UserGroup.id.in_(users_usergroup_ids),
                UserGroup.name.like(group_name_prefix + "%"),
                UserGroup.name != group_name_prefix,
            )
            .order_by(UserGroup.name)
        )
        .scalars()
        .all()
    )
    users_subgroups_json = []
    for users_subgroup in users_subgroups:
        users_subgroups_json.append(users_subgroup.to_json())
    return json_response(users_subgroups_json)


# TODO: Handle errors.
@badges_blueprint.get("/users_personal_group/<name>")
def users_personal_group(name: str) -> Response:
    personal_group = UserGroup.get_by_name(name)
    if personal_group:
        return json_response(personal_group)
    return error_generic("there's no user with name: " + name, 404)
