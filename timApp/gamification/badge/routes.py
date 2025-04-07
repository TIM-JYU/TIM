"""badge-related routes."""
from dataclasses import dataclass
from operator import attrgetter
from pathlib import Path
from urllib import request

from flask import Response, current_app
from sqlalchemy import select, or_
from wtforms.widgets.core import Select

from timApp.auth.accesshelper import (
    AccessDenied,
    verify_teacher_access,
    get_item_or_abort,
)
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.gamification.badge.badges import Badge, BadgeGiven
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.user.groups import raise_group_not_found_if_none
from timApp.user.user import User
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


# TODO: Fix context_group maybe.
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


def check_group_member(usergroup: int) -> bool:
    """
    Checks whether logged in user is a member of usergroup.
    :param usergroup: usergroup to check
    :return: true if user is member of usergroup, false otherwise
    """
    current_user = get_current_user_object()
    context_usergroup = (
        run_sql(select(UserGroup).filter(UserGroup.id == usergroup)).scalars().first()
    )
    allowed_member = (
        run_sql(
            select(UserGroupMember).filter(
                UserGroupMember.user_id == current_user.id,
                UserGroupMember.usergroup_id == context_usergroup.id,
                or_(
                    UserGroupMember.membership_end > datetime_tz.now(),
                    UserGroupMember.membership_end == None,
                ),
            )
        )
        .scalars()
        .first()
    )
    if allowed_member:
        return True
    else:
        return False


# TODO: Not in use. Remove?
@badges_blueprint.get("/all_badges_including_inactive")
def all_badges_including_inactive() -> Response:
    """
    Fetches all badges including the inactive badges. Sorted by created-timestamp.
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
    Fetches all badges except the inactive badges. Sorted by created-timestamp.
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
    Fetches all badges in specific context_group. Sorted by created-timestamp.
    :param doc_id: Current document's ID
    :param user_id: Current user's ID
    :param context_group: Context group to get badges from
    :return: Badges in json response format
    """
    d = DocEntry.find_by_path(f"groups/{context_group}")
    if not d:
        raise NotExist()
    context_usergroup = UserGroup.get_by_name(context_group)
    verify_teacher_access(d)
    badges = (
        run_sql(
            select(Badge)
            .filter(Badge.active, (Badge.context_group == context_usergroup.id))
            .order_by(Badge.created),
        )
        .scalars()
        .all()
    )
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


# TODO: Not in use. Remove?
@badges_blueprint.get("/badge/<badge_id>")
def get_badge(badge_id: int) -> Response:
    """
    Fetches a specific badge.
    :param badge_id: ID of the badge to get
    :return: badge in json response format or error when there is no badge with that id
    """
    badge = run_sql(select(Badge).filter_by(id=badge_id)).scalars().first()
    if badge is None:
        raise NotExist()
        # return error_generic("there's no badge with id: " + str(badge_id), 404)
    badge_json = badge.to_json()
    return json_response(badge_json)


# TODO: Fix return to make testable.
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
    d = DocEntry.find_by_path(f"groups/{context_group}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    context_usergroup = UserGroup.get_by_name(context_group)
    badge = Badge(
        active=True,
        context_group=context_usergroup.id,
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
    # return json_response(badge.to_json(), 200)
    return ok_response()


# TODO: Handle errors.
@badges_blueprint.post("/modify_badge")
def modify_badge(
    badge_id: int,
    modified_by: int,
    doc_id: int,
    context_group: int,
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
    context_group_object = UserGroup.get_by_id(context_group)
    if not context_group_object:
        raise NotExist()
    d = DocEntry.find_by_path(f"groups/{context_group_object.name}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
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
    d = DocEntry.find_by_path(f"groups/{context_group}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
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
    d = DocEntry.find_by_path(f"groups/{context_group}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
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


# TODO: Check access rights.
# TODO: Handle errors.
@badges_blueprint.get("/groups_badges/<group_id>/<context_group>")
def get_groups_badges(group_id: int, context_group: str) -> Response:
    """
    Fetches badges that are given to a usergroup. Sorted by given-timestamp.
    :param group_id: ID of the usergroup
    :param context_group: Name of the context group
    :return: Badges in json response format
    """
    in_group = check_group_member(group_id)
    if not in_group:
        d = DocEntry.find_by_path(f"groups/{context_group}")
        if not d:
            raise NotExist()
        verify_teacher_access(d)
        # group = UserGroup.get_by_id(group_id)
        # # if group is a personal group
        # if group.is_personal_group:
        #     user = User.get_by_name(group.name)
        #     # fetch group memberships where user belongs
        #     users_group_memberships = (
        #         run_sql(
        #             select(UserGroupMember).filter(
        #                 UserGroupMember.user_id == user.id,
        #                 or_(
        #                     UserGroupMember.membership_end > datetime_tz.now(),
        #                     UserGroupMember.membership_end == None,
        #                 ),
        #             )
        #         )
        #         .scalars()
        #         .all()
        #     )
        #     # if there is at least one group in users_group_memberships
        #     # where current user has teacher rights, then approve
        #     for users_group_membership in users_group_memberships:
        #         usergroup = UserGroup.get_by_id(users_group_membership.usergroup_id)
        #         d = DocEntry.find_by_path(f"groups/{usergroup.name}")
        #         if not d:
        #             raise NotExist()
        #         verify_teacher_access(
        #             d
        #         )  # There is a problem because of abort_if_not_access_and_required.
        # # if group is not a personal group
        # else:
        #     d = DocEntry.find_by_path(f"groups/{group.name}")
        #     if not d:
        #         raise NotExist()
        #     verify_teacher_access(d)
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
    badge_ids_unique = []
    badge_ids_badgegiven_ids_and_msgs: dict[str, tuple] = {}
    for badgeGiven in groups_badges_given:
        key_extension = 0
        while (
            str(badgeGiven.badge_id) + "_" + str(key_extension)
        ) in badge_ids_badgegiven_ids_and_msgs.keys():
            key_extension += 1
        badge_ids.append(badgeGiven.badge_id)
        badge_ids_unique.append((badgeGiven.badge_id, key_extension))
        badge_ids_badgegiven_ids_and_msgs[
            str(badgeGiven.badge_id) + "_" + str(key_extension)
        ] = (
            badgeGiven.id,
            badgeGiven.message,
            badgeGiven.given_by,
            badgeGiven.given,
            badgeGiven.withdrawn_by,
            badgeGiven.withdrawn,
            badgeGiven.undo_withdrawn_by,
            badgeGiven.undo_withdrawn,
        )
    context_group_object = UserGroup.get_by_name(context_group)
    groups_badges = (
        run_sql(
            select(Badge)
            .filter_by(active=True)
            .filter(
                Badge.context_group == context_group_object.id, Badge.id.in_(badge_ids)
            )
        )
        .scalars()
        .all()
    )
    groups_badges_ordered = []
    for badge_id in badge_ids:
        for groups_badge in groups_badges:
            if groups_badge.id == badge_id:
                groups_badges_ordered.append(groups_badge)
    badges_json = []
    i = 0
    for groups_badge in groups_badges_ordered:
        groups_badge_json = groups_badge.to_json()
        key = f"{badge_ids_unique[i][0]}_{badge_ids_unique[i][1]}"
        groups_badge_json["badgegiven_id"] = badge_ids_badgegiven_ids_and_msgs[key][0]
        groups_badge_json["message"] = badge_ids_badgegiven_ids_and_msgs[key][1]
        groups_badge_json["given_by"] = badge_ids_badgegiven_ids_and_msgs[key][2]
        groups_badge_json["given"] = badge_ids_badgegiven_ids_and_msgs[key][3]
        groups_badge_json["withdrawn_by"] = badge_ids_badgegiven_ids_and_msgs[key][4]
        groups_badge_json["withdrawn"] = badge_ids_badgegiven_ids_and_msgs[key][5]
        groups_badge_json["undo_withdrawn_by"] = badge_ids_badgegiven_ids_and_msgs[key][
            6
        ]
        groups_badge_json["undo_withdrawn"] = badge_ids_badgegiven_ids_and_msgs[key][7]
        badges_json.append(groups_badge_json)
        i += 1
    for badge_json in badges_json:
        if badge_json["created_by"]:
            created_by = User.get_by_id(badge_json["created_by"])
        else:
            created_by = None
        if badge_json["modified_by"]:
            modified_by = User.get_by_id(badge_json["modified_by"])
        else:
            modified_by = None
        if badge_json["deleted_by"]:
            deleted_by = User.get_by_id(badge_json["deleted_by"])
        else:
            deleted_by = None
        if badge_json["restored_by"]:
            restored_by = User.get_by_id(badge_json["restored_by"])
        else:
            restored_by = None
        if badge_json["given_by"]:
            given_by = User.get_by_id(badge_json["given_by"])
        else:
            given_by = None
        if badge_json["withdrawn_by"]:
            withdrawn_by = User.get_by_id(badge_json["withdrawn_by"])
        else:
            withdrawn_by = None
        if badge_json["undo_withdrawn_by"]:
            undo_withdrawn_by = User.get_by_id(badge_json["undo_withdrawn_by"])
        else:
            undo_withdrawn_by = None
        if created_by:
            badge_json["created_by_name"] = created_by.real_name
        else:
            badge_json["created_by_name"] = None
        if modified_by:
            badge_json["modified_by_name"] = modified_by.real_name
        else:
            badge_json["modified_by_name"] = None
        if deleted_by:
            badge_json["deleted_by_name"] = deleted_by.real_name
        else:
            badge_json["deleted_by_name"] = None
        if restored_by:
            badge_json["restored_by_name"] = restored_by.real_name
        else:
            badge_json["restored_by_name"] = None
        if given_by:
            badge_json["given_by_name"] = given_by.real_name
        else:
            badge_json["given_by_name"] = None
        if withdrawn_by:
            badge_json["withdrawn_by_name"] = withdrawn_by.real_name
        else:
            badge_json["withdrawn_by_name"] = None
        if undo_withdrawn_by:
            badge_json["undo_withdrawn_by_name"] = undo_withdrawn_by.real_name
        else:
            badge_json["undo_withdrawn_by_name"] = None
    return json_response(badges_json)


# TODO: Handle errors.
@badges_blueprint.get("/badge_given/<badge_id>")
def get_badge_given(badge_id: int) -> Response:
    """
    Fetches holders of given badges.
    :param badge_id: ID of the badge
    :return: Badgegivens in json response format
    """
    badge_given = (
        run_sql(
            select(BadgeGiven)
            .filter(BadgeGiven.active)
            .filter(BadgeGiven.badge_id == badge_id)
        )
        .scalars()
        .all()
    )
    return json_response(badge_given)


# TODO: Do access right checks.
@badges_blueprint.get("/badge_holders/<badge_id>")
def get_badge_holders(badge_id: int) -> Response:
    """
    Fetches all usergroups that holds certain badge.
    :param badge_id: Badge ID
    :return: list of users and list of usergroups in json format
    """
    badges_given = (
        run_sql(
            select(BadgeGiven).filter(
                BadgeGiven.badge_id == badge_id, BadgeGiven.active
            )
        )
        .scalars()
        .all()
    )
    group_ids = []
    for badge_given in badges_given:
        group_ids.append(badge_given.group_id)
    unique_group_ids = list(set(group_ids))
    user_groups = []
    for unique_group_id in unique_group_ids:
        user_groups.append(UserGroup.get_by_id(unique_group_id))
    user_accounts = []
    non_personal_groups = []
    for user_group in user_groups:
        if user_group:
            if user_group.is_personal_group:
                user_accounts.append(User.get_by_name(user_group.name))
            else:
                non_personal_groups.append(user_group)
        else:
            NotExist()
    return json_response(
        (
            sorted(list(user_accounts), key=attrgetter("real_name")),
            sorted(list(non_personal_groups), key=attrgetter("name")),
        )
    )


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
    d = DocEntry.find_by_path(f"groups/{context_group}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
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
    d = DocEntry.find_by_path(f"groups/{context_group}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
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
@badges_blueprint.post("/withdraw_all_badges")
def withdraw_all_badges(
    badge_id: int, usergroup_id: int, withdrawn_by: int, context_group: str
) -> Response:
    """
    Withdraws all badges of given ID from a usergroup.
    :param usergroup_id: ID of the usergroup
    :param context_group: Context group where the badge is included
    :param badge_id: ID of the badge
    :param withdrawn_by: ID of the useraccount that withdraws the badge
    :return: ok response
    """
    d = DocEntry.find_by_path(f"groups/{context_group}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    badge_given = {
        "active": False,
        "withdrawn_by": withdrawn_by,
        "withdrawn": datetime_tz.now(),
    }
    BadgeGiven.query.filter(
        BadgeGiven.badge_id == badge_id, BadgeGiven.group_id == usergroup_id
    ).update(badge_given)
    db.session.commit()
    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "withdraw_all_badges",
                "timestamp": badge_given["withdrawn"],
                "badge_id": badge_id,
                "usergroup_id": usergroup_id,
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
    d = DocEntry.find_by_path(f"groups/{context_group}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
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
    d = DocEntry.find_by_path(f"groups/{group_name_prefix}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
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
@badges_blueprint.get("/user_and_personal_group/<name>")
def users_personal_group(name: str) -> Response:
    """
    Fetches user and his/her personal usergroup.
    :param name: User's username
    :return: useraccount and usergroup in json format
    """
    personal_group = UserGroup.get_by_name(name)
    user_account = User.get_by_name(name)
    if personal_group and user_account:
        return json_response((user_account, personal_group))
    return error_generic("there's no user with username: " + name, 404)


# TODO: Handle errors.
@badges_blueprint.get("/usergroups_members/<doc_id>/<usergroup_name>")
def usergroups_members(doc_id: int, usergroup_name: str) -> Response:
    """
    Fetches usergroup's members.
    :param doc_id: Current document's ID
    :param usergroup_name: usergroup's name
    :return: List of users
    """
    usergroup = UserGroup.get_by_name(usergroup_name)
    raise_group_not_found_if_none(usergroup_name, usergroup)
    d = DocEntry.find_by_path(f"groups/{usergroup_name}")
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    return json_response(sorted(list(usergroup.users), key=attrgetter("real_name")))


@badges_blueprint.get("/current_group_name/<name>")
def group_name(name: str):
    """
    Fetches group name from the database.
    :param name: Name of the group given in group changer
    :return: group name
    """
    group = UserGroup.get_by_name(name)
    doc = group.admin_doc
    if not doc:
        raise error_generic("no rights", 404)
    pretty_name = doc.description
    if group:
        return json_response(
            {
                "id": group.id,
                "name": group.name,
                "description": pretty_name,
            }
        )
    return error_generic("there's no group with name: " + name, 404)


@badges_blueprint.post("/editGroupName/<group_name>/<new_name>")
def change_group_name(group_name: str, new_name: str) -> Response:
    """
    Changes groups description block (pretty name)
    :param group_name: full group name
    :param new_name: new group name (pretty name)
    :return: new description
    """
    usergroup = UserGroup.get_by_name(group_name)
    if not usergroup:
        return error_generic("there's no group with name: " + group_name, 404)
    doc = usergroup.admin_doc
    if not doc:
        return error_generic("no rights", 404)
    verify_teacher_access(doc)
    doc.description = new_name
    # doc.save()
    db.session.commit()
    # return ok_response()
    return json_response(
        {"id": usergroup.id, "name": usergroup.name, "description": doc.description}
    )
