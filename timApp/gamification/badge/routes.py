"""badge-related routes."""
from typing import cast
from collections import defaultdict
from dataclasses import dataclass
from operator import attrgetter
from pathlib import Path
from flask import Response, current_app
from sqlalchemy import select, func, desc, or_
from timApp.auth.accesshelper import (
    verify_teacher_access,
    verify_view_access,
)
from timApp.auth.sessioninfo import get_current_user_object
from timApp.gamification.badge.badges import BadgeTemplate, Badge
from timApp.item.block import Block
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.flask.requesthelper import NotExist
from timApp.util.flask.responsehelper import (
    json_response,
    to_json_str,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_info

badges_blueprint = TypedBlueprint("badges", __name__, url_prefix="/badges")


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
    modified: datetime_tz | None
    deleted: datetime_tz | None


# Bit of a dirty hack to get rid of spurious Mypy errors
def check_and_coerce_not_none(block: Block | None) -> Block:
    if block is not None:
        return cast(Block, block)
    raise NotExist(f"The item does not exist: {block}")


def log_badge_event(log_info: dict) -> None:
    """
    Logs all events that modifies badge or badgegiven tables. Log file can be
    found at ../timapp/tim_logs/badge.log.
    :param log_info: Info that is about to be logged in badge.log file
    :return:
    """
    path = Path(current_app.config["BADGE_LOG_PATH"])
    with path.open("a", encoding="utf-8") as f:
        f.write(to_json_str(log_info) + "\n")


@badges_blueprint.get("/check_connection")
def check_connection() -> Response:
    """
    Checks connection to backend.
    :return: True in json response if connection is working
    """
    return json_response(True)


@badges_blueprint.get("/all_badges/<context_group>")
def all_badges(context_group: str) -> Response:
    """
    Fetches all badge templates for a specific context_group. Sorted by created-timestamp.
    :param context_group: Context group to get badges from
    :return: Badges in json response format
    """
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f"Context group {context_group} does not exist")
    verify_teacher_access(check_and_coerce_not_none(context_usergroup.admin_doc))

    badges = (
        run_sql(
            select(BadgeTemplate)
            .filter(
                BadgeTemplate.active,
                (BadgeTemplate.context_group == context_usergroup.id),
            )
            .order_by(BadgeTemplate.created),
        )
        .scalars()
        .all()
    )
    badges_json = []
    for badge in badges:
        badges_json.append(badge.to_json())
    return json_response(badges_json)


@badges_blueprint.post("/create_badge")
def create_badge(
    context_group: str,
    title: str,
    color: str,
    shape: str,
    image: int,
    description: str,
) -> Response:
    """
    Creates a new badge.
    :param context_group: Context group where the badge will be included
    :param title: Title of the badge
    :param color: Color of the badge
    :param shape: Shape of the badge
    :param image: Image of the badge
    :param description: Description of the badge
    :return: Created badge in json format
    """
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f"Context group {context_group} does not exist")
    verify_teacher_access(check_and_coerce_not_none(context_usergroup.admin_doc))

    badge = BadgeTemplate(
        active=True,
        context_group=context_usergroup.id,
        title=title,
        color=color,
        shape=shape,
        image=image,
        description=description,
        created_by=get_current_user_object().id,
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
    return json_response(badge.to_json(), 200)


@badges_blueprint.post("/modify_badge")
def modify_badge(
    badge_id: int,
    context_group: int,
    title: str,
    color: str,
    shape: str,
    image: int,
    description: str,
) -> Response:
    """
    Modifies a badge.
    :param badge_id: ID of the badge
    :param context_group: Context group where the badge will be included
    :param title: Title of the badge
    :param color: Color of the badge
    :param shape: Shape of the badge
    :param image: Image of the badge
    :param description: Description of the badge
    :return: Modified badge in json format
    """
    context_usergroup = UserGroup.get_by_id(context_group)
    if not context_usergroup:
        raise NotExist(f"Context group {context_group} does not exist")
    verify_teacher_access(check_and_coerce_not_none(context_usergroup.admin_doc))

    new_badge = {
        "context_group": context_group,
        "title": title,
        "color": color,
        "shape": shape,
        "image": image,
        "description": description,
        # "modified_by": get_current_user_object().id,
        "modified": datetime_tz.now(),
    }
    old_badge = run_sql(select(BadgeTemplate).filter_by(id=badge_id)).scalars().first()
    if old_badge is None:
        raise NotExist(f'Badge with id "{badge_id}" not found')
    BadgeTemplate.query.filter_by(id=badge_id).update(new_badge)
    db.session.commit()
    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "modify_badge",
                "timestamp": new_badge["modified"],
                "id": badge_id,
                # "executor": new_badge["modified_by"],
                "context_group": new_badge["context_group"],
                "title": new_badge["title"],
                "color": new_badge["color"],
                "shape": new_badge["shape"],
                "image": new_badge["image"],
                "description": new_badge["description"],
            }
        )
    return json_response(new_badge, 200)


@badges_blueprint.post("/deactivate_badge/<int:badge_id>/<context_group>")
def deactivate_badge(badge_id: int, context_group: str) -> Response:
    """
    Deactivates a badge.
    :param context_group: Context group where the badge is included
    :param badge_id: ID of the badge
    :return: Info of deleted badge in json format
    """
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f"Context group {context_group} does not exist")
    verify_teacher_access(check_and_coerce_not_none(context_usergroup.admin_doc))

    new_badge = {
        "active": False,
        # "deleted_by": get_current_user_object().id,
        "deleted": datetime_tz.now(),
    }
    old_badge = run_sql(select(BadgeTemplate).filter_by(id=badge_id)).scalars().first()
    if old_badge is None:
        raise NotExist(f'Badge with id "{badge_id}" not found')
    BadgeTemplate.query.filter_by(id=badge_id).update(new_badge)
    db.session.commit()
    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "delete_badge",
                "timestamp": new_badge["deleted"],
                "id": badge_id,
                # "executor": new_badge["deleted_by"],
            }
        )
    return json_response(new_badge, 200)


def check_group_member(current_user: User, usergroup: int) -> bool:
    """
    Checks whether logged in user is a member of user group.
    :param current_user: Logged in user
    :param usergroup: User group to check
    :return: True if user is member of user group, false otherwise
    """
    context_usergroup = (
        run_sql(select(UserGroup).filter(UserGroup.id == usergroup)).scalars().first()
    )
    allowed_member = None
    if context_usergroup:
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


def verify_access(
    access_type: str,
    user_group: UserGroup | None,
    user_group_name: str | None = None,
    user_group_id: int | None = None,
) -> None:
    """
    Checks whether logged in user has particular access to a given user group.
    :param access_type: Access type. Either 'teacher' or 'view'.
    :param user_group: User group to check.
    :param user_group_name: Name of user group.
    :param user_group_id: ID of user group.
    :return:
    """
    if not user_group:
        if user_group_name:
            raise NotExist(f'User group "{user_group_name}" not found')
        elif user_group_id:
            raise NotExist(f'User group with id "{user_group_id}" not found')
        else:
            raise NotExist(f"User group not found")

    block = user_group.admin_doc
    if not block:
        raise NotExist(f'Admin doc for user group "{user_group.name}" not found')

    if access_type == "teacher":
        verify_teacher_access(
            block,
            message=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{user_group.name}", please contact TIM admin.',
        )
    elif access_type == "view":
        verify_view_access(
            block,
            message=f"Sorry, you don't have permission to use this resource.",
        )


@badges_blueprint.get("/group_badges/<int:group_id>/<context_group>")
def get_groups_badges(group_id: int, context_group: str) -> Response:
    """
    Fetches badges that are given to a user group. Sorted by given-timestamp.
    :param group_id: ID of the user group
    :param context_group: Name of the context group
    :return: Badges in json response format
    """
    if group_id == "undefined":
        raise NotExist("User group not found")
    usergroup = UserGroup.get_by_id(group_id)
    if not usergroup:
        raise NotExist(f'User group with id "{group_id}" not found')
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f'User group "{context_group}" not found')

    current_user = get_current_user_object()
    in_group = check_group_member(current_user, group_id)
    if not in_group:
        try:
            verify_access("view", usergroup, user_group_id=group_id)
        except NotExist:
            verify_access("teacher", context_usergroup, user_group_name=context_group)

    # log_info(f"Current user's groups: {current_user.groups}")
    #
    # if usergroup not in current_user.groups:
    #     block = usergroup.admin_doc
    #     if not block:
    #         raise NotExist(f"No admin doc for group id {group_id}")
    #     if not verify_view_access(block, require=False):
    #         block = context_usergroup.admin_doc
    #         if not block:
    #             raise NotExist(f"No admin doc for context group {context_group}")
    #         verify_teacher_access(
    #             block,
    #             message=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{context_group}", please contact TIM admin.',
    #         )

    groups_badges_given = (
        run_sql(
            select(Badge)
            .filter(Badge.active, Badge.group_id == group_id)
            .order_by(Badge.given)
        )
        .scalars()
        .all()
    )

    badge_map = defaultdict(list)
    for bg in groups_badges_given:
        badge_map[bg.badge_id].append(bg)
    badge_ids = list(badge_map.keys())

    badges = (
        run_sql(
            select(BadgeTemplate)
            .filter_by(active=True)
            .filter(
                BadgeTemplate.context_group == context_usergroup.id,
                BadgeTemplate.id.in_(badge_ids),
            )
        )
        .scalars()
        .all()
    )

    valid_badge_ids = {b.id for b in badges}
    badge_id_to_badge = {b.id: b for b in badges}

    badges_json = []
    for badge_id in badge_ids:
        if badge_id not in valid_badge_ids:
            continue
        badge = badge_id_to_badge[badge_id]
        for i, badge_given in enumerate(badge_map[badge_id]):
            badge_json = badge.to_json()
            badge_json.update(
                {
                    "badgegiven_id": badge_given.id,
                    "message": badge_given.message,
                    "given_by": badge_given.given_by,
                    "given": badge_given.given,
                    "withdrawn": badge_given.withdrawn,
                }
            )

            user_fields = [
                "created_by",
                "given_by",
            ]
            for field in user_fields:
                uid = badge_json.get(field)
                user = User.get_by_id(uid) if uid else None
                badge_json[f"{field}_name"] = user.real_name if user else None

            badges_json.append(badge_json)

    return json_response(badges_json)


@badges_blueprint.get("/badge_holders/<badge_id>")
def get_badge_holders(badge_id: int) -> Response:
    """
    Fetches all users and user groups that holds certain badge.
    :param badge_id: Badge ID
    :return: List of users and list of user groups in json format
    """
    badge = BadgeTemplate.get_by_id(badge_id)
    if not badge:
        raise NotExist(f'Badge with id "{badge_id}" not found')
    context_usergroup = UserGroup.get_by_id(badge.context_group)
    if not context_usergroup:
        raise NotExist(f"Context group {badge.context_group} does not exist")
    verify_teacher_access(check_and_coerce_not_none(context_usergroup.admin_doc))

    badges_given = (
        run_sql(select(Badge).filter(Badge.badge_id == badge_id, Badge.active))
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
            NotExist(f"User group not found")
    return json_response(
        (
            sorted(list(user_accounts), key=attrgetter("real_name")),
            sorted(list(non_personal_groups), key=attrgetter("name")),
        )
    )


@badges_blueprint.post("/give_badge")
def give_badge(
    context_group: str,
    group_id: int,
    badge_id: int,
    message: str,
) -> Response:
    """
    Gives a badge to a user group.
    :param context_group: Context group where the badge is included
    :param group_id: ID of the user group that the badge is given
    :param badge_id: ID of the badge that is given to the user group
    :param message: Message to give to the userg roup when the badge is given
    :return: Given badge in json format
    """
    badge = BadgeTemplate.get_by_id(badge_id)
    if not badge:
        raise NotExist(f'Badge with id "{badge_id}" not found')
    usergroup = UserGroup.get_by_id(group_id)
    if not usergroup:
        raise NotExist(f'User group with id "{group_id}" not found')
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f"{context_group} not found")
    verify_teacher_access(check_and_coerce_not_none(context_usergroup.admin_doc))

    badge_given = Badge(
        active=True,
        group_id=group_id,
        badge_id=badge_id,
        message=message,
        given_by=get_current_user_object().id,
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
    return json_response(badge_given.to_json(), 200)


@badges_blueprint.post("/withdraw_badge")
def withdraw_badge(badge_given_id: int, context_group: str) -> Response:
    """
    Withdraws a badge from a user group.
    :param context_group: Context group where the badge is included
    :param badge_given_id: ID of the badgegiven
    :return: info of withdrawn badge in json format
    """
    badge_given_old = Badge.get_by_id(badge_given_id)
    if not badge_given_old:
        raise NotExist(f'Given badge with id "{badge_given_id}" not found')
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f"{context_group} not found")
    verify_teacher_access(check_and_coerce_not_none(context_usergroup.admin_doc))

    badge_given_new = {
        "active": False,
        "withdrawn": datetime_tz.now(),
    }
    Badge.query.filter_by(id=badge_given_id).update(badge_given_new)
    db.session.commit()
    if current_app.config["BADGE_LOG_FILE"]:
        log_badge_event(
            {
                "event": "withdraw_badge",
                "timestamp": badge_given_new["withdrawn"],
                "id": badge_given_id,
                "active": badge_given_new["active"],
            }
        )
    return json_response(badge_given_new, 200)


@badges_blueprint.get("/podium/<group_name_prefix>")
def podium(group_name_prefix: str) -> Response:
    """
    Fetches 5 user groups with given group name prefix that has had the most badges.
    :param group_name_prefix: Context group
    :return: 5 subgroups with most badges in json format
    """
    context_usergroup = UserGroup.get_by_name(group_name_prefix)
    if not context_usergroup:
        raise NotExist(f'User group "{group_name_prefix}" not found')

    current_user = get_current_user_object()

    if not context_usergroup in current_user.groups:
        verify_teacher_access(check_and_coerce_not_none(context_usergroup.admin_doc))

    results = run_sql(
        select(UserGroup.name, func.count(Badge.id).label("badge_count"))
        .filter(
            UserGroup.name.like(group_name_prefix + "%"),
            UserGroup.name != group_name_prefix,
        )
        .outerjoin(Badge, Badge.group_id == UserGroup.id)
        .where(Badge.active.is_(True))
        .outerjoin(BadgeTemplate, BadgeTemplate.id == Badge.badge_id)
        .where(BadgeTemplate.active.is_(True))
        .group_by(UserGroup.id, UserGroup.name)
        .order_by(desc("badge_count"))
        .limit(5)
    ).all()

    podium_json = []
    for grp_name, badge_count in results:
        podium_json.append(
            {
                "group_name": grp_name,
                "badge_count": badge_count,
            }
        )

    return json_response(podium_json)
