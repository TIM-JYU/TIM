"""badge-related routes."""
from typing import cast
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
from timApp.user.subgroups import SubGroup
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
    # if not context_usergroup:
    #     raise NotExist(f"Context group {context_group} does not exist")
    verify_access("teacher", context_usergroup, user_group_name=context_group)

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
    # if not context_usergroup:
    #     raise NotExist(f"Context group {context_group} does not exist")
    verify_access("teacher", context_usergroup, user_group_name=context_group)

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
    :param context_group: Name of the context group where the badge will be included
    :param title: Title of the badge
    :param color: Color of the badge
    :param shape: Shape of the badge
    :param image: Image of the badge
    :param description: Description of the badge
    :return: Modified badge in json format
    """
    # verify_access raises NotExist when the group does not exist, so context_usergroup
    # is set by the time it returns.
    context_usergroup = UserGroup.get_by_name(context_group)
    verify_access("teacher", context_usergroup, user_group_name=context_group)

    new_badge = {
        # The column stores the group id; only the route parameter is a name.
        "context_group": context_usergroup.id,
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


@badges_blueprint.post("/deactivate_badge")
def deactivate_badge(badge_id: int, context_group: str) -> Response:
    """
    Deactivates a badge.
    :param context_group: Context group where the badge is included
    :param badge_id: ID of the badge
    :return: Info of deleted badge in json format
    """
    context_usergroup = UserGroup.get_by_name(context_group)
    # if not context_usergroup:
    #     raise NotExist(f"Context group {context_group} does not exist")
    verify_access("teacher", context_usergroup, user_group_name=context_group)

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
    _user_group = user_group
    if not user_group:
        if user_group_name:
            _user_group = UserGroup.get_by_name(user_group_name)
        elif user_group_id:
            _user_group = UserGroup.get_by_id(user_group_id)
        else:
            raise NotExist(f"User group was not provided.")
        if not _user_group:
            raise NotExist(f'User group "{user_group_name or user_group_id}" not found')

    block = _user_group.admin_doc if _user_group else None
    if not block:
        raise NotExist(f'Admin doc for user group "{_user_group.name}" not found')

    if access_type == "teacher":
        verify_teacher_access(
            block,
            message=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{_user_group.name}", please contact TIM admin.',
        )
    elif access_type == "view":
        verify_view_access(
            block,
            message=f"Sorry, you don't have permission to use this resource.",
        )


@badges_blueprint.get("/group_badges/<int:group_id>/<context_group>")
def get_groups_badges(group_id: int, context_group: str) -> Response:
    """
    Fetches the badges given to a user group, oldest first.

    Awards and templates are returned as two separate lists rather than merged, so that
    an award carries only what belongs to it and refers to its template through
    ``badge_id``. Only awards whose template is active and belongs to the context group
    are included, and every returned award's template is present in ``templates``.

    :param group_id: ID of the user group
    :param context_group: Name of the context group
    :return: ``{"badges": [...], "templates": [...]}``, where each badge additionally
             carries ``given_by_name`` and each template ``created_by_name``
    """
    if group_id == "undefined":
        raise NotExist("User group not found")
    usergroup = UserGroup.get_by_id(group_id)
    if not usergroup:
        raise NotExist(f'User group with id "{group_id}" not found')
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f'User group "{context_group}" not found')

    # Badges belong to a course, and belonging to that course is what grants the right
    # to see them: any member of the context group may read the badges of any group
    # within it. Members of a subgroup are members of the context group as well (see
    # timApp.user.subgroups), so a student on a coding camp sees their camp's badges
    # through their own student group. Teachers of the context group may look without
    # being members of it.
    current_user = get_current_user_object()
    if not check_group_member(current_user, context_usergroup.id):
        verify_access("teacher", context_usergroup, user_group_name=context_group)

    awards = (
        run_sql(
            select(Badge)
            .filter(Badge.active, Badge.group_id == group_id)
            .order_by(Badge.given)
        )
        .scalars()
        .all()
    )

    templates = (
        run_sql(
            select(BadgeTemplate)
            .filter_by(active=True)
            .filter(
                BadgeTemplate.context_group == context_usergroup.id,
                BadgeTemplate.id.in_({award.badge_id for award in awards}),
            )
        )
        .scalars()
        .all()
    )
    template_ids = {template.id for template in templates}

    # Resolve each display name once rather than once per row.
    user_names: dict[int, str | None] = {}

    def name_of(user_id: int | None) -> str | None:
        if not user_id:
            return None
        if user_id not in user_names:
            user = User.get_by_id(user_id)
            user_names[user_id] = user.real_name if user else None
        return user_names[user_id]

    badges_json = []
    for award in awards:
        # Drop awards whose template is inactive or belongs to another context group.
        if award.badge_id not in template_ids:
            continue
        award_json = award.to_json()
        award_json["given_by_name"] = name_of(award.given_by)
        badges_json.append(award_json)

    templates_json = []
    for template in templates:
        template_json = template.to_json()
        template_json["created_by_name"] = name_of(template.created_by)
        templates_json.append(template_json)

    return json_response({"badges": badges_json, "templates": templates_json})


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
    # if not context_usergroup:
    #     raise NotExist(f"Context group {badge.context_group} does not exist")
    verify_access("teacher", context_usergroup, user_group_id=badge.context_group)

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
    # if not context_usergroup:
    #     raise NotExist(f"{context_group} not found")
    verify_access("teacher", context_usergroup, user_group_name=context_group)

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
    # if not context_usergroup:
    #     raise NotExist(f"{context_group} not found")
    verify_access("teacher", context_usergroup, user_group_name=context_group)

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


@badges_blueprint.get("/podium/<context_group>")
def podium(context_group: str) -> Response:
    """
    Fetches the 5 subgroups of the given group that have been given the most badges.

    Which groups count as subgroups comes from the usergroup_subgroups table; it used
    to be inferred from a shared name prefix, which also matched unrelated groups that
    happened to start with the same characters.

    :param context_group: Name of the context group
    :return: 5 subgroups with most badges in json format
    """
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f'User group "{context_group}" not found')

    current_user = get_current_user_object()

    if not context_usergroup in current_user.groups:
        verify_access("teacher", context_usergroup, user_group_name=context_group)

    # Inner joins: only subgroups that hold at least one active badge can place, which
    # is what the previous outerjoin + WHERE on the joined tables already amounted to.
    results = run_sql(
        select(UserGroup.name, func.count(Badge.id).label("badge_count"))
        .join(SubGroup, SubGroup.child_id == UserGroup.id)
        .join(Badge, Badge.group_id == UserGroup.id)
        .join(BadgeTemplate, BadgeTemplate.id == Badge.badge_id)
        .where(
            (SubGroup.parent_id == context_usergroup.id)
            & Badge.active.is_(True)
            & BadgeTemplate.active.is_(True)
        )
        .group_by(UserGroup.id, UserGroup.name)
        # Name breaks ties so that equal badge counts come back in a stable order.
        .order_by(desc("badge_count"), UserGroup.name)
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


@badges_blueprint.get("/user_podium/<context_group>")
def user_podium(context_group: str) -> Response:
    """
    Fetches the 10 members of the given group that have been given the most badges.

    The group counterpart of this route is :func:`podium`; this one ranks the individual
    members instead of the subgroups. A badge given to a person is recorded against
    their personal user group, so the count comes from joining each member to the user
    group that shares their name.

    Subgroup membership is materialised (see :mod:`timApp.user.subgroups`), so members
    of a subgroup are members of the context group as well and place here without the
    subgroup table being consulted. Memberships that have already ended do not count.

    Unlike :func:`podium`, this counts only badges whose template belongs to the context
    group. A personal user group follows its owner from course to course and collects
    the badges of every one of them, so without that restriction a member's score here
    would include badges earned somewhere else entirely.

    :param context_group: Name of the context group
    :return: 10 members with most badges in json format
    """
    context_usergroup = UserGroup.get_by_name(context_group)
    if not context_usergroup:
        raise NotExist(f'User group "{context_group}" not found')

    current_user = get_current_user_object()

    if not context_usergroup in current_user.groups:
        verify_access("teacher", context_usergroup, user_group_name=context_group)

    # Inner joins throughout: only members that hold at least one active badge of this
    # context group can place, so there is nothing to outer join in.
    results = run_sql(
        select(User.name, User.real_name, func.count(Badge.id).label("badge_count"))
        .join(UserGroupMember, UserGroupMember.user_id == User.id)
        # A personal user group is the group named after its owner; that name equality
        # is the whole definition (see UserGroup.is_personal_group).
        .join(UserGroup, UserGroup.name == User.name)
        .join(Badge, Badge.group_id == UserGroup.id)
        .join(BadgeTemplate, BadgeTemplate.id == Badge.badge_id)
        .where(
            (UserGroupMember.usergroup_id == context_usergroup.id)
            & or_(
                UserGroupMember.membership_end == None,
                UserGroupMember.membership_end > datetime_tz.now(),
            )
            & Badge.active.is_(True)
            & BadgeTemplate.active.is_(True)
            & (BadgeTemplate.context_group == context_usergroup.id)
        )
        .group_by(User.id, User.name, User.real_name)
        # Name breaks ties so that equal badge counts come back in a stable order.
        .order_by(desc("badge_count"), User.real_name, User.name)
        .limit(10)
    ).all()

    podium_json = []
    for user_name, real_name, badge_count in results:
        podium_json.append(
            {
                "user_name": user_name,
                # Users without a real name are shown under their username.
                "real_name": real_name or user_name,
                "badge_count": badge_count,
            }
        )

    return json_response(podium_json)
