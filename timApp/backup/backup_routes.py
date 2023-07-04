from flask import Response
from sqlalchemy import select

from timApp.answer.backup import save_answer_backup
from timApp.answer.exportedanswer import ExportedAnswer
from timApp.tim_app import csrf
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember, membership_current
from timApp.util.flask.requesthelper import NotExist
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.secret import check_secret

backup = TypedBlueprint(
    "backup",
    __name__,
    url_prefix="/backup/",
)


@backup.post("answer")
@csrf.exempt
def receive_answer_backup(answer: ExportedAnswer, token: str) -> Response:
    return save_answer_backup(answer, token)


@backup.post("user/memberships")
@csrf.exempt
def receive_user_memberships(
    email: str, secret: str, memberships: list[str]
) -> Response:
    check_secret(secret, "SYNC_USER_GROUPS_RECEIVE_SECRET")

    user = User.get_by_email(email)
    if not user:
        raise NotExist("User does not exist")

    current_memberships = {ug.name for ug in user.groups}
    changed_memberships = set(memberships)

    new_memberships = changed_memberships - current_memberships
    removed_memberships = current_memberships - changed_memberships

    for group_name in new_memberships:
        ug = UserGroup.get_by_name(group_name)
        if not ug:
            continue
        user.add_to_group(ug, None)

    if removed_memberships:
        removed_memberships_objs: list[UserGroupMember] = db.session.execute(
            select(UserGroupMember).join(UserGroup, UserGroupMember.group)
            .join(User, UserGroupMember.user)
            .filter(
                (User.name == user.name)
                & UserGroup.name.in_(removed_memberships)
                & membership_current
            )
        ).scalars()

        for ugm in removed_memberships_objs:
            ugm.set_expired()

    db.session.commit()
    return ok_response()
