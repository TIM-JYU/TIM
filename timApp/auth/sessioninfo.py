from textwrap import dedent

from flask import session, g, request, has_request_context
from sqlalchemy.orm import joinedload

from timApp.document.usercontext import UserContext
from timApp.user.user import User, user_query_with_joined_groups
from timApp.user.usergroup import UserGroup


def get_current_user() -> dict:
    return get_current_user_object().to_json()


def clear_session() -> None:
    session.clear()
    g.pop("user", None)


def get_current_user_object() -> User:
    if not hasattr(g, "user"):
        curr_id = get_current_user_id()
        u = (
            user_query_with_joined_groups()
            .options(joinedload(User.lectures))
            .get(curr_id)
        )
        if u is None:
            if curr_id != 0:
                curr_id = 0
                session["user_id"] = curr_id
                session["user_name"] = "Anonymous"
                u = User.get_by_id(curr_id)
        if not u:
            raise Exception(
                dedent(
                    f"""
            Database has no users; you need to re-initialize it:
            ./tim dc down -v
            <delete tim_files folder>
            ./tim up"""
                ).strip()
            )
        g.user = u
    return g.user


def user_context_with_logged_in(u: User | None) -> UserContext:
    curr = get_current_user_object()
    return UserContext(user=u or curr, logged_user=curr)


def user_context_with_logged_in_or_anon() -> UserContext:
    if not has_request_context():
        anon_user = User.get_anon()
        return UserContext(user=anon_user, logged_user=anon_user)
    return user_context_with_logged_in(None)


def get_other_users() -> dict[str, dict[str, str]]:
    return session.get("other_users", {})


def get_other_users_as_list() -> list[dict[str, str]]:
    return list(session.get("other_users", {}).values())


def get_session_users() -> list[dict]:
    return [get_current_user()] + get_other_users_as_list()


def get_session_users_objs() -> list[User]:
    return get_users_objs(get_session_users())


def get_other_session_users_objs() -> list[User]:
    return get_users_objs(get_other_users_as_list())


def get_users_objs(lis) -> list[User]:
    return User.query.filter(User.id.in_([u["id"] for u in lis])).all()


def get_session_users_ids() -> list[int]:
    return [u["id"] for u in get_session_users()]


def get_session_usergroup_ids() -> list[int]:
    return [
        User.get_by_id(u["id"]).get_personal_group().id for u in get_session_users()
    ]


def get_current_user_id() -> int:
    uid = session.get("user_id")
    return uid if uid is not None else 0


def get_current_user_name() -> str:
    if not logged_in():
        return "Anonymous"
    name = session.get("user_name")
    if not name:
        u = get_current_user_object()
        session["user_name"] = u.name
        name = u.name
    return name


def get_current_session_id() -> str | None:
    return session.get("session_id", None)


def get_current_user_group() -> int:
    return get_current_user_group_object().id


def get_current_user_group_object() -> UserGroup:
    return get_current_user_object().get_personal_group()


def logged_in() -> bool:
    return get_current_user_id() != 0


def save_last_page() -> None:
    session["last_doc"] = request.full_path
