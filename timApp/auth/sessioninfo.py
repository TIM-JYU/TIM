from textwrap import dedent
from typing import Dict, List, Optional

from flask import session, g, request, current_app

from timApp.document.usercontext import UserContext
from timApp.user.user import User


def get_current_user():
    return get_current_user_object().to_json()


def get_current_user_object() -> User:
    if not hasattr(g, 'user'):
        curr_id = get_current_user_id()
        u = User.get_by_id(curr_id)
        if u is None:
            if curr_id != 0:
                curr_id = 0
                session['user_id'] = curr_id
                u = User.get_by_id(curr_id)
        if not u:
            raise Exception(dedent(f"""
            Database has no users; you need to re-initialize it:
            ./dc stop -t 0 tim celery postgresql
            docker volume rm {current_app.config['TIM_NAME']}_data11
            delete tim_files folder
            ./up.sh""").strip())
        g.user = u
    return g.user


def user_context_with_logged_in(u: Optional[User]) -> UserContext:
    curr = get_current_user_object()
    return UserContext(user=u or curr, logged_user=curr)


def get_other_users() -> Dict[str, Dict[str, str]]:
    return session.get('other_users', {})


def get_other_users_as_list() -> List[Dict[str, str]]:
    return list(session.get('other_users', {}).values())


def get_session_users():
    return [get_current_user()] + get_other_users_as_list()


def get_session_users_objs() -> List[User]:
    return User.query.filter(User.id.in_([u['id'] for u in get_session_users()])).all()


def get_session_users_ids():
    return [u['id'] for u in get_session_users()]


def get_session_usergroup_ids():
    return [User.get_by_id(u['id']).get_personal_group().id for u in get_session_users()]


def get_current_user_id() -> int:
    uid = session.get('user_id')
    return uid if uid is not None else 0


def get_current_user_group():
    return get_current_user_group_object().id


def get_current_user_group_object():
    return get_current_user_object().get_personal_group()


def logged_in() -> bool:
    return get_current_user_id() != 0


def save_last_page():
    session['last_doc'] = request.full_path
