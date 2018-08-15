from typing import Dict, List

from flask import session, g, request

from timApp.user.user import User


def get_current_user():
    return get_current_user_object().to_json()


def get_current_user_object() -> User:
    if not hasattr(g, 'user'):
        while True:
            u = User.query.get(get_current_user_id())
            if u is None:
                session['user_id'] = 0
            else:
                break
        g.user = u
    return g.user


def get_other_users() -> Dict[str, Dict[str, str]]:
    return session.get('other_users', {})


def get_other_users_as_list() -> List[Dict[str, str]]:
    return list(session.get('other_users', {}).values())


def get_session_users():
    return [get_current_user()] + get_other_users_as_list()


def get_session_users_objs():
    return [User.query.get(u['id']) for u in get_session_users()]


def get_session_users_ids():
    return [u['id'] for u in get_session_users()]


def get_session_usergroup_ids():
    return [User.query.get(u['id']).get_personal_group().id for u in get_session_users()]


def get_current_user_id():
    uid = session.get('user_id')
    return uid if uid is not None else 0


def get_current_user_group():
    return get_current_user_object().get_personal_group().id


def logged_in():
    return get_current_user_id() != 0


def current_user_in_lecture():
    lectures = get_current_user_object().lectures.all()
    return bool(lectures and lectures[0].is_running)


def get_user_settings():
    return session.get('settings', {})


def save_last_page():
    session['last_doc'] = request.path
