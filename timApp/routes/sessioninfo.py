from typing import Dict, List

from flask import session, g

from timdb.models.user import User


def get_current_user():
    return {'id': get_current_user_id(),
            'name': get_current_user_name(),
            'real_name': session.get('real_name'),
            'email': session.get('email')}


def get_current_user_object() -> User:
    if not hasattr(g, 'user'):
        g.user = User.query.get(get_current_user_id())
    return g.user


def get_other_users() -> Dict[int, Dict[str, str]]:
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


def get_current_user_name():
    name = session.get('user_name')
    return name if name is not None else 'Anonymous'


def get_current_user_group():
    return get_current_user_object().get_personal_group().id


def logged_in():
    return get_current_user_id() != 0
