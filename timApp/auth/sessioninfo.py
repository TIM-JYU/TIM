from textwrap import dedent
from typing import Dict, List, Optional

from flask import session, g, request, current_app
from sqlalchemy.orm import joinedload

from timApp.document.usercontext import UserContext
from timApp.user.user import User, user_query_with_joined_groups


def get_current_user():
    return get_current_user_object().to_json()


def get_current_user_object() -> User:
    if not hasattr(g, 'user'):
        curr_id = get_current_user_id()
        u = user_query_with_joined_groups().options(joinedload(User.lectures)).get(curr_id)
        if u is None:
            if curr_id != 0:
                curr_id = 0
                session['user_id'] = curr_id
                session['user_name'] = 'Anonymous'
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


def user_context_with_logged_in(u: Optional[User], answer_count: Optional[int] = 0) -> UserContext:
    curr = get_current_user_object()
    return UserContext(user=u or curr, logged_user=curr, answer_nr=answer_count or 0)


def get_other_users() -> Dict[str, Dict[str, str]]:
    return session.get('other_users', {})


def get_other_users_as_list() -> List[Dict[str, str]]:
    return list(session.get('other_users', {}).values())


def get_session_users():
    return [get_current_user()] + get_other_users_as_list()


def get_session_users_objs() -> List[User]:
    return get_users_objs(get_session_users())


def get_other_session_users_objs() -> List[User]:
    return get_users_objs(get_other_users_as_list())


def get_users_objs(lis) -> List[User]:
    return User.query.filter(User.id.in_([u['id'] for u in lis])).all()


def get_session_users_ids() -> List[int]:
    return [u['id'] for u in get_session_users()]


def get_session_usergroup_ids():
    return [User.get_by_id(u['id']).get_personal_group().id for u in get_session_users()]


def get_current_user_id() -> int:
    uid = session.get('user_id')
    return uid if uid is not None else 0


def get_current_user_name() -> str:
    if not logged_in():
        return 'Anonymous'
    name = session.get('user_name')
    if not name:
        u = get_current_user_object()
        session['user_name'] = u.name
        name = u.name
    return name


def get_current_user_group():
    return get_current_user_group_object().id


def get_current_user_group_object():
    return get_current_user_object().get_personal_group()


def logged_in() -> bool:
    return get_current_user_id() != 0


def save_last_page():
    session['last_doc'] = request.full_path
