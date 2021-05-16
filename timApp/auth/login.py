"""Routes related to email signup and login."""
import random
import re
import string
from typing import Optional, Dict, Union

from flask import current_app, Response
from flask import flash
from flask import render_template
from flask import request
from flask import session
from flask import url_for
from flask.sessions import SecureCookieSession

from timApp.admin.user_cli import do_merge_users, do_soft_delete
from timApp.auth.accesshelper import verify_admin, AccessDenied, verify_ip_ok
from timApp.auth.sessioninfo import get_current_user_id, logged_in
from timApp.auth.sessioninfo import get_other_users, get_session_users_ids, get_other_users_as_list, \
    get_current_user_object
from timApp.notification.send_email import send_email
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.newuser import NewUser
from timApp.user.personaluniquecode import PersonalUniqueCode
from timApp.user.user import User, UserOrigin, UserInfo
from timApp.user.usergroup import UserGroup
from timApp.user.users import create_anonymous_user
from timApp.user.userutils import create_password_hash, check_password_hash
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import safe_redirect, json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_error, log_warning, log_info
from timApp.util.utils import is_valid_email

login_page = TypedBlueprint(
    'login_page',
    __name__,
    url_prefix='',  # TODO: Better URL prefix.
)


def get_real_name(email: str) -> str:
    atindex = email.index('@')
    if atindex <= 0:
        return email
    parts = email[0:atindex].split('.')
    parts2 = [part.capitalize() if len(part) > 1 else part.capitalize() + '.' for part in parts]
    return ' '.join(parts2)


@login_page.route("/logout", methods=['POST'])
def logout(user_id: Optional[int] = None) -> Response:
    if user_id is not None and user_id != get_current_user_id():
        group = get_other_users()
        group.pop(str(user_id), None)
        session['other_users'] = group
    else:
        session.clear()
    return login_response()


def login_response() -> Response:
    return json_response(login_user_data())


def login_user_data() -> Dict:
    return dict(
        current_user=get_current_user_object().to_json(full=True),
        other_users=get_other_users_as_list(),
    )


@login_page.route("/login")
def login(anchor: Optional[str] = None) -> Union[Response, str]:
    save_came_from()
    if logged_in():
        flash('You are already logged in.')
        return safe_redirect(session.get('came_from', '/'))
    return render_template('loginpage.jinja2',
                           text='Please use the above button to log in.',
                           anchor=anchor)


def create_or_update_user(
        info: UserInfo,
        group_to_add: Optional[UserGroup]=None,
        update_username: bool = True,
) -> User:
    assert info.username is not None
    user = User.get_by_name(info.username)

    if user is None and info.email:
        user = User.get_by_email(info.email)
    if user is None and info.unique_codes:
        for uc in info.unique_codes:
            puc = PersonalUniqueCode.find_by_code(code=uc.code, codetype=uc.codetype, org=uc.org)
            if puc:
                user = puc.user
                break

    if user is not None:
        if user.email != info.email and info.email:
            ue = user.get_by_email(info.email)
            if ue and user != ue:
                log_warning(f'Merging users during login: {user.name} and {ue.name}')
                do_merge_users(user, ue)
                do_soft_delete(ue)
                db.session.flush()
            elif ue:
                raise Exception(f'Users were the same but still different email: {user.name}')
        if not update_username:
            info.username = None
        user.update_info(info)
    else:
        user, _ = User.create_with_group(info)

    if group_to_add and group_to_add not in user.groups:
        user.groups.append(group_to_add)
    return user


def set_user_to_session(user: User) -> None:
    adding = session.pop('adding_user', None)
    if adding:
        if user.id in get_session_users_ids():
            flash(f'{user.real_name} is already logged in.')
            return
        other_users = session.get('other_users', dict())
        other_users[str(user.id)] = user.to_json()
        session['other_users'] = other_users
    else:
        set_single_user_to_session(user)


def set_single_user_to_session(user: User) -> None:
    session['user_id'] = user.id

    # We also store user name to session because we don't want to have to access the database every request
    # just to get the user name. Otherwise we would have to access the database for logging the request.
    session['user_name'] = user.name

    session.pop('other_users', None)


"""Sent passwords are stored here when running tests."""
test_pws = []


@login_page.route("/checkTempPass", methods=['POST'])
def check_temp_password(email: str, token: str) -> Response:
    """Checks that the temporary password provided by user is correct.
    Sends the real name of the user if the email already exists so that the name field can be prefilled.
    """
    email_or_username = email
    nu = check_temp_pw(convert_email_to_lower(email_or_username), token)
    u = User.get_by_email(nu.email)
    if u:
        return json_response({'status': 'name', 'name': u.real_name, 'can_change_name': u.real_name is None})
    else:
        return ok_response()


@login_page.route("/emailSignup", methods=['POST'])
def email_signup(email: str, url: Optional[str] = None, reset_password: bool = False) -> Response:
    """Begins email signup process or resets a password for a user.

    :param email: Email or username.
    :param url: A fake parameter that should not be provided by human users.
      This is a primitive method for catching bots.
    :param reset_password: Whether the user is resetting a password.
    :return: ok_response()
    """
    if not is_email_registration_enabled() and not reset_password:
        raise AccessDenied('Email registration is disabled.')
    return do_email_signup_or_password_reset(email, url, only_password_reset=reset_password)


def is_email_registration_enabled() -> bool:
    return current_app.config['EMAIL_REGISTRATION_ENABLED']


def do_email_signup_or_password_reset(
        email_or_u: str,
        url: Optional[str] = None,
        force_fail: bool = False,
        only_password_reset: bool = False,
) -> Response:
    email_or_username = email_or_u.strip()
    fail = force_fail
    is_email = False
    if not is_valid_email(email_or_username):
        u = User.get_by_name(email_or_username)
        if not u:
            log_warning(f'Invalid email/username in registration: "{email_or_username}"')
            # Don't return immediately; otherwise it is too easy to analyze timing of the route to deduce success.
            fail = True
            email = 'nobody@example.com'
        else:
            email = u.email
    else:
        is_email = True
        email = email_or_username.lower()
        if only_password_reset and not User.get_by_email_case_insensitive(email):
            fail = True

    if only_password_reset and not current_app.config['PASSWORD_RESET_ENABLED']:
        raise AccessDenied('PasswordResetDisabled')

    password = ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(6))

    nu = NewUser.query.get(email)
    password_hash = create_password_hash(password)
    if nu:
        nu.pass_ = password_hash
    else:
        nu = NewUser(email=email, pass_=password_hash)
        db.session.add(nu)

    # Real users should never submit the url parameter.
    # It is meant for catching bots.
    if is_email and url and not (email.endswith('.fi') or email.endswith('@gmail.com')):
        log_warning(f'Bot registration attempt: {email}, URL: {url}')
        fail = True

    if fail:
        # We return ok because we don't want to leak any information about the existence of accounts etc.
        return ok_response()

    db.session.commit()

    session.pop('user_id', None)

    try:
        log_info(f'Sending temp password {password} to {email}')
        send_email(email, 'Your new TIM password', f'Your password is {password}')
        if current_app.config['TESTING']:
            test_pws.append(password)
        return ok_response()
    except Exception as e:
        log_error(f'Could not send login email (user: {email}, password: {password}, exception: {str(e)})')
        raise RouteException(f'Could not send the email, please try again later. The error was: {str(e)}')


def check_temp_pw(email_or_username: str, oldpass: str) -> NewUser:
    nu = NewUser.query.get(email_or_username)
    if not nu:
        u = User.get_by_name(email_or_username)
        if u:
            nu = NewUser.query.get(u.email)
    if not (nu and nu.check_password(oldpass)):
        log_warning(f'Wrong temp password for "{email_or_username}": "{oldpass}"')
        raise RouteException('WrongTempPassword')
    return nu


@login_page.route("/emailSignupFinish", methods=['POST'])
def email_signup_finish(
        email: str,
        passconfirm: str,
        password: str,
        realname: Optional[str],
        token: str,
) -> Response:
    """Finished the email signup or password reset process.

    :param email: Email or username.
    :param passconfirm: New password.
    :param password: New password again.
    :param realname: Full name of the user. Will be disregarded if the user already has a name set.
    :param token: The temporary password provided by TIM.
    :return: {'status': 'updated' | 'registered'}
    """
    if password != passconfirm:
        raise RouteException('PasswordsNotMatch')

    min_pass_len = current_app.config['MIN_PASSWORD_LENGTH']
    if len(password) < min_pass_len:
        raise RouteException(f'PasswordTooShort')

    save_came_from()
    email_or_username = convert_email_to_lower(email)

    nu = check_temp_pw(email_or_username, token)
    nu_email = nu.email
    username = nu_email
    real_name = realname
    user = User.get_by_email(nu_email)
    verify_ip_ok(user)
    if user is not None:
        # User with this email already exists
        user_id = user.id
        u2 = User.get_by_name(username)

        if u2 is not None and u2.id != user_id:
            raise RouteException('UserAlreadyExists')

        # Use the existing user name; don't replace it with email
        username = user.name
        success_status = 'updated'

        # We disallow name change if the name has already been set.
        if user.real_name is not None:
            real_name = user.real_name

        user.update_info(UserInfo(username=username, full_name=real_name, email=nu_email, password=password))
    else:
        if User.get_by_name(username) is not None:
            raise RouteException('UserAlreadyExists')
        success_status = 'registered'
        user, _ = User.create_with_group(
            UserInfo(
                username=username,
                full_name=real_name,
                email=nu_email,
                password=password,
                origin=UserOrigin.Email,
            )
        )
        db.session.flush()

    db.session.delete(nu)
    db.session.commit()

    set_user_to_session(user)
    return json_response({'status': success_status})


def is_possibly_home_org_account(email_or_username: str) -> bool:
    return bool(re.fullmatch(
        r'[a-z]{2,8}|.+@([a-z]+\.)*' + re.escape(current_app.config['HOME_ORGANIZATION']),
        email_or_username,
    ))


def check_password_and_stripped(user: User, password: str) -> bool:
    if user.check_password(password, allow_old=True, update_if_old=True):
        return True
    return user.check_password(password.strip(), allow_old=True, update_if_old=True)


@login_page.route("/emailLogin", methods=['POST'])
def email_login(
        email: str,
        password: str,
        add_user: bool = False,
) -> Response:
    """Logs a user in.

    :param email: Email or username.
    :param password: Password.
    :param add_user: Whether the user is adding a user to the session.
    :return: See login_user_data().
    """
    save_came_from()
    session['adding_user'] = add_user
    return json_response(do_email_login(email, password))


def do_email_login(
        email_or_u: str,
        password: str,
) -> Dict:
    email_or_username = convert_email_to_lower(email_or_u)

    users = User.get_by_email_case_insensitive_or_username(email_or_username)
    if len(users) == 1:
        user = users[0]
        old_hash = user.pass_
        if check_password_and_stripped(user, password):
            # Check if the users' group exists
            try:
                user.get_personal_group()
            except TimDbException:
                ug = UserGroup(name=user.name)
                user.groups.append(ug)
                db.session.commit()
            verify_ip_ok(user)

            set_user_to_session(user)

            # if password hash was updated, save it
            if old_hash != user.pass_:
                db.session.commit()
            return login_user_data()
        else:
            log_warning(f'Failed login (wrong password): {email_or_username}')
    elif not users:
        log_warning(f'Failed login (account not found): {email_or_username}')
        # Protect from timing attacks.
        for _ in range(2):
            check_password_hash('', '$2b$12$zXpqPI7SNOWkbmYKb6QK9ePEUe.0pxZRctLybWNE1nxw0/WMiYlPu')
    else:
        log_warning(f'Failed login (multiple accounts): {email_or_username}')
        raise RouteException('AmbiguousAccount')

    error_msg = "EmailOrPasswordNotMatch"
    if is_possibly_home_org_account(email_or_username) and current_app.config['HAKA_ENABLED']:
        error_msg = 'EmailOrPasswordNotMatchUseHaka'
    raise AccessDenied(error_msg)


@login_page.route("/simpleLogin/email", methods=['POST'])
def simple_login(email: str) -> Response:
    """Begins simple email login process if simple email login is enabled.

    :param email: Email or username.
    :return: ok_response().
    """
    verify_simple_email_login_enabled()
    users = User.get_by_email_case_insensitive_or_username(convert_email_to_lower(email))

    if len(users) == 0:
        return do_email_signup_or_password_reset(email, force_fail=not is_email_registration_enabled())
    elif len(users) == 1 and users[0].pass_ is None:
        return do_email_signup_or_password_reset(email)
    elif len(users) > 1:
        raise RouteException('AmbiguousAccount')
    else:
        # The user already has a password - no need to do anything.
        return ok_response()


@login_page.route("/simpleLogin/password", methods=['post'])
def simple_login_password(email: str, password: str) -> Response:
    """Continues simple email login process if simple email login is enabled.

    :param email: Email or username.
    :param password: Password. If the user is signing up, this is the temporary password provided by TIM. Otherwise,
      it is the user's self-made password.
    """
    verify_simple_email_login_enabled()
    users = User.get_by_email_case_insensitive_or_username(convert_email_to_lower(email))
    if len(users) == 1 and users[0].pass_ is not None:
        return json_response({'type': 'login', 'data': do_email_login(email, password)})
    elif len(users) <= 1:
        can_change_name = len(users) == 0 or users[0].real_name is None
        name = None
        if users and users[0].real_name:
            name = users[0].real_name
        try:
            check_temp_pw(email, password)
        except RouteException:
            raise AccessDenied('EmailOrPasswordNotMatch')
        else:
            return json_response({
                'type': 'registration',
                'data': {'can_change_name': can_change_name, 'name': name},
            })
    else:
        raise RouteException('AmbiguousAccount')


def verify_simple_email_login_enabled() -> None:
    if not current_app.config['SIMPLE_EMAIL_LOGIN']:
        raise RouteException('Simple email login is not enabled.')


def convert_email_to_lower(email_or_username: str) -> str:
    email_or_username = email_or_username.strip()
    if is_valid_email(email_or_username):
        return email_or_username.lower()
    return email_or_username


def save_came_from() -> None:
    session['came_from'] = session.get('last_doc', '/')
    session['anchor'] = request.args.get('anchor') or request.form.get('anchor') or ''


@login_page.route("/quickLogin/<username>")
def quick_login(username: str) -> Response:
    """A debug helping method for logging in as another user.

    For developer use only.

    """
    verify_admin()
    user = User.get_by_name(username)
    if user is None:
        raise NotExist('User not found.')
    set_single_user_to_session(user)
    flash(f"Logged in as: {username}")
    return safe_redirect(url_for('view_page.index_page'))


def log_in_as_anonymous(sess: SecureCookieSession) -> User:
    user_name = 'Anonymous'
    user_real_name = 'Guest'
    user = create_anonymous_user(user_name, user_real_name)
    db.session.flush()
    sess['user_id'] = user.id
    return user
