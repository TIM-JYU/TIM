"""Routes for login view."""
import random
import re
import string
import urllib.parse
from typing import Optional

from dataclasses import dataclass
from flask import Blueprint, render_template
from flask import abort
from flask import current_app
from flask import flash
from flask import redirect
from flask import request
from flask import session
from flask import url_for

from timApp.admin.user_cli import do_merge_users, do_soft_delete
from timApp.auth.accesshelper import verify_admin, AccessDenied
from timApp.auth.sessioninfo import get_current_user_id, logged_in
from timApp.auth.sessioninfo import get_other_users, get_session_users_ids, get_other_users_as_list, \
    get_current_user_object
from timApp.korppi.openid import KorppiOpenIDResponse
from timApp.notification.notify import send_email
from timApp.tim_app import oid, get_home_organization_group
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.newuser import NewUser
from timApp.user.personaluniquecode import PersonalUniqueCode
from timApp.user.user import User, UserOrigin, UserInfo
from timApp.user.usergroup import UserGroup
from timApp.user.users import create_anonymous_user
from timApp.user.userutils import create_password_hash, check_password_hash
from timApp.util.flask.requesthelper import verify_json_params, get_option, is_xhr, use_model, RouteException
from timApp.util.flask.responsehelper import safe_redirect, json_response, ok_response, error_generic
from timApp.util.logger import log_error, log_warning, log_info
from timApp.util.utils import is_valid_email

login_page = Blueprint('login_page',
                       __name__,
                       url_prefix='')  # TODO: Better URL prefix.


def get_real_name(email):
    atindex = email.index('@')
    if atindex <= 0:
        return email
    parts = email[0:atindex].split('.')
    parts2 = [part.capitalize() if len(part) > 1 else part.capitalize() + '.' for part in parts]
    return ' '.join(parts2)


@login_page.route("/logout", methods=['POST'])
def logout():
    user_id, = verify_json_params('user_id', require=False)
    if user_id is not None and user_id != get_current_user_id():
        group = get_other_users()
        group.pop(str(user_id), None)
        session['other_users'] = group
    else:
        session.clear()
    return login_response()


def login_response():
    return json_response(dict(current_user=get_current_user_object().to_json(full=True),
                              other_users=get_other_users_as_list()))


@login_page.route("/login")
def login():
    save_came_from()
    if logged_in():
        flash('You are already logged in.')
        return safe_redirect(session.get('came_from', '/'))
    return render_template('loginpage.html',
                           text='Please use the above button to log in.',
                           anchor=request.args.get('anchor'))


@login_page.route("/korppiLogin")
def login_with_korppi():
    return safe_redirect('/openIDLogin?provider=korppi')


def create_or_update_user(
        info: UserInfo,
        group_to_add: Optional[UserGroup]=None,
        update_username: bool = True,
) -> User:
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


@login_page.route("/openIDLogin")
@oid.loginhandler
def login_with_openid():
    add_user = get_option(request, 'add_user', False)
    if not logged_in() and add_user:
        raise AccessDenied('You must be logged in before adding users to session.')
    if not add_user and logged_in():
        flash("You're already logged in.")
        return finish_login()
    if session.get('adding_user') is None:
        session['adding_user'] = add_user

    provider = get_option(request, 'provider', None)
    if provider != 'korppi':
        return abort(400, 'Unknown OpenID provider. Only korppi is supported so far.')
    save_came_from()
    # see possible fields at http://openid.net/specs/openid-simple-registration-extension-1_0.html
    return oid.try_login(current_app.config['OPENID_IDENTITY_URL'],
                         ask_for_optional=['email', 'fullname', 'firstname', 'lastname'])


username_parse_error = 'Could not parse username from OpenID response.'


class KorppiEmailException(Exception):
    code = 400
    description = ""


@login_page.errorhandler(KorppiEmailException)
def already_exists(error: KorppiEmailException):
    return error_generic(error.description, 400, template='korppi_email_error.html')


@oid.after_login
def openid_success_handler(resp: KorppiOpenIDResponse):
    m = re.fullmatch('https://korppi.jyu.fi/openid/account/([a-z]+)', resp.identity_url)
    if not m:
        return abort(400, username_parse_error)
    username = m.group(1)
    if not username:
        return abort(400, username_parse_error)
    if not resp.email:
        # Allow existing users to log in even if Korppi didn't give email.
        u = User.get_by_name(username)
        if u:
            log_warning(f'Existing user did not have email in Korppi: {username}')
            set_user_to_session(u)
            return finish_login()
        log_warning(f'New user did not have email in Korppi: {username}')
        raise KorppiEmailException()
    if not resp.fullname:
        return abort(400, 'Missing fullname')
    if not resp.firstname:
        return abort(400, 'Missing firstname')
    if not resp.lastname:
        return abort(400, 'Missing lastname')
    fullname = f'{resp.lastname} {resp.firstname}'
    user = create_or_update_user(
        UserInfo(email=resp.email, full_name=fullname, username=username, origin=UserOrigin.Korppi),
        group_to_add=get_home_organization_group(),
    )
    db.session.commit()
    set_user_to_session(user)
    return finish_login()


def set_user_to_session(user: User):
    adding = session.pop('adding_user', None)
    if adding:
        if user.id in get_session_users_ids():
            flash(f'{user.real_name} is already logged in.')
            return
        other_users = session.get('other_users', dict())
        other_users[str(user.id)] = user.to_json()
        session['other_users'] = other_users
    else:
        session['user_id'] = user.id
        session.pop('other_users', None)


"""Sent passwords are stored here when running tests."""
test_pws = []


@login_page.route("/checkTempPass", methods=['POST'])
def check_temp_password():
    """Checks that the temporary password provided by user is correct.
    Sends the real name of the user if the email already exists so that the name field can be prefilled.
    """
    email_or_username, token, = verify_json_params('email', 'token')
    nu = check_temp_pw(convert_email_to_lower(email_or_username), token)
    u = User.get_by_email(nu.email)
    if u:
        return json_response({'status': 'name', 'name': u.real_name, 'can_change_name': u.is_email_user})
    else:
        return ok_response()


@dataclass
class AltSignupModel:
    email: str
    url: Optional[str] = None


@login_page.route("/altsignup", methods=['POST'])
@use_model(AltSignupModel)
def alt_signup(m: AltSignupModel):
    if not current_app.config['EMAIL_REGISTRATION_ENABLED']:
        raise AccessDenied('Email registration is disabled.')
    email_or_username = m.email
    fail = False
    is_email = False
    if not is_valid_email(email_or_username):
        u = User.get_by_name(email_or_username)
        if not u:
            # Don't return immediately; otherwise it is too easy to analyze timing of the route to deduce success.
            fail = True
            email = 'nobody@example.com'
        else:
            email = u.email
    else:
        is_email = True
        email = email_or_username.lower()

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
    if is_email and m.url and not (email.endswith('.fi') or email.endswith('@gmail.com')):
        log_warning(f'Bot registration attempt: {email}, URL: {m.url}')
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
        return abort(400, f'Could not send the email, please try again later. The error was: {str(e)}')


def check_temp_pw(email_or_username: str, oldpass: str) -> NewUser:
    nu = NewUser.query.get(email_or_username)
    if not nu:
        u = User.get_by_name(email_or_username)
        if u:
            nu = NewUser.query.get(u.email)
    if not (nu and nu.check_password(oldpass)):
        return abort(400, 'WrongTempPassword')
    return nu


@dataclass
class AltSignup2Model:
    email: str
    passconfirm: str
    password: str
    realname: str
    token: str


@login_page.route("/altsignup2", methods=['POST'])
@use_model(AltSignup2Model)
def alt_signup_after(m: AltSignup2Model):
    if m.password != m.passconfirm:
        return abort(400, 'PasswordsNotMatch')

    min_pass_len = current_app.config['MIN_PASSWORD_LENGTH']
    if len(m.password) < min_pass_len:
        return abort(400, f'PasswordTooShort')

    save_came_from()
    email_or_username = convert_email_to_lower(m.email)

    nu = check_temp_pw(email_or_username, m.token)
    email = nu.email
    username = email
    real_name = m.realname
    user = User.get_by_email(email)
    if user is not None:
        # User with this email already exists
        user_id = user.id
        u2 = User.get_by_name(username)

        if u2 is not None and u2.id != user_id:
            return abort(400, 'UserAlreadyExists')

        # Use the existing user name; don't replace it with email
        username = user.name
        success_status = 'updated'

        # If the user isn't an email user, don't let them change name
        # (because it has been provided by other system such as Korppi).
        if not user.is_email_user:
            real_name = user.real_name

        user.update_info(UserInfo(username=username, full_name=real_name, email=email, password=m.password))
    else:
        if User.get_by_name(username) is not None:
            return abort(400, 'UserAlreadyExists')
        success_status = 'registered'
        user, _ = User.create_with_group(
            UserInfo(
                username=username,
                full_name=real_name,
                email=email,
                password=m.password,
                origin=UserOrigin.Email,
            )
        )
        db.session.flush()
        user_id = user.id

    db.session.delete(nu)
    db.session.commit()

    session['user_id'] = user_id
    return json_response({'status': success_status})


def is_possibly_home_org_account(email_or_username: str):
    return bool(re.fullmatch(
        '[a-z]{2,8}|.+@([a-z]+\.)*' + re.escape(current_app.config['HOME_ORGANIZATION']),
        email_or_username,
    ))


def check_password_and_stripped(user: User, password: str) -> bool:
    if user.check_password(password, allow_old=True, update_if_old=True):
        return True
    return user.check_password(password.strip(), allow_old=True, update_if_old=True)


@login_page.route("/altlogin", methods=['POST'])
def alt_login():
    save_came_from()
    email_or_username = request.form['email'].strip()
    password = request.form['password']
    session['adding_user'] = request.form.get('add_user', 'false').lower() == 'true'

    email_or_username = convert_email_to_lower(email_or_username)

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
            set_user_to_session(user)

            # if password hash was updated, save it
            if old_hash != user.pass_:
                db.session.commit()
            return finish_login()
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
    if is_xhr(request):
        return abort(403, error_msg)
    else:
        flash(error_msg, 'loginmsg')
    return finish_login(ready=False)


def convert_email_to_lower(email_or_username):
    if is_valid_email(email_or_username):
        return email_or_username.lower()
    return email_or_username


def save_came_from():
    came_from = request.args.get('came_from') or request.form.get('came_from')
    if came_from:
        session['came_from'] = came_from
        session['last_doc'] = came_from
    else:
        session['came_from'] = session.get('last_doc', '/')
    session['anchor'] = request.args.get('anchor') or request.form.get('anchor') or ''


def finish_login(ready=True):
    if not ready:
        return safe_redirect(url_for('start_page'))

    anchor = session.get('anchor', '')
    if anchor:
        anchor = "#" + anchor
    came_from = session.get('came_from', '/')
    if not is_xhr(request):
        return safe_redirect(urllib.parse.unquote(came_from) + anchor)
    else:
        return login_response()


@login_page.route("/quickLogin/<username>")
def quick_login(username):
    """A debug helping method for logging in as another user.

    For developer use only.

    """
    verify_admin()
    user = User.get_by_name(username)
    if user is None:
        abort(404, 'User not found.')
    session['user_id'] = user.id
    flash(f"Logged in as: {username}")
    return redirect(url_for('view_page.index_page'))


def log_in_as_anonymous(sess) -> User:
    user_name = 'Anonymous'
    user_real_name = 'Guest'
    user = create_anonymous_user(user_name, user_real_name)
    db.session.flush()
    sess['user_id'] = user.id
    return user
