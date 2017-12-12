"""Routes for login view."""
import codecs
import os
import random
import re
import string

import requests
import requests.exceptions
from flask import Blueprint, render_template
from flask import abort
from flask import current_app
from flask import flash
from flask import redirect
from flask import request
from flask import session
from flask import url_for
from yubico_client import Yubico
from yubico_client.yubico_exceptions import YubicoError

from timApp.accesshelper import verify_logged_in, verify_admin
from timApp.dbaccess import get_timdb
from timApp.logger import log_error
from timApp.requesthelper import verify_json_params, get_option, is_xhr
from timApp.responsehelper import safe_redirect, json_response, ok_response
from timApp.routes.notify import send_email
from timApp.sessioninfo import get_current_user, get_other_users, get_session_users_ids, get_other_users_as_list, \
    get_current_user_object
from timApp.sessioninfo import get_current_user_id, logged_in
from timApp.timdb.models.newuser import NewUser
from timApp.timdb.models.user import User
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.tim_models import db
from timApp.timdb.timdbexception import TimDbException
from timApp.timdb.userutils import create_password_hash

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


def is_valid_email(email):
    return re.match('^[\w.-]+@([\w-]+\.)+[\w-]+$', email) is not None


@login_page.route("/logout", methods=['POST'])
def logout():
    user_id, = verify_json_params('user_id', require=False)
    if user_id is not None and user_id != get_current_user_id():
        group = get_other_users()
        group.pop(str(user_id), None)
        session['other_users'] = group
    else:
        session.pop('user_id', None)
        session.pop('user_name', None)
        session.pop('email', None)
        session.pop('real_name', None)
        session.pop('appcookie', None)
        session.pop('altlogin', None)
        session.pop('came_from', None)
        session.pop('last_doc', None)
        session.pop('anchor', None)
        session.pop('other_users', None)
        session.pop('adding_user', None)
        session['user_name'] = 'Anonymous'
    return json_response(dict(current_user=get_current_user(), other_users=get_other_users_as_list()))


@login_page.route("/login")
def login():
    save_came_from()
    if logged_in():
        flash('You are already logged in.')
        return safe_redirect(session.get('came_from', '/'))
    if request.args.get('korppiLogin'):
        return login_with_korppi()
    elif request.args.get('emailLogin'):
        return login_with_email()
    elif request.args.get('emailSignup'):
        return signup_with_email()
    else:
        return render_template('loginpage.html',
                               hide_require_text=True,
                               anchor=request.args.get('anchor'))


@login_page.route("/korppiLogin")
def login_with_korppi():
    add_user = get_option(request, 'add_user', False)
    if not logged_in() and add_user:
        return abort(403, 'You must be logged in before adding users to session.')
    if session.get('adding_user') is None:
        session['adding_user'] = add_user
    urlfile = request.url_root + "korppiLogin"
    save_came_from()

    if not session.get('appcookie'):
        random_hex = codecs.encode(os.urandom(24), 'hex').decode('utf-8')
        session['appcookie'] = random_hex
    url = current_app.config['KORPPI_AUTHORIZE_URL']
    korppi_down_text = 'Korppi seems to be down, so login is currently not possible. Try again later.'
    try:
        r = requests.get(url, params={'request': session['appcookie']}, verify=True)
    except (requests.exceptions.SSLError, requests.exceptions.ConnectionError):
        return abort(503, korppi_down_text)

    if r.status_code != 200:
        return abort(503, korppi_down_text)
    korppi_response = r.text.strip()
    if not korppi_response:
        return redirect(url + "?authorize=" + session['appcookie'] + "&returnTo=" + urlfile, code=303)
    pieces = (korppi_response + "\n\n").split('\n')
    user_name = pieces[0]
    real_name = pieces[1]
    email = pieces[2]

    user: User = User.query.filter_by(name=user_name).first()

    if user is None:
        # Try email
        user: User = User.query.filter_by(email=email).first()
        if user is not None:
            # Two possibilities here:
            # 1) An email user signs in using Korppi for the first time. We update the user's username and personal
            # usergroup.
            # 2) Korppi username has been changed (rare but it can happen).
            # In this case, we must not re-add the user to the Korppi group.
            user.update_info(name=user_name, real_name=real_name, email=email)
            korppi_group = UserGroup.get_korppi_group()
            if korppi_group not in user.groups:
                user.groups.append(korppi_group)
        else:
            user, _ = User.create_with_group(user_name, real_name, email)
            user.groups.append(UserGroup.get_korppi_group())
    else:
        if real_name:
            user.update_info(name=user_name, real_name=real_name, email=email)

    db.session.commit()
    set_user_to_session(user)
    return finish_login()


def set_user_to_session(user: User):
    adding = session.get('adding_user')
    session.pop('appcookie', None)
    session.pop('altlogin', None)
    session.pop('adding_user', None)
    if adding:
        if user.id in get_session_users_ids():
            flash(f'{user.real_name} is already logged in.')
            return
        other_users = session.get('other_users', dict())
        other_users[str(user.id)] = user.basic_info_dict
        session['other_users'] = other_users
    else:
        session['user_id'] = user.id
        session['user_name'] = user.name
        session['real_name'] = user.real_name
        session['email'] = user.email
        session.pop('other_users', None)


def login_with_email():
    if 'altlogin' in session and session['altlogin'] == 'login':
        session.pop('altlogin', None)
    else:
        session['altlogin'] = "login"

    return safe_redirect(session.get('came_from', '/'))


def signup_with_email():
    if 'altlogin' in session and session['altlogin'] == 'signup':
        session.pop('altlogin', None)
    else:
        session['altlogin'] = "signup"

    return safe_redirect(session.get('came_from', '/'))


"""Sent passwords are stored here when running tests."""
test_pws = []


@login_page.route("/altsignup", methods=['POST'])
def alt_signup():
    # Before password verification
    email = request.form['email']
    if not email or not is_valid_email(email):
        flash("You must supply a valid email address!")
        return finish_login(ready=False)

    password = ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(6))

    nu = NewUser.query.get(email)
    password_hash = create_password_hash(password)
    if nu:
        nu.pass_ = password_hash
    else:
        nu = NewUser(email=email, pass_=password_hash)
        db.session.add(nu)
    db.session.commit()

    session.pop('altlogin', None)
    session.pop('user_id', None)
    session.pop('appcookie', None)
    session['user_name'] = 'Anonymous'
    session["email"] = email

    try:
        send_email(email, 'Your new TIM password', f'Your password is {password}')
        if current_app.config['TESTING']:
            test_pws.append(password)
        flash("A password has been sent to you. Please check your email.")
    except Exception as e:
        log_error(f'Could not send login email (user: {email}, password: {password}, exception: {str(e)})')
        flash(f'Could not send the email, please try again later. The error was: {str(e)}')

    return finish_login(ready=False)


@login_page.route("/altsignup2", methods=['POST'])
def alt_signup_after():
    # After password verification
    user_id = 0
    real_name = request.form['realname']
    email = session['email']
    username = email
    oldpass = request.form['token']
    password = request.form['password']
    confirm = request.form['passconfirm']
    save_came_from()

    nu = NewUser.query.get(email)
    if not (nu and nu.check_password(oldpass)):
        flash('The temporary password you provided is wrong. Please re-check your email to see the password.')
        return finish_login(ready=False)

    user = User.get_by_email(email)
    if user is not None:
        # User with this email already exists
        user_id = user.id
        u2 = User.get_by_name(username)

        if u2 is not None and u2.id != user_id:
            flash('User name already exists. Please try another one.', 'loginmsg')
            return finish_login(ready=False)

        # Use the existing user name; don't replace it with email
        username = user.name
    else:
        if User.get_by_name(username) is not None:
            flash('User name already exists. Please try another one.', 'loginmsg')
            return finish_login(ready=False)

    if password != confirm:
        flash('Passwords do not match.', 'loginmsg')
        return finish_login(ready=False)

    if len(password) < 6:
        flash('A password should contain at least six characters.', 'loginmsg')
        return finish_login(ready=False)

    if not user:
        flash('Registration succeeded!')
        user, _ = User.create_with_group(username, real_name, email, password=password)
        user_id = user.id
    else:
        flash('Your information was updated successfully.')
        user.update_info(username, real_name, email, password=password)

    db.session.delete(nu)
    db.session.commit()

    session.pop('altlogin', None)
    session['user_id'] = user_id
    session['user_name'] = username
    session['real_name'] = real_name
    return finish_login()


@login_page.route("/altlogin", methods=['POST'])
def alt_login():
    save_came_from()
    email = request.form['email']
    password = request.form['password']
    session['adding_user'] = request.form.get('add_user', 'false').lower() == 'true'

    user = User.get_by_email(email)
    if user is not None:
        old_hash = user.pass_
        if user.check_password(password, allow_old=True, update_if_old=True):
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

    nu = NewUser.query.get(email)
    if nu and nu.check_password(password):
        # New user
        session['user_id'] = 0
        session['user_name'] = email
        session['real_name'] = get_real_name(email)
        session['email'] = email
        session['altlogin'] = 'signup2'
        session['token'] = password

    else:
        error_msg = "Email address or password did not match. Please try again."
        if is_xhr(request):
            return abort(403, error_msg)
        else:
            flash(error_msg, 'loginmsg')

    return finish_login(ready=False)


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
        return safe_redirect(came_from + anchor)
    else:
        return json_response(dict(current_user=get_current_user(), other_users=get_other_users_as_list()))


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
    session['user_name'] = user.name
    session['real_name'] = user.real_name
    session['email'] = user.email
    flash(f"Logged in as: {username}")
    return redirect(url_for('view_page.index_page'))


def get_yubico_client():
    # You need to put the key file in timApp directory
    # The first line is app key (5 numbers) and
    # the second line is app secret (28 characters)

    yubi_key_file = 'yubi.key'
    if not os.path.exists(yubi_key_file):
        return None

    with open(yubi_key_file, 'rt') as f:
        app_key = f.readline().rstrip('\n')
        app_secret = f.readline().rstrip('\n')
        return Yubico(app_key, app_secret)


@login_page.route("/yubi_reg/<otp>")
def yubi_register(otp):
    """For registering a new YubiKey for a user"""
    verify_logged_in()

    client = get_yubico_client()
    if not client:
        abort(403, 'Yubico API keys not configured - see routes/login.py for details')

    try:
        if not client.verify(otp):
            abort(403, 'Authentication failed')
    except YubicoError:
        abort(403, 'Authentication failed')

    get_current_user_object().yubikey = otp[:12]
    db.session.commit()
    return ok_response()


@login_page.route("/yubi_login/<username>/<otp>")
def yubi_login(username, otp):
    """Log in using an YubiKey (see http://www.yubico.com)."""
    user = User.get_by_name(username)
    if user is None:
        abort(404, 'User not found.')

    user_pubid = user.yubikey
    if user_pubid is None or len(user_pubid) != 12:
        abort(403, 'YubiKey login is not enabled for this account.')
    if len(otp) != 44 or otp[:12] != user_pubid:
        abort(403, 'OTP invalid or not registered to this account.')

    client = get_yubico_client()
    if not client:
        abort(403, 'Yubico API keys not configured - see routes/login.py for details')

    try:
        if not client.verify(otp):
            abort(403, 'Authentication failed')
    except YubicoError:
        abort(403, 'Authentication failed')

    session['user_id'] = user.id
    session['user_name'] = user.name
    session['real_name'] = user.real_name
    session['email'] = user.email
    flash(f"Logged in as: {username}")
    return redirect(url_for('view_page.index_page'))


def log_in_as_anonymous(sess) -> User:
    timdb = get_timdb()
    user_name = 'Anonymous'
    user_real_name = 'Guest'
    user = timdb.users.create_anonymous_user(user_name, user_real_name)
    sess['user_id'] = user.id
    sess['user_name'] = user_name
    sess['real_name'] = user_real_name
    return user
