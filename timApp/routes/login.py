"""Routes for login view."""
from .common import *
from email.mime.text import MIMEText
from flask import Blueprint, flash, redirect, render_template, request, url_for
from timdb.timdb2 import TimDb

import os
import smtplib
import string
import random
import re
import requests


login_page = Blueprint('login_page',
                        __name__,
                        url_prefix='')  # TODO: Better URL prefix.

def __getRealName(email):
    atindex = email.index('@')
    if atindex <= 0:
        return email
    parts = email[0:atindex].split('.')
    parts2 = [part.capitalize() if len(part) > 1 else part.capitalize() + '.' for part in parts]
    return ' '.join(parts2)
    
def __isValidEmail(email):
    return re.match('^[\w\.-]+@([\w-]+\.)+[\w-]+$', email) is not None

def __sendMail(email, subject, text, sender='no-reply@tim.it.jyu.fi'):
    msg = MIMEText(text)
    msg['Subject'] = subject
    msg['From'] = sender
    msg['To'] = email

    s = smtplib.SMTP('smtp.jyu.fi')
    s.sendmail(sender, [email], msg.as_string())
    s.quit()


@login_page.route("/logout", methods=['POST'])
def logout():
    session.pop('user_id', None)
    session.pop('appcookie', None)
    session['user_name'] = 'Anonymous'
    flash('You were successfully logged out.', 'loginmsg')
    return redirect(url_for('indexPage'))

@login_page.route("/login")
def login():
    if request.args.get('korppiLogin'):
        return loginWithKorppi()
    elif request.args.get('emailLogin'):
        return loginWithEmail()
    elif request.args.get('emailSignup'):
        return signupWithEmail()
    else:
        return "Invalid login request!"

def loginWithKorppi():
    urlfile = request.url_root + "login"
    if request.args.get('came_from'):
        session['came_from'] = request.args.get('came_from')
    if not session.get('appcookie'):
        randomHex = codecs.encode(os.urandom(24), 'hex').decode('utf-8')
        session['appcookie'] = randomHex
    url = "https://korppi.jyu.fi/kotka/interface/allowRemoteLogin.jsp"
    try:
        r = requests.get(url, params={'request': session['appcookie']}, verify=True)
    except requests.exceptions.SSLError:
        return render_template('503.html', message='Korppi seems to be down, so login is currently not possible. '
                                                   'Try again later.'), 503
    
    if r.status_code != 200:
        return render_template('503.html', message='Korppi seems to be down, so login is currently not possible. '
                                                   'Try again later.'), 503
    korppiResponse = r.text.strip()
    #print("korppiresponse is: '{}'".format(korppiResponse))
    if not korppiResponse:
        return redirect(url+"?authorize=" + session['appcookie'] + "&returnTo=" + urlfile, code=303)
    pieces = (korppiResponse + "\n\n").split('\n')
    userName = pieces[0]
    realName = pieces[1]
    email = pieces[2]

    timdb = getTimDb()
    userId = timdb.users.getUserByName(userName)
    
    if userId is None:
        uid = timdb.users.createUser(userName, realName, email)
        gid = timdb.users.createUserGroup(userName)
        timdb.users.addUserToGroup(gid, uid)
        userId = uid
    else:
        if realName:
            timdb.users.updateUser(userId, userName, realName, email)
    session['user_id'] = userId
    session['user_name'] = userName
    session['real_name'] = realName
    session['email'] = email
    flash('You were successfully logged in.', 'loginmsg')
    return redirect(session.get('came_from', '/'))

def loginWithEmail():
    if ('altlogin' in session and session['altlogin'] == 'login'):
        session.pop('altlogin')
    else:
        session['altlogin'] = "login"

    return redirect(session.get('came_from', '/'))

def signupWithEmail():
    if ('altlogin' in session and session['altlogin'] == 'signup'):
        session.pop('altlogin')
    else:
        session['altlogin'] = "signup"

    return redirect(session.get('came_from', '/'))

@login_page.route("/altsignup", methods=['POST'])
def altSignup():
    # Before password verification
    email = request.form['email']
    if not email or not __isValidEmail(email):
        flash("You must supply a valid email address!")
        return redirect(session.get('came_from', '/'))
        
    password = ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(6))
    
    try:
        __sendMail(email, 'Your new TIM password', 'Your password is {}'.format(password))
    except Exception as e:
        flash('Could not send the email, please try again later. The error was: {}'.format(str(e)))
        return redirect(session.get('came_from', '/'))
    
    timdb = getTimDb()
    timdb.users.createPotentialUser(email, password)
    
    #print("Signup: email {}, password {}".format(email, password))
    session.pop('altlogin')
    flash("A password has been sent to you. Please check your email.")
    return redirect(session.get('came_from', '/'))

@login_page.route("/altsignup2", methods=['POST'])
def altSignupAfter():
    # After password verification
    userId = 0
    userName = request.form['name']
    realName = request.form['realname']
    email = session['email']
    oldpass = request.form['token']
    password = request.form['password']
    confirm = request.form['passconfirm']
    timdb = getTimDb()

    if not timdb.users.testPotentialUser(email, oldpass):
        return jsonResponse({'message': 'Authentication failure'}, 403)

    if timdb.users.getUserByEmail(email) is not None:
        # User with this email already exists
        userId = timdb.users.getUserByEmail(email)['id']
        nameId = timdb.users.getUserByName(userName)
        if nameId is not None and nameId != userId:
            flash('User name already exists. Please try another one.', 'loginmsg')
            return redirect(session.get('came_from', '/'))
    else:
        if timdb.users.getUserByName(userName) is not None:
            flash('User name already exists. Please try another one.', 'loginmsg')
            return redirect(session.get('came_from', '/'))
    
    if password != confirm:
        flash('Passwords do not match.', 'loginmsg')
        return redirect(session.get('came_from', '/'))
        
    if len(password) < 6:
        flash('A password should contain at least six characters.', 'loginmsg')
        return redirect(session.get('came_from', '/'))
    
    if userId == 0:
        userId = timdb.users.createUser(userName, realName, email, password=password)
    else:
        timdb.users.updateUser(userId, userName, realName, email, password=password)
    
    timdb.users.deletePotentialUser(email)
    
    session.pop('altlogin')
    session['user_id'] = userId
    session['user_name'] = userName
    session['real_name'] = realName
    flash('You were successfully logged in.', 'loginmsg')
    return redirect(session.get('came_from', '/'))
    
@login_page.route("/altlogin", methods=['POST'])
def altLogin():
    email = request.form['email']
    password = request.form['password']
    timdb = getTimDb()

    if timdb.users.testUser(email, password):
        # Registered user
        user = timdb.users.getUserByEmail(email)
        session.pop('altlogin')
        session['user_id'] = user['id']
        session['user_name'] = user['name']
        session['real_name'] = user['real_name']
        session['email'] = user['email']
        flash('You were successfully logged in.', 'loginmsg')
        
    elif timdb.users.testPotentialUser(email, password):
        # New user
        session['user_id'] = 0
        session['user_name'] = email
        session['real_name'] = __getRealName(email)
        session['email'] = email
        session['altlogin'] = 'signup2'
        session['token'] = password
        
    else:
        flash("Email address or password did not match. Please try again.", 'loginmsg')
    
    return redirect(session.get('came_from', '/'))
    
