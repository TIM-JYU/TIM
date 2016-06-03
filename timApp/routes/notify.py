import http.client
import socket
import os

from flask import Blueprint, abort, request
from decorators import async
from routes.common import *
from routes.logger import log_message
from tim_app import app
from typing import Optional


FUNNEL_HOST = "funnel"
FUNNEL_PORT = 80

notify = Blueprint('notify',
                   __name__,
                   url_prefix='')


@notify.route('/notify/<int:doc_id>', methods=['GET'])
def get_notify_settings(doc_id):
    verifyLoggedIn()
    timdb = getTimDb()
    settings = timdb.documents.get_notify_settings(getCurrentUserId(), doc_id)
    return jsonResponse(settings)


@notify.route('/notify/<int:doc_id>', methods=['POST'])
def set_notify_settings(doc_id):
    verifyLoggedIn()
    jsondata = request.get_json()
    timdb = getTimDb()
    timdb.documents.set_notify_settings(getCurrentUserId(), doc_id, jsondata)
    return okJsonResponse()


@async
def send_email(rcpt: str, subject: str, msg: str, mail_from: Optional[str] = None, reply_to: Optional[str] = None,
               group_id: Optional[str] = None, group_subject: Optional[str] = None):
    with app.app_context():
        conn = None
        try:
            headers = {
                "Host": "tim",
                "Accept-Encoding": "text/plain",
                "Encoding": "text/html",
                "Connection": "close",
                "Subject": subject,
                "Rcpt-To": rcpt}

            if mail_from:
                headers['From'] = mail_from
            if reply_to:
                headers['Reply-To'] = reply_to
            if group_id and group_subject:
                headers['Group-Id'] = group_id
                headers['Group-Subject'] = group_subject

            conn = http.client.HTTPConnection(FUNNEL_HOST, port=FUNNEL_PORT)
            conn.request("POST", "/mail", body=msg.replace('\n', '<br>').encode('utf-8'), headers=headers)
            log_message("Sending email to " + rcpt, 'INFO')

            response = conn.getresponse()
            if response.status != 200:
                log_message('Response from funnel: {} {}'.format(response.status, response.reason), 'ERROR')

        except (ConnectionError, socket.error, http.client.error) as e:
            log_message("Couldn't connect to funnel: " + str(e), 'ERROR')

        finally:
            if conn is not None:
                conn.close()


def replace_macros(msg: str, doc_id: int, par_id: Optional[str]) -> str:
    timdb = getTimDb()
    new_msg = msg
    if '[user_name]' in msg:
        new_msg = new_msg.replace('[user_name]', getCurrentUserName())
    if '[doc_name]' in msg or '[doc_url]' in msg:
        doc_name = timdb.documents.get_first_document_name(doc_id)
        par_part = '' if par_id is None else '#' + par_id
        doc_url = 'http://{}/view/{}{}'.format(os.environ.get("TIM_HOST", "localhost"), doc_name.replace(' ', '%20'),
                                               par_part)
        new_msg = new_msg.replace('[doc_name]', doc_name).replace('[doc_url]', doc_url)

    return new_msg


def notify_doc_owner(doc_id, subject, msg, setting=None, par_id=None, group_id=None, group_subject=None):
    timdb = getTimDb()
    me = get_current_user()
    owner_group = timdb.documents.get_owner(doc_id)
    macro_subject = replace_macros(subject, doc_id, par_id)
    macro_msg = replace_macros(msg, doc_id, par_id)
    macro_msg += '\n\n--\This message was automatically sent by TIM'

    macro_grpsubj = replace_macros(group_subject, doc_id, par_id) if group_subject else None

    for user in timdb.users.get_users_in_group(owner_group):
        if user['id'] != me['id'] and user['email']:
            if setting is not None:
                settings = timdb.documents.get_notify_settings(user['id'], doc_id)
                if not settings['email_' + setting]:
                    continue

            send_email(user['email'], macro_subject, macro_msg, mail_from=me['email'],
                       group_id=group_id, group_subject=macro_grpsubj)

