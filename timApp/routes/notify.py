import http.client
import socket
from typing import Optional

from flask import Blueprint
from flask import request

from accesshelper import verify_logged_in, verify_view_access
from dbaccess import get_timdb
from decorators import async
from documentmodel.docparagraph import DocParagraph
from logger import log_info, log_error
from requesthelper import verify_json_params
from responsehelper import json_response, ok_response
from sessioninfo import get_current_user_id, get_current_user_object
from tim_app import app
from timdb.docinfo import DocInfo
from timdb.models.docentry import DocEntry
from timdb.models.notification import NotificationType
from timdb.models.user import User
from timdb.tim_models import db

FUNNEL_HOST = "funnel"
FUNNEL_PORT = 80

notify = Blueprint('notify',
                   __name__,
                   url_prefix='')

sent_mails_in_testing = []


@notify.route('/notify/<int:doc_id>', methods=['GET'])
def get_notify_settings(doc_id):
    verify_logged_in()
    verify_view_access(doc_id)
    return json_response(
        get_current_user_object().get_notify_settings(DocEntry.find_by_id(doc_id, try_translation=True)))


@notify.route('/notify/<int:doc_id>', methods=['POST'])
def set_notify_settings(doc_id):
    verify_logged_in()
    verify_view_access(doc_id)
    comment_modify, comment_add, doc_modify = verify_json_params('email_comment_modify', 'email_comment_add',
                                                                 'email_doc_modify')
    get_current_user_object().set_notify_settings(DocEntry.find_by_id(doc_id, try_translation=True),
                                                  comment_modify=comment_modify,
                                                  comment_add=comment_add,
                                                  doc_modify=doc_modify)
    db.session.commit()
    return ok_response()


@async
def send_email(rcpt: str, subject: str, msg: str, mail_from: Optional[str] = None, reply_to: Optional[str] = None,
               group_id: Optional[str] = None, group_subject: Optional[str] = None):
    if app.config['TESTING']:
        sent_mails_in_testing.append(locals())
        return

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
            log_info("Sending email to " + rcpt)

            response = conn.getresponse()
            if response.status != 200:
                log_error('Response from funnel: {} {}'.format(response.status, response.reason))

        except (ConnectionError, socket.error, http.client.error) as e:
            log_error("Couldn't connect to funnel: {} - {}".format(e, msg))

        finally:
            if conn is not None:
                conn.close()


def notify_doc_watchers(doc: DocInfo,
                        content_msg: str,
                        notify_type: NotificationType,
                        par: Optional[DocParagraph] = None):
    me = get_current_user_object()
    if notify_type == NotificationType.DocModified:
        subject = 'Someone edited the document {}'.format(doc.title)
        subject_full = '{} edited the document {}'.format(me.pretty_full_name, doc.title)
        group_subject = 'The document {} has been modified'.format(doc.title)
        group_id = 'docmodify_{}'.format(doc.id)
    elif notify_type == NotificationType.CommentAdded:
        subject = 'Someone posted a comment to the document {}'.format(doc.title)
        subject_full = '{} posted a comment to the document {}'.format(me.pretty_full_name, doc.title)
        group_subject = 'The document {} has new notes'.format(doc.title)
        group_id = 'notes_{}'.format(doc.id)
    elif notify_type == NotificationType.CommentModified:
        subject = 'Someone edited a comment in the document {}'.format(doc.title)
        subject_full = '{} edited a comment in the document {}'.format(me.pretty_full_name, doc.title)
        group_subject = 'The document {} has new notes'.format(doc.title)
        group_id = 'notes_{}'.format(doc.id)
    else:
        assert False, 'Unknown NotificationType'
    msg = 'Link to the {}: {}{}'.format('paragraph' if par else 'document',
                                        doc.url,
                                        '#' + par.get_id() if par else '')

    full_msg = msg + '\n\n' + content_msg

    for note in doc.get_notifications(notify_type):
        user = note.user  # type: User
        if user.id == me.id or not user.email:
            continue

        # If a document was modified and the user doesn't have edit access to it, we must not send the source md
        send_full_msg = notify_type != NotificationType.DocModified or user.has_edit_access(doc.id)

        # Poster identity should be hidden unless the user has teacher access to the document
        send_full_subject = user.has_teacher_access(doc.id)
        reply_to = me.email if user.has_teacher_access(doc.id) else None
        send_email(user.email,
                   subject_full if send_full_subject else subject,
                   full_msg if send_full_msg else msg,
                   mail_from='tim@jyu.fi',
                   reply_to=reply_to,
                   group_id=group_id, group_subject=group_subject)
