import http.client
import socket
from typing import Optional

from flask import Blueprint
from flask import abort

from timApp.accesshelper import verify_logged_in, verify_view_access, get_doc_or_abort
from timApp.decorators import async
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.logger import log_info, log_error
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import json_response, ok_response
from timApp.sessioninfo import get_current_user_object
from timApp.tim_app import app
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.notification import NotificationType
from timApp.timdb.models.user import User
from timApp.timdb.tim_models import db

FUNNEL_HOST = "funnel"
FUNNEL_PORT = 80

notify = Blueprint('notify',
                   __name__,
                   url_prefix='')

sent_mails_in_testing = []


@notify.route('/notify/<int:doc_id>', methods=['GET'])
def get_notify_settings(doc_id):
    verify_logged_in()
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    return json_response(
        get_current_user_object().get_notify_settings(d))


@notify.route('/notify/<int:doc_id>', methods=['POST'])
def set_notify_settings(doc_id):
    verify_logged_in()
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    comment_modify, comment_add, doc_modify = verify_json_params('email_comment_modify', 'email_comment_add',
                                                                 'email_doc_modify')
    get_current_user_object().set_notify_settings(d,
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
                log_error(f'Response from funnel: {response.status} {response.reason}')

        except (ConnectionError, socket.error, http.client.error) as e:
            log_error(f"Couldn't connect to funnel: {e} - {msg}")

        finally:
            if conn is not None:
                conn.close()


def notify_doc_watchers(doc: DocInfo,
                        content_msg: str,
                        notify_type: NotificationType,
                        par: Optional[DocParagraph] = None):
    me = get_current_user_object()
    if notify_type == NotificationType.DocModified:
        subject = f'Someone edited the document {doc.title}'
        subject_full = f'{me.pretty_full_name} edited the document {doc.title}'
        group_subject = f'The document {doc.title} has been modified'
        group_id = f'docmodify_{doc.id}'
    elif notify_type == NotificationType.CommentAdded:
        subject = f'Someone posted a comment to the document {doc.title}'
        subject_full = f'{me.pretty_full_name} posted a comment to the document {doc.title}'
        group_subject = f'The document {doc.title} has new notes'
        group_id = f'notes_{doc.id}'
    elif notify_type == NotificationType.CommentModified:
        subject = f'Someone edited a comment in the document {doc.title}'
        subject_full = f'{me.pretty_full_name} edited a comment in the document {doc.title}'
        group_subject = f'The document {doc.title} has new notes'
        group_id = f'notes_{doc.id}'
    else:
        assert False, 'Unknown NotificationType'
    msg = f'Link to the {"paragraph" if par else "document"}: {doc.url}{"#" + par.get_id() if par else ""}'

    full_msg = msg + '\n\n' + content_msg

    for note in doc.get_notifications(notify_type):
        user: User = note.user
        if user.id == me.id or not user.email or not user.has_view_access(doc):
            continue

        # If a document was modified and the user doesn't have edit access to it, we must not send the source md
        send_full_msg = notify_type != NotificationType.DocModified or user.has_edit_access(doc)

        # Poster identity should be hidden unless the user has teacher access to the document
        send_full_subject = user.has_teacher_access(doc)
        reply_to = me.email if user.has_teacher_access(doc) else None
        send_email(user.email,
                   subject_full if send_full_subject else subject,
                   full_msg if send_full_msg else msg,
                   mail_from='tim@jyu.fi',
                   reply_to=reply_to,
                   group_id=group_id, group_subject=group_subject)
