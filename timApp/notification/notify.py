import smtplib
import urllib.parse
from collections import defaultdict
from email.mime.text import MIMEText
from threading import Thread
from typing import Optional, List, DefaultDict, Set

from flask import Blueprint, current_app, Flask
from sqlalchemy.orm import joinedload

from timApp.auth.accesshelper import verify_logged_in, verify_view_access, get_item_or_abort
from timApp.auth.sessioninfo import get_current_user_object, get_current_user_id
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.version import Version, ver_to_str
from timApp.item.block import Block
from timApp.notification.notification import NotificationType, Notification
from timApp.notification.pending_notification import DocumentNotification, CommentNotification, \
    PendingNotification, get_pending_notifications, GroupingKey
from timApp.tim_app import app
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import verify_json_params, is_testing, is_localhost
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.logger import log_error
from timApp.util.utils import get_current_time, seq_to_str

notify = Blueprint('notify',
                   __name__,
                   url_prefix='/notify')

sent_mails_in_testing = []


@notify.route('/<int:doc_id>', methods=['GET'])
def get_notify_settings(doc_id):
    verify_logged_in()
    i = get_item_or_abort(doc_id)
    verify_view_access(i)
    return json_response(
        get_current_user_object().get_notify_settings(i))


@notify.route('/<int:doc_id>', methods=['POST'])
def set_notify_settings(doc_id):
    verify_logged_in()
    i = get_item_or_abort(doc_id)
    verify_view_access(i)
    comment_modify, comment_add, doc_modify = verify_json_params('email_comment_modify', 'email_comment_add',
                                                                 'email_doc_modify')
    get_current_user_object().set_notify_settings(i,
                                                  comment_modify=comment_modify,
                                                  comment_add=comment_add,
                                                  doc_modify=doc_modify)
    db.session.commit()
    return ok_response()


@notify.route('/all')
def get_user_notify_settings():
    verify_logged_in()
    nots = get_current_user_notifications()
    return json_response(nots)


def get_current_user_notifications(limit: Optional[int] = None):
    q = (Notification.query.filter_by(user_id=get_current_user_id()).options(
        joinedload(Notification.block)
            .joinedload(Block.docentries)
    ).options(
        joinedload(Notification.block)
            .joinedload(Block.folder)
    ).options(
        joinedload(Notification.block)
            .joinedload(Block.translation)
    )).order_by(Notification.doc_id.desc())
    if limit is not None:
        q = q.limit(limit)
    nots = q.all()
    return nots


def send_email(
        rcpt: str,
        subject: str,
        msg: str,
        mail_from: str = app.config['HELP_EMAIL'],
        reply_to: str = app.config['NOREPLY_EMAIL'],
) -> Optional[Thread]:
    if is_testing():
        sent_mails_in_testing.append(locals())
        return None

    if is_localhost():
        # don't use log_* function because this is typically run in Celery
        print(f'Skipping mail send on localhost, rcpt: {rcpt}, message: {msg}')
        return None

    return Thread(target=send_email_impl, args=(app, rcpt, subject, msg, mail_from, reply_to)).start()


def send_email_impl(
        flask_app: Flask,
        rcpt: str,
        subject: str,
        msg: str,
        mail_from: str = app.config['HELP_EMAIL'],
        reply_to: str = app.config['NOREPLY_EMAIL'],
):
    with flask_app.app_context():
        mime_msg = MIMEText(msg + flask_app.config['MAIL_SIGNATURE'])
        mime_msg['Subject'] = subject
        mime_msg['From'] = mail_from
        mime_msg['To'] = rcpt

        if reply_to:
            mime_msg.add_header('Reply-To', reply_to)

        s = smtplib.SMTP(flask_app.config['MAIL_HOST'])
        try:
            s.sendmail(mail_from, [rcpt], mime_msg.as_string())
        except (smtplib.SMTPSenderRefused,
                smtplib.SMTPRecipientsRefused,
                smtplib.SMTPHeloError,
                smtplib.SMTPDataError,
                smtplib.SMTPNotSupportedError) as e:
            log_error(str(e))
        else:
            pass
        finally:
            s.quit()

def multi_send_email(
        rcpt: str,
        subject: str,
        msg: str,
        mail_from: str = app.config['HELP_EMAIL'],
        reply_to: str = app.config['NOREPLY_EMAIL'],
        bcc: str = ''
) -> Optional[Thread]:
    if is_testing():
        sent_mails_in_testing.append(locals())
        return None

    if is_localhost():
        # don't use log_* function because this is typically run in Celery
        print(f'Skipping mail send on localhost, rcpt: {rcpt}, message: {msg}')
        return None

    return Thread(target=multi_send_email_impl, args=(app, rcpt, subject, msg, mail_from, reply_to, bcc)).start()


def multi_send_email_impl(
        flask_app: Flask,
        rcpt: str,
        subject: str,
        msg: str,
        mail_from: str = app.config['HELP_EMAIL'],
        reply_to: str = app.config['NOREPLY_EMAIL'],
        bcc: str = ''
):
    with flask_app.app_context():
        s = smtplib.SMTP(flask_app.config['MAIL_HOST'])
        rcpts = rcpt.split(";")
        bccmail = bcc
        extra = ''
        if bcc:
            if len(rcpts) > 3:
                rcpts.append(bcc)
                bccmail = ''
                extra = "\n\n" + "\n".join(rcpts)
        try:
            for rcp in rcpts:
                try:
                    # TODO: Mailmerge here possible templates.
                    mime_msg = MIMEText(msg)  # + flask_app.config['MAIL_SIGNATURE'])
                    mime_msg['Subject'] = subject
                    mime_msg['From'] = mail_from
                    mime_msg['Bcc'] = bccmail
                    send_extra = ''
                    if rcp == bcc:
                        send_extra = extra
                    if reply_to:
                        mime_msg.add_header('Reply-To', reply_to)
                    mime_msg['To'] = rcp
                    s.sendmail(mail_from, [rcp, bccmail], mime_msg.as_string() + send_extra)
                except (smtplib.SMTPSenderRefused,
                        smtplib.SMTPRecipientsRefused,
                        smtplib.SMTPHeloError,
                        smtplib.SMTPDataError,
                        smtplib.SMTPNotSupportedError) as e:
                    log_error(str(e))
                else:
                    pass
        finally:
            s.quit()


def notify_doc_watchers(doc: DocInfo, content_msg: str, notify_type: NotificationType,
                        par: Optional[DocParagraph] = None, old_version: Version = None):
    me = get_current_user_object()
    new_version = doc.document.get_version()
    if notify_type.is_document_modification:
        p = DocumentNotification(
            user=me,
            doc_id=doc.id,
            par_id=par.get_id() if par else None,
            text=content_msg,
            version_change=f'{ver_to_str(old_version)}/{ver_to_str(new_version)}',
            kind=notify_type,
        )
    else:
        p = CommentNotification(
            user=me,
            doc_id=doc.id,
            par_id=par.get_id() if par else None,
            text=content_msg,
            kind=notify_type,
        )
    db.session.add(p)


def get_name_string(users: List[User], show_names: bool):
    num_users = len(users)
    if show_names:
        return seq_to_str(list(u.pretty_full_name for u in users))
    else:
        return get_user_count_str(num_users)


MIXED_DOC_MODIFY = 'doc_modify'
MIXED_COMMENT = 'comment'


def get_diff_link(docentry: DocInfo, ver_before: Version, ver_after: Version):
    return f"""{current_app.config['TIM_HOST']}/diff/{docentry.id}/{ver_before[0]}/{ver_before[1]}/{ver_after[0]}/{ver_after[1]}"""


def get_message_for(ps: List[PendingNotification], d: DocInfo, show_text: bool, show_names: bool):
    msg = ''
    num_chgs = len(ps)
    if ps[0].notify_type.is_document_modification and num_chgs > 1 and show_text:
        first = ps[0]
        last = ps[-1]
        assert isinstance(first, DocumentNotification), 'Expected a DocumentNotification'
        assert isinstance(last, DocumentNotification), 'Expected a DocumentNotification'
        first_ver = first.version_before
        last_ver = last.version_after
        msg += f'Link to all changes: {get_diff_link(d, first_ver, last_ver)}\n\n' \
               f'The individual changes ({num_chgs}) are listed below.\n\n'

    for p in ps:
        name_str = get_name_string([p.user], show_names=show_names)
        par = p.par_id
        t = p.notify_type
        if t == NotificationType.DocModified:
            s = f'Document modified'
        elif t == NotificationType.ParAdded:
            s = f'Paragraph added'
        elif t == NotificationType.ParModified:
            s = f'Paragraph modified'
        elif t == NotificationType.ParDeleted:
            s = f'Paragraph deleted'
        elif t == NotificationType.CommentAdded:
            s = f'Comment posted'
        elif t == NotificationType.CommentModified:
            s = f'Comment modified'
        elif t == NotificationType.CommentDeleted:
            s = f'Comment deleted'
        else:
            assert False, 'Unknown NotificationType'
        url = f'{d.url}{"#" + par if par else ""}'
        if show_names:
            s += f' by {name_str}'
            d.document.insert_preamble_pars()
            if par and not p.notify_type.is_document_modification:
                try:
                    pobj = d.document.get_paragraph(par)
                except TimDbException:
                    pass
                else:
                    if pobj.is_task():
                        task_id = pobj.get_attr('taskId')
                        params = urllib.parse.urlencode({'task': task_id, 'user': p.user.name})
                        url = f'{d.get_url_for_view("answers")}?{params}'

        msg += f'{s}: {url}'

        if show_text and p.notify_type.is_document_modification:
            assert isinstance(p, DocumentNotification)
            v1 = p.version_before
            v2 = p.version_after
            msg += f' (changes: {get_diff_link(d, v1, v2)} )'
        msg += '\n\n'
        if show_text or not p.notify_type.is_document_modification:
            msg += p.text + '\n\n'
    return msg.strip()


def get_subject_for(ps: List[PendingNotification], d: DocInfo, show_names: bool):
    num_mods = len(ps)
    distinct_users = list(set(p.user for p in ps))
    type_of_all = get_type_of_notify(ps)
    name_str = get_name_string(distinct_users, show_names)
    if type_of_all == NotificationType.DocModified or type_of_all == MIXED_DOC_MODIFY:
        return f'{name_str} edited the document {d.title} {get_edit_count_str(num_mods)}'
    elif type_of_all == NotificationType.ParAdded:
        return f'{name_str} added {get_par_count_str(num_mods)} to the document {d.title}'
    elif type_of_all == NotificationType.ParModified:
        return f'{name_str} modified {get_par_count_str(num_mods)} in the document {d.title}'
    elif type_of_all == NotificationType.ParDeleted:
        return f'{name_str} deleted {get_par_count_str(num_mods)} from the document {d.title}'
    elif type_of_all == NotificationType.CommentAdded:
        return f'{name_str} posted {get_comment_count_str(num_mods)} to the document {d.title}'
    elif type_of_all == NotificationType.CommentModified:
        return f'{name_str} modified {get_comment_count_str(num_mods)} in the document {d.title}'
    elif type_of_all == NotificationType.CommentDeleted:
        return f'{name_str} deleted {get_comment_count_str(num_mods)} from the document {d.title}'
    elif type_of_all == MIXED_COMMENT:
        return f'{name_str} posted/modified/deleted {get_comment_count_str(num_mods)} in the document {d.title}'


def get_type_of_notify(ps):
    for n in NotificationType:
        if all(p.notify_type == n for p in ps):
            return n
    if all(p.notify_type.is_document_modification for p in ps):
        return MIXED_DOC_MODIFY
    elif all(not p.notify_type.is_document_modification for p in ps):
        return MIXED_COMMENT
    else:
        assert False, 'There should not be mixed comment and doc modification notification types in a batch'


def get_edit_count_str(num_edits):
    if num_edits > 1:
        return f'{num_edits} times'
    else:
        return ''


def get_user_count_str(num_users):
    if num_users > 1:
        return f'{num_users} people'
    else:
        return 'Someone'


def get_par_count_str(num_mods):
    if num_mods > 1:
        return f'{num_mods} paragraphs'
    else:
        return 'a paragraph'


def get_comment_count_str(num_mods):
    if num_mods > 1:
        return f'{num_mods} comments'
    else:
        return 'a comment'


def process_pending_notifications():
    pns = get_pending_notifications()
    grouped_pns: DefaultDict[GroupingKey, List[PendingNotification]] = defaultdict(list)
    email_threads: List[Thread] = []
    for p in pns:
        grouped_pns[p.grouping_key].append(p)
    for (doc_id, t), ps in grouped_pns.items():
        doc = DocEntry.find_by_id(doc_id)
        # Combine ps to a single mail (tailored for each subscriber) and send it
        if t == 'd':
            assert all(isinstance(p, DocumentNotification) for p in
                       ps), 'Expected all notifications of type DocumentNotification'
            condition = Notification.email_doc_modify == True
        elif t == 'c':
            assert all(isinstance(p, CommentNotification) for p in
                       ps), 'Expected all notifications of type CommentNotification'
            condition = (Notification.email_comment_add == True) | (Notification.email_comment_modify == True)
        else:
            assert False, 'Unknown notify type'
        users_to_notify: Set[User] = set(n.user for n in doc.get_notifications(condition))
        for user in users_to_notify:
            if not user.email or not user.has_view_access(doc) or user.get_prefs().is_item_excluded_from_emails(doc):
                continue

            # don't send emails about own actions
            ps_to_consider = [p for p in ps if p.user != user]

            # TODO Currently email_comment_add and email_comment_modify are basically the same option.
            # Should do additional filtering here.

            if not ps_to_consider:
                continue

            # Poster identity should be hidden unless the user has teacher access to the document
            show_names = user.has_teacher_access(doc)
            subject = get_subject_for(ps_to_consider, doc, show_names=show_names)
            # If a document was modified and the user doesn't have edit access to it, we must not send the source md
            msg = get_message_for(
                ps_to_consider,
                doc,
                show_text=user.has_edit_access(doc) or not ps_to_consider[0].notify_type.is_document_modification,
                show_names=show_names
            )

            is_unique_user = len(set(p.user for p in ps_to_consider)) == 1
            reply_to = ps_to_consider[0].user.email if show_names and is_unique_user else None
            result = send_email(
                user.email,
                subject,
                msg,
                mail_from=app.config['NOREPLY_EMAIL'],
                reply_to=reply_to,
            )
            if result:
                email_threads.append(result)
        for p in ps:
            p.processed = get_current_time()
    for t in email_threads:
        t.join()
    db.session.commit()
