import smtplib
from email.mime.text import MIMEText
from threading import Thread

from flask import Flask
from typing import Optional

from timApp.tim_app import app
from timApp.util.flask.requesthelper import is_testing, is_localhost
from timApp.util.logger import log_error

sent_mails_in_testing = []


def send_email(
        rcpt: str,
        subject: str,
        msg: str,
        mail_from: str = app.config['MAIL_FROM'],
        reply_to: str = app.config['NOREPLY_EMAIL'],
) -> Optional[Thread]:
    if is_testing():
        sent_mails_in_testing.append(locals())
        return None

    if is_localhost():
        # don't use log_* function because this is typically run in Celery
        print(f'Skipping mail send on localhost, rcpt: {rcpt}, message: {msg}')
        return None

    t = Thread(target=send_email_impl, args=(app, rcpt, subject, msg, mail_from, reply_to))
    t.start()
    return t


def send_email_impl(
        flask_app: Flask,
        rcpt: str,
        subject: str,
        msg: str,
        mail_from: str = app.config['MAIL_FROM'],
        reply_to: str = app.config['NOREPLY_EMAIL'],
) -> None:
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
        mail_from: str = app.config['MAIL_FROM'],
        reply_to: str = app.config['NOREPLY_EMAIL'],
        bcc: str = ''
) -> None:
    if is_testing():
        sent_mails_in_testing.append(locals())
        return

    if is_localhost():
        # don't use log_* function because this is typically run in Celery
        print(f'Skipping mail send on localhost, rcpt: {rcpt}, message: {msg}')
        return

    Thread(target=multi_send_email_impl, args=(app, rcpt, subject, msg, mail_from, reply_to, bcc)).start()


def multi_send_email_impl(
        flask_app: Flask,
        rcpt: str,
        subject: str,
        msg: str,
        mail_from: str = app.config['MAIL_FROM'],
        reply_to: str = app.config['NOREPLY_EMAIL'],
        bcc: str = ''
) -> None:
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
                    send_extra = ''
                    if rcp == bcc:
                        send_extra = extra
                    mime_msg = MIMEText(msg + send_extra)  # + flask_app.config['MAIL_SIGNATURE'])
                    mime_msg['Subject'] = subject
                    mime_msg['From'] = mail_from
                    mime_msg['Bcc'] = bccmail
                    mime_msg['To'] = rcp
                    if reply_to:
                        mime_msg.add_header('Reply-To', reply_to)
                    s.sendmail(mail_from, [rcp, bccmail], mime_msg.as_string() )
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
