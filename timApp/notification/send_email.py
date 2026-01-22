import smtplib
from email.mime.text import MIMEText
from email.utils import formatdate
from threading import Thread
from typing import Any

from flask import Flask

from timApp.tim_app import app
from timApp.util.flask.requesthelper import is_testing, is_localhost
from timApp.util.logger import log_error

sent_mails_in_testing: list[dict[str, Any]] = []


def send_email(
    rcpt: str,
    subject: str,
    msg: str,
    mail_from: str = app.config["MAIL_FROM"],
    reply_to: str = app.config["NOREPLY_EMAIL"],
) -> Thread | None:
    if is_testing():
        sent_mails_in_testing.append(locals())
        return None

    if is_localhost():
        # don't use log_* function because this is typically run in Celery
        print(f"Skipping mail send on localhost, rcpt: {rcpt}, message: {msg}")
        return None

    t = Thread(
        target=send_email_impl, args=(app, rcpt, subject, msg, mail_from, reply_to)
    )
    t.start()
    return t


def send_email_impl(
    flask_app: Flask,
    rcpt: str,
    subject: str,
    msg: str,
    mail_from: str = app.config["MAIL_FROM"],
    reply_to: str = app.config["NOREPLY_EMAIL"],
) -> None:
    # FIXME: Temp for VK26
    mail_from = app.config["MAIL_FROM"]

    with flask_app.app_context():
        mime_msg = MIMEText(msg + flask_app.config["MAIL_SIGNATURE"])
        mime_msg["Subject"] = subject
        mime_msg["From"] = mail_from
        mime_msg["To"] = rcpt
        mime_msg["Date"] = formatdate(localtime=True)

        if reply_to:
            mime_msg.add_header("Reply-To", reply_to)

        s = smtplib.SMTP_SSL(
            flask_app.config["MAIL_HOST"], flask_app.config["MAIL_PORT"]
        )
        try:
            s.login(mail_from, app.config["MAIL_PASSWORD"])
            s.sendmail(mail_from, [rcpt], mime_msg.as_string())
        except (
            smtplib.SMTPSenderRefused,
            smtplib.SMTPRecipientsRefused,
            smtplib.SMTPHeloError,
            smtplib.SMTPDataError,
            smtplib.SMTPNotSupportedError,
            smtplib.SMTPAuthenticationError,
            smtplib.SMTPException,
        ) as e:
            log_error(str(e))
        else:
            pass
        finally:
            s.quit()


def multi_send_email(
    rcpt: str,
    subject: str,
    msg: str,
    mail_from: str = app.config["MAIL_FROM"],
    reply_to: str = app.config["NOREPLY_EMAIL"],
    bcc: str = "",
    reply_all: bool = False,
    with_signature: bool = False,
) -> None:
    if is_testing():
        sent_mails_in_testing.append(locals())
        return

    Thread(
        target=multi_send_email_impl,
        args=(
            app,
            rcpt,
            subject,
            msg,
            mail_from,
            reply_to,
            bcc,
            reply_all,
            with_signature,
        ),
    ).start()


def multi_send_email_impl(
    flask_app: Flask,
    rcpt: str,
    subject: str,
    msg: str,
    mail_from: str = app.config["MAIL_FROM"],
    reply_to: str = app.config["NOREPLY_EMAIL"],
    bcc: str = "",
    reply_all: bool = False,
    with_signature: bool = False,
) -> None:
    # FIXME: Temp for VK26
    mail_from = app.config["MAIL_FROM"]

    with flask_app.app_context():
        s = (
            smtplib.SMTP_SSL(
                flask_app.config["MAIL_HOST"], flask_app.config["MAIL_PORT"]
            )
            if not is_localhost()
            else None
        )
        rcpts = rcpt.split(";")
        mail_targets: list[str | list[str]] = list(rcpts) if not reply_all else [rcpts]
        bccmail = bcc
        extra = ""
        if bcc:
            if len(rcpts) > 3:
                mail_targets.append(bcc)
                bccmail = ""
                extra = "\n\n" + "\n".join(rcpts)
        try:
            if not s:
                s.login(mail_from, app.config["MAIL_PASSWORD"])
            for rcp in mail_targets:
                try:
                    # TODO: Mailmerge here possible templates.
                    send_extra = ""
                    send_to = rcp if isinstance(rcp, list) else [rcp]
                    send_to = [m for m in send_to if m]
                    send_to_str = ",".join(send_to)
                    if bccmail:
                        send_to.append(bccmail)
                    if rcp == bcc:
                        send_extra = extra
                    mime_msg = MIMEText(
                        msg
                        + send_extra
                        + (flask_app.config["MAIL_SIGNATURE"] if with_signature else "")
                    )

                    mime_msg["Subject"] = subject
                    mime_msg["From"] = mail_from
                    mime_msg["Bcc"] = bccmail
                    mime_msg["To"] = send_to_str
                    mime_msg["Date"] = formatdate(localtime=True)
                    if reply_to:
                        mime_msg.add_header("Reply-To", reply_to)

                    if not s:
                        # don't use log_* function because this is typically run in Celery
                        print(
                            f"Dry run send mail, from: {mail_from}, send_to: {send_to},  message: {mime_msg.as_string()}"
                        )
                    else:
                        s.sendmail(mail_from, send_to, mime_msg.as_string())
                except (
                    smtplib.SMTPSenderRefused,
                    smtplib.SMTPRecipientsRefused,
                    smtplib.SMTPHeloError,
                    smtplib.SMTPDataError,
                    smtplib.SMTPNotSupportedError,
                ) as e:
                    log_error(str(e))
                else:
                    pass
        finally:
            if s:
                s.quit()
