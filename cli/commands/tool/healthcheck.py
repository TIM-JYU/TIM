import datetime
import smtplib
import textwrap
import time
import urllib.request
from argparse import ArgumentParser
from email.mime.text import MIMEText

from cli.util.logging import log_info, log_error

info = {
    "help": "Start a healthcheck watchdog",
    "description": """
Start a healthcheck watchdog for TIM instance.

The watchdog periodically checks for TIM's health as follows:

(1) Every 1 minute, it checks if the TIM instance is running by pinging the /ping route.
(2) If the ping fails, the watchdog will send an email message and ping the TIM instance again
    in {interval + 1} minutes until it succeeds.
""",
}


class Arguments:
    tim_url: str
    from_email: str
    to_email: str
    smtp_server: str
    ensure_once: bool


def _now() -> str:
    return datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def handle_error(msg: str, current_interval: int, args: Arguments) -> int:
    s = None
    try:
        log_error(
            f"({_now()}) Could not ping TIM instance: '{msg}'; recheck in {current_interval} minute(s)"
        )
        mail_msg = MIMEText(
            textwrap.dedent(
                f"""
                TIM ({args.tim_url}) is down. Pinged URL {args.tim_url}/ping, {msg}.
                
                Checking again in {current_interval} minute(s).
                """
            )
        )
        mail_msg["Subject"] = f"TIM ({args.tim_url}) is down"
        mail_msg["From"] = args.from_email
        mail_msg["To"] = args.to_email

        s = smtplib.SMTP(args.smtp_server)
        s.sendmail(args.from_email, [args.to_email], mail_msg.as_string())
    except Exception as e:
        log_error(f"Failed to send email because: {e}")
    finally:
        if s is not None:
            s.quit()
    return current_interval + 1


LOCK_FD = -1


def run(args: Arguments) -> None:
    if args.ensure_once:
        global LOCK_FD
        import fcntl
        import errno
        import os

        try:
            LOCK_FD = os.open("/tmp/tim_healthcheck.lock", os.O_RDWR | os.O_CREAT)
            fcntl.flock(LOCK_FD, fcntl.LOCK_EX | fcntl.LOCK_NB)  # type: ignore
        except (IOError, OSError) as e:
            if e.errno in (errno.EACCES, errno.EAGAIN):
                log_info("Healthcheck watchdog is already running")
                return
        # No need to close the lock, it should be released when the process exits

    log_info("Starting healthcheck watchdog")
    try:
        interval = 1
        next_interval = interval
        while True:
            try:
                ping_url = urllib.request.urlopen(f"{args.tim_url}/ping")
                if ping_url.getcode() == 200:
                    interval = 1
                    next_interval = interval
                    log_info(
                        f"({_now()}) TIM instance is running; checking again in {interval} minute"
                    )
                else:
                    next_interval = handle_error(
                        f"got status code {ping_url.getcode()} (expected 200)",
                        interval,
                        args,
                    )
            except Exception as e:
                next_interval = handle_error(f"got error: {e}", interval, args)
            time.sleep(interval * 60)
            interval = next_interval
    except KeyboardInterrupt:
        log_info("Healthcheck watchdog stopped")


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--url",
        help="TIM instance URL",
        required=True,
        dest="tim_url",
    )
    parser.add_argument(
        "--from-email",
        help="The email address to send the healthcheck email from",
        required=True,
        dest="from_email",
    )
    parser.add_argument(
        "--to-email",
        help="The email address to send the healthcheck email to",
        required=True,
        dest="to_email",
    )
    parser.add_argument(
        "--smtp-server",
        help="The SMTP server to use for sending the healthcheck email",
        required=True,
        dest="smtp_server",
    )
    parser.add_argument(
        "--ensure-once",
        help="Exit if the watchdog is already running",
        action="store_true",
        default=False,
        dest="ensure_once",
    )
