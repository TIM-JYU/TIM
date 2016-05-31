import logging
import smtplib

from email.mime.text import MIMEText
from persistent_queue import PersistentQueue, PersistenceException
from typing import Dict, Optional, Tuple, Union

MAIL_HOST = "smtp.jyu.fi"
MAIL_DIR = "/service/mail"


CLIENT_RATE        = 20  # Max messages per client rate window
CLIENT_RATE_WINDOW = 60  # In seconds


class Mailer:
    def __init__(self,
                 mail_host: str = MAIL_HOST,
                 mail_dir: str = MAIL_DIR,
                 client_rate: int = CLIENT_RATE,
                 client_rate_window: int = CLIENT_RATE_WINDOW,
                 dry_run: bool = False):

        logging.getLogger('mailer').info('Mailer plugin started, host is {}'.format(mail_host))
        logging.getLogger('mailer').info('Dry run mode is {}'.format(dry_run))
        self.mail_host = mail_host
        self.mail_dir = mail_dir
        self.queue = PersistentQueue(self.mail_dir)

        self.client_rate = client_rate
        self.client_rate_window = client_rate_window
        self.dry_run = dry_run

        self.messages_remaining = self.client_rate
        self.window_age = 0

    def get_first_filename(self) -> str:
        return self.queue.get_first_filename()

    def get_last_filename(self) -> str:
        return self.queue.get_last_filename()

    def get_random_filenames(self) -> Tuple[str, str]:
        return self.queue.get_random_filenames()

    def set_next(self, filename: str, next_filename: str):
        try:
            self.queue.set_next(filename, next_filename)
        except PersistenceException as e:
            logging.getLogger('mailer').error(str(e))

    def enqueue(self, headers: dict, msg: str) -> str:
        logging.getLogger('mailer').debug('Enqueuing message: {}'.format(headers))
        e = headers.copy()
        e['Body'] = msg
        this_relfile = self.queue.enqueue(e)
        logging.getLogger('mailer').debug('Enqueuing succeeded')
        return this_relfile

    def has_messages(self):
        return not self.queue.is_empty()

    def dequeue(self) -> Optional[Dict[str, str]]:
        logging.getLogger('mailer').debug('Dequeuing message')
        msg = self.queue.dequeue()
        logging.getLogger('mailer').debug('Dequeuing succeeded: {}'.format(msg))
        return msg

    def send_message(self, msg: Union[dict, None]):
        if msg is None:
            logging.getLogger('mailer').warning("Null argument to send_message")
            return

        logging.getLogger('mailer').info("Mail to {}: {}".format(msg['Rcpt-To'], msg['Body']))
        if self.dry_run:
            logging.getLogger('mailer').info("Dry run mode specified, not sending")
            return

        mime_msg = MIMEText(msg['Body'])
        mime_msg['Subject'] = msg['Subject']
        mime_msg['From'] = msg['From']
        mime_msg['To'] = msg['Rcpt-To']

        if msg.get('Reply-To', None):
            mime_msg.add_header('Reply-To', msg['Reply-To'])

        s = smtplib.SMTP(self.mail_host)
        s.sendmail(msg['From'], [msg['Rcpt-To']], mime_msg.as_string())
        s.quit()
        logging.getLogger('mailer').info("Message sent")

    def update(self, dt: float):
        self.window_age += dt
        if self.window_age > self.client_rate_window:
            self.messages_remaining = self.client_rate
            while self.window_age >= self.client_rate_window:
                self.window_age -= self.client_rate_window
            logging.getLogger('mailer').info("A new message window is opened")

        while self.has_messages():
            if self.messages_remaining < 1:
                logging.getLogger('mailer').info("{} messages in queue, waiting for a new send window".format(
                    self.messages_remaining))
                return

            self.send_message(self.dequeue())
            self.messages_remaining -= 1

