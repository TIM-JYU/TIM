import json
import logging
import os
import random
import smtplib
import string
import time

from email.mime.text import MIMEText
from typing import Union

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
        if not os.path.exists(mail_dir):
            os.mkdir(mail_dir)

        self.client_rate = client_rate
        self.client_rate_window = client_rate_window
        self.dry_run = dry_run

        self.first_message_time = None
        self.messages_remaining = client_rate

    def get_first_filename(self) -> str:
        return os.path.join(self.mail_dir, 'first')

    def get_last_filename(self) -> str:
        return os.path.join(self.mail_dir, 'last')

    def get_random_filenames(self) -> str:
        while True:
            without_path = ''.join([random.choice(string.ascii_letters) for _ in range(16)])
            with_path = os.path.join(self.mail_dir, without_path)
            if not os.path.isfile(with_path):
                return with_path, without_path

    def set_next(self, filename: str, next_filename: str):
        try:
            with open(filename, 'r') as f_src:
                msg = json.loads(f_src.read())
        except Exception as e:
            logging.getLogger('mailer').error('Error reading {}: {}'.format(filename, e))
            return

        msg['Next-Msg'] = next_filename

        try:
            with open(filename, 'w') as f_dest:
                f_dest.write(json.dumps(msg))
        except Exception as e:
            logging.getLogger('mailer').error('Error writing {}: {}'.format(filename, e))
            return

    def enqueue(self, headers: dict, msg: str) -> str:
        logging.getLogger('mailer').debug('Enqueuing message: {}'.format(headers))
        this_absfile, this_relfile = self.get_random_filenames()
        with open(this_absfile, 'w') as f:
            fullmsg = headers.copy()
            fullmsg['Body'] = msg
            fullmsg['Next-Msg'] = ''
            f.write(json.dumps(fullmsg))

        first_file = self.get_first_filename()
        last_file = self.get_last_filename()

        if not os.path.islink(first_file):
            files = os.listdir(self.mail_dir)
            if len(files) > 1:
                os.symlink(files[0], first_file)
            else:
                os.symlink(this_relfile, first_file)

        if os.path.islink(last_file):
            self.set_next(last_file, this_relfile)
            os.unlink(last_file)

        os.symlink(this_relfile, last_file)
        logging.getLogger('mailer').debug('Enqueuing succeeded')
        return this_relfile

    def has_messages(self):
        return os.path.islink(self.get_first_filename())

    def dequeue(self) -> Union[dict, None]:
        logging.getLogger('mailer').debug('Dequeuing message')
        first_file = self.get_first_filename()
        if not os.path.isfile(first_file):
            logging.getLogger().warning('Called dequeue with no messages in the queue')
            return None

        try:
            with open(first_file, 'r') as f_src:
                msg = json.loads(f_src.read())
        except Exception as e:
            logging.getLogger().error('Error parsing file {}: {}'.format(first_file, str(e)))
            # TODO: proper error handling... move the file to an error directory
            return None

        os.unlink(os.path.join(self.mail_dir, os.readlink(first_file)))
        os.unlink(first_file)
        if msg['Next-Msg'] == '':
            os.unlink(self.get_last_filename())
        else:
            os.symlink(msg['Next-Msg'], first_file)

        logging.getLogger('mailer').debug('Dequeuing succeeded: {}'.format(msg))
        return {'From': msg['From'], 'Rcpt-To': msg['Rcpt-To'], 'Subject': msg['Subject'], 'Body': msg['Body']}

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

        s = smtplib.SMTP(self.mail_host)
        s.sendmail(msg['From'], [msg['Rcpt-To']], mime_msg.as_string())
        s.quit()
        logging.getLogger('mailer').info("Message sent")

    def update(self):
        if self.first_message_time is not None:
            window_age = time.time() - self.first_message_time
            if window_age > self.client_rate_window:
                # New window
                self.first_message_time = None
                self.messages_remaining = self.client_rate
                logging.getLogger('mailer').info("A new message window is opened")

        if not self.has_messages():
            return

        if self.messages_remaining < 1:
            logging.getLogger('mailer').info("{} messages in queue, waiting for a new send window".format(
                self.messages_remaining))
            return

        self.send_message(self.dequeue())
        self.messages_remaining -= 1
        if self.first_message_time is None:
            self.first_message_time = time.time()

