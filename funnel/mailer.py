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
        with open(filename, 'r') as f_src:
            lines = f_src.read().split('\n')

        if len(lines) < 4:
            logging.getLogger().error('Syntax error in file ' + filename)
            return

        lines[0] = next_filename
        with open(filename, 'w') as f_dest:
            f_dest.write('\n'.join(lines))

    def enqueue(self, sender: str, rcpt: str, subject:str, msg: str) -> str:
        this_absfile, this_relfile = self.get_random_filenames()
        with open(this_absfile, 'w') as f:
            f.write('\n'.join(['', sender, rcpt, subject, msg]))

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
        return this_relfile

    def has_messages(self):
        return os.path.islink(self.get_first_filename())

    def dequeue(self) -> Union[list, None]:
        first_file = self.get_first_filename()
        if not os.path.isfile(first_file):
            return None

        with open(first_file, 'r') as f_src:
            lines = f_src.read().split('\n')

        if len(lines) < 4:
            logging.getLogger().error('Syntax error in file ' + first_file)
            return None

        os.unlink(os.path.join(self.mail_dir, os.readlink(first_file)))
        os.unlink(first_file)
        if lines[0] == '':
            os.unlink(self.get_last_filename())
        else:
            os.symlink(lines[0], first_file)

        return {'From': lines[1], 'To': lines[2], 'Subject': lines[3], 'Msg': '\n'.join(lines[4:])}

    def send_message(self, msg: dict):
        logging.getLogger().info("Mail to {}: {}".format(msg['To'], msg['Msg']))
        if self.dry_run:
            logging.getLogger().info("Dry run mode specified, not sending")
            return

        mime_msg = MIMEText(msg['Msg'])
        mime_msg['Subject'] = msg['Subject']
        mime_msg['From'] = msg['From']
        mime_msg['To'] = msg['To']

        s = smtplib.SMTP(self.mail_host)
        s.sendmail(msg['From'], [msg['To']], mime_msg.as_string())
        s.quit()

    def update(self):
        if self.first_message_time is not None:
            window_age = time.time() - self.first_message_time
            if window_age > self.client_rate_window:
                # New window
                self.first_message_time = None
                self.messages_remaining = self.client_rate

        if not self.has_messages() or self.messages_remaining < 1:
            return

        self.send_message(self.dequeue())
        self.messages_remaining -= 1
        if self.first_message_time is None:
            self.first_message_time = time.time()
