import logging
import smtplib

from email.mime.text import MIMEText
from fileutils import *
from grouping_queue import GroupingQueue
from persistent_queue import PersistentQueue, PersistenceException
from ratelimiter import RateLimited
from typing import Dict, Optional, Tuple, Union

MAIL_HOST = "smtp.jyu.fi"
MAIL_DIR = "/service/mail"


CLIENT_RATE        = 20  # Max messages per client rate window
CLIENT_RATE_WINDOW = 60  # In seconds
GROUP_DELAY        = 120 # In seconds, the delay to combine similar messages into one


def group_messages(msg_a: Dict, msg_b: Dict):
    if msg_a['Rcpt-To'] != msg_b['Rcpt-To']:
        return None

    return {
        'From': 'tim.jyu.fi',
        'Rcpt-To': msg_a['Rcpt-To'],
        'Subject': msg_a.get('Group-Subject', 'Various notifications'),
        'Body': msg_a['Body'] + '\n\n' + msg_b['Body']
    }


class Mailer:
    def __init__(self,
                 mail_host: str = MAIL_HOST,
                 mail_dir: str = MAIL_DIR,
                 client_rate: int = CLIENT_RATE,
                 client_rate_window: int = CLIENT_RATE_WINDOW,
                 group_delay: int = GROUP_DELAY,
                 dry_run: bool = False):

        logging.getLogger('mailer').info('Mailer plugin started, host is {}'.format(mail_host))
        logging.getLogger('mailer').info('Dry run mode is {}'.format(dry_run))
        self.mail_host = mail_host
        self.mail_dir = mail_dir
        self.queue = PersistentQueue(self.mail_dir)
        self.group_queues = {group_id: self.get_group_queue(group_id, use_cache=False) for group_id in get_subdirs(mail_dir)}
        self.group_delay = group_delay
        self.dry_run = dry_run

        self.dequeue = RateLimited(self.force_dequeue, client_rate_window, client_rate)

    def get_first_filename(self) -> str:
        return self.queue.get_first_filename()

    def get_last_filename(self) -> str:
        return self.queue.get_last_filename()

    def get_group_queue(self, group_id: str, use_cache: bool = True) -> PersistentQueue:
        if not use_cache or group_id not in self.group_queues:
            abs_path = os.path.join(self.mail_dir, group_id)
            group_queue = GroupingQueue(abs_path, grouping_function=group_messages)
            group_queue.limited_dequeue = RateLimited(group_queue.dequeue, self.group_delay, 1)

            if use_cache:
                self.group_queues[group_id] = group_queue
        else:
            group_queue = self.group_queues[group_id]

        return group_queue

    def enqueue(self, headers: dict, msg: str) -> Optional[str]:
        group_id = headers.get('Group-Id')
        group_name = 'master' if group_id is None else group_id
        logging.getLogger('mailer').debug('Enqueuing message: {} in {} queue'.format(headers, group_name))

        e = headers.copy()
        e['Body'] = msg

        try:
            if group_id is None:
                this_relfile = self.queue.enqueue(e)
            else:
                this_relfile = self.get_group_queue(group_id).enqueue(e)

            logging.getLogger('mailer').debug('Enqueuing succeeded')
            return this_relfile

        except PersistenceException as e:
            logging.getLogger('mailer').error('Enqueuing failed: ' + str(e))
            return None

    def has_messages(self):
        for queue_name in self.group_queues:
            queue = self.group_queues[queue_name]
            if not queue.is_empty():
                return False

        return not self.queue.is_empty()

    def force_dequeue(self) -> Optional[Dict[str, str]]:
        logging.getLogger('mailer').debug('Dequeuing message')
        msg = None

        for queue_name in self.group_queues:
            queue = self.group_queues[queue_name]
            msg = queue.limited_dequeue()
            if msg is not None:
                logging.getLogger('mailer').debug('Dequeuing from {} queue'.format(queue_name))
                break

        if msg is None:
            logging.getLogger('mailer').debug('Dequeuing from master queue')
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
        self.dequeue.update(dt)

        while self.has_messages():
            msg = self.dequeue()
            if msg is None:
                return

            self.send_message(msg)
