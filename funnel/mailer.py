import logging
import smtplib

from email.mime.text import MIMEText
from fileutils import *
from funnel_thread import Updatable
from grouping_queue import GroupingQueue
from persistent_queue import PersistentQueue, PersistenceException
from ratelimiter import RateLimited
from typing import Dict, Optional, Tuple, Union

MAIL_HOST = "smtp.jyu.fi"
MAIL_DIR = "/service/mail"
MAIL_SIGNATURE = "\n\n-- This message was automatically sent by TIM"


CLIENT_RATE        = 20  # Max messages per client rate window
CLIENT_RATE_WINDOW = 60  # In seconds
GROUP_DELAY        = 600 # In seconds, the delay to combine similar messages into one


def group_messages(msg_a: Dict, msg_b: Dict):
    if msg_a['Rcpt-To'] != msg_b['Rcpt-To']:
        return None

    msg_new = msg_a.copy()
    msg_new.update({
        'From': 'tim.jyu.fi',
        'Subject': msg_a.get('Group-Subject', 'Various notifications'),
        'Body': msg_a['Body'] + '\n\n' + msg_b['Body']
    })

    return msg_new


class Mailer(Updatable):
    def __init__(self,
                 mail_host: str = MAIL_HOST,
                 mail_dir: str = MAIL_DIR,
                 mail_signature: str = MAIL_SIGNATURE,
                 client_rate: int = CLIENT_RATE,
                 client_rate_window: int = CLIENT_RATE_WINDOW,
                 group_delay: int = GROUP_DELAY,
                 dry_run: bool = False):

        logging.getLogger('mailer').info('Mailer plugin created, host is {}'.format(mail_host))
        logging.getLogger('mailer').info('Dry run mode is {}'.format(dry_run))
        self.mail_host = mail_host
        self.mail_dir = mail_dir
        self.mail_signature = MAIL_SIGNATURE

        self.queue = PersistentQueue(self.mail_dir)
        self.group_delay = group_delay
        self.dry_run = dry_run

        self.dequeue = RateLimited(self.force_dequeue, client_rate_window, client_rate)
        self.group_queues = {group_id: self.get_group_queue(group_id, use_cache=False) for group_id in
                             get_subdirs(mail_dir)}

        logging.getLogger('mailer').debug('{} group queues: {}'.format(len(self.group_queues),
                                                                       ', '.join(self.group_queues)))

    def get_first_filename(self) -> str:
        return self.queue.get_first_filename()

    def get_last_filename(self) -> str:
        return self.queue.get_last_filename()

    def get_group_queue(self, group_id: str, use_cache: bool = True) -> PersistentQueue:
        if not use_cache or group_id not in self.group_queues:
            abs_path = os.path.join(self.mail_dir, group_id)
            group_queue = GroupingQueue(abs_path, grouping_function=group_messages)
            group_queue.limited_dequeue = RateLimited(group_queue.dequeue, self.group_delay, 1)
            group_queue.limited_dequeue.calls_remaining = 0

            if use_cache:
                self.group_queues[group_id] = group_queue
        else:
            group_queue = self.group_queues[group_id]

        return group_queue

    def rm_group_queue(self, group_id: str):
        if group_id in self.group_queues:
            self.group_queues.pop(group_id).delete()

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
        return len(self.group_queues) > 0 or not self.queue.is_empty()

    def force_dequeue(self) -> Optional[Dict[str, str]]:
        msg = None
        rm_queues = []

        for queue_name in self.group_queues:
            queue = self.group_queues[queue_name]
            logging.getLogger('mailer').debug('Polling queue {}'.format(queue_name))
            msg = queue.limited_dequeue()
            if msg is not None:
                logging.getLogger('mailer').debug('Dequeuing from {} queue'.format(queue_name))
                break
            elif queue.is_empty():
                logging.getLogger('mailer').debug('Queue is empty, removing')
                rm_queues.append(queue_name)

        for queue_name in rm_queues:
            self.rm_group_queue(queue_name)

        if msg is None:
            if self.queue.is_empty():
                return None

            logging.getLogger('mailer').debug('Dequeuing from master queue')
            msg = self.queue.dequeue()

        logging.getLogger('mailer').debug('Dequeuing succeeded: {}'.format(msg))
        return msg

    def send_message(self, msg: Union[dict, None]):
        if msg is None:
            logging.getLogger('mailer').warning("Null argument to send_message")
            return

        logging.getLogger('mailer').info("Mail to {}: {}".format(msg['Rcpt-To'], msg['Body'] + self.mail_signature))
        if self.dry_run:
            logging.getLogger('mailer').info("Dry run mode specified, not sending")
            return

        mime_msg = MIMEText(msg['Body'] + self.mail_signature)
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
        for queue_name in self.group_queues:
            queue = self.group_queues[queue_name]
            logging.getLogger('mailer').debug('Queue {}: {} calls left'.format(
                queue_name, queue.limited_dequeue.calls_remaining))
            queue.limited_dequeue.update(dt)

        while self.has_messages():
            logging.getLogger('mailer').debug('has_messages() = true')
            msg = self.dequeue()
            if msg is None:
                return

            self.send_message(msg)
