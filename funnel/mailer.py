import os
import random
import string
import time


MAIL_DIR = "/service/mail"


class Mailer:
    @classmethod
    def queue_mail(cls, sender: str, rcpt: str, msg: str):
        if not os.path.exists(MAIL_DIR):
            os.mkdir(MAIL_DIR)

        ordinal = str(len(os.listdir(MAIL_DIR)))
        random_part = ''.join([random.choice(string.ascii_lowercase) for _ in range(6)])
        fname = os.path.join(MAIL_DIR, ordinal + '_' + random_part)

        with open(fname, 'w') as f:
            f.write('\n'.join([sender, rcpt, msg]))


    @classmethod
    def update(cls):
        pass

