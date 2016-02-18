#! /usr/bin/python3
import logging
import time
import sys

from http.server import BaseHTTPRequestHandler, HTTPServer
from logging.config import fileConfig
from mailer import Mailer
from typing import Union

HOST_NAME = "0.0.0.0"
HOST_PORT = 80

MAIL_DIR = "/service/mail"


class Funnel:
    mailer = Mailer(dry_run='--dry-run' in sys.argv)

    def __init__(self):
        self.server = None

    @classmethod
    def get_mailer(cls):
        return cls.mailer

    def start(self):
        if self.server is not None:
            logging.getLogger().warn('Server already started!')
            return
        self.server = HTTPServer((HOST_NAME, HOST_PORT), MyServer)
        logging.getLogger().info('Server starts')

    def stop(self):
        if self.server is None:
            print("Server not running")
            return
        self.server.server_close()
        self.server = None
        logging.getLogger().info('Server stops')

    def update(self):
        if self.server:
            self.server.handle_request()
            self.mailer.update()


class MyServer(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == "/mail":
            mfrom = self.headers.get('From', 'no-reply@tim.jyu.fi')
            mto = self.headers.get('Rcpt-To', None)
            msubj = self.headers.get('Subject', 'TIM Notification')
            content_len = int(self.headers.get('content-length', 0))
            mdata = str(self.rfile.read(content_len)).replace('<br>', '\n')

            if mto is None:
                self.send_str_response(400, 'Missing message recipient', self.headers.items())
                return
            if mdata == '':
                self.send_str_response(400, 'Missing message data', self.headers.items())
                return

            Funnel.get_mailer().enqueue(mfrom, mto, msubj, mdata)
            self.send_str_response(200, 'Message queued')
        else:
            self.send_str_response(400, 'Unknown route ' + self.path)

    def send_str_response(self, status: int, msg: str, headers: Union[dict, None] = None):
        self.send_response(status)
        self.send_header("Content-type", "text/html")
        self.end_headers()

        fullmsg = '{}\n\nHeaders: {}\n'.format(msg, headers) if headers else msg + '\n'
        self.wfile.write(bytes(fullmsg, 'utf-8'))

if __name__ == '__main__':
    fileConfig('logging.ini')
    funnel = Funnel()
    funnel.start()

    try:
        while True:
            funnel.update()

    except KeyboardInterrupt:
        pass

    finally:
        funnel.stop()
