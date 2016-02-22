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
    instance = None

    def __init__(self, dry_run: bool):
        self.mailer = Mailer(dry_run=dry_run)
        self.server = None
        Funnel.instance = self

    @classmethod
    def get_mailer(cls):
        return None if cls.instance is None else cls.instance.mailer

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
        logging.getLogger().debug('Received a HTTP {} request from {}'.format(self.path, self.client_address))
        if self.path == "/mail":
            mfrom = self.headers.get('From', 'no-reply@tim.jyu.fi')
            mto = self.headers.get('Rcpt-To', None)
            msubj = self.headers.get('Subject', 'TIM Notification')
            content_len = int(self.headers.get('content-length', 0))
            mdata = bytes.decode(self.rfile.read(content_len), 'utf-8').replace('<br>', '\n')
            logging.getLogger().debug('Mail from {}, to {}, subject {}, content-length {}'.format(
                mfrom, mto, msubj, content_len))

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
        logging.getLogger().debug('Sent a response to {}: {}'.format(self.client_address, fullmsg))

if __name__ == '__main__':
    fileConfig('logging.ini')
    funnel = Funnel(dry_run='--dry-run' in sys.argv)
    funnel.start()

    try:
        while True:
            funnel.update()

    except KeyboardInterrupt:
        pass

    finally:
        funnel.stop()
