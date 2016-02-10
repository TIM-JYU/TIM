import os
import random
import string
import time

from http.server import BaseHTTPRequestHandler, HTTPServer

HOST_NAME = "localhost"
HOST_PORT = 80

MAIL_DIR = "/service/mail"


class MyServer(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == "/mail":
            mfrom = self.headers.get('From', 'no-reply@tim.jyu.fi')
            mto = self.headers.get('To', None)
            mdata = self.headers.get('Msg', None)

            if mto is None:
                self.send_str_response(400, 'Missing message recipient')
                return
            if mdata is None:
                self.send_str_response(400, 'Missing message data')
                return

            self.queue_mail(mfrom, mto, mdata)
            self.send_str_response(200, 'Message queued')
        else:
            self.send_str_response(400, 'Unknown route ' + self.path)

    def send_str_response(self, status: str, msg: str):
        self.send_response(status)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(bytes(msg + '\n', 'utf-8'))

    @classmethod
    def queue_mail(cls, sender: str, rcpt: str, msg: str):
        if not os.path.exists(MAIL_DIR):
            os.mkdir(MAIL_DIR)

        ordinal = str(len(os.listdir(MAIL_DIR)))
        random_part = ''.join([random.choice(string.ascii_lowercase) for _ in range(6)])
        fname = os.path.join(MAIL_DIR, ordinal + '_' + random_part)

        with open(fname, 'w') as f:
            f.write('\n'.join([sender, rcpt, msg]))

if __name__ == '__main__':
    myServer = HTTPServer((HOST_NAME, HOST_PORT), MyServer)
    print(time.asctime(), "Server Starts - %s:%s" % (HOST_NAME, HOST_PORT))

    try:
        myServer.serve_forever()
    except KeyboardInterrupt:
        pass

    myServer.server_close()
    print(time.asctime(), "Server Stops - %s:%s" % (HOST_NAME, HOST_PORT))


