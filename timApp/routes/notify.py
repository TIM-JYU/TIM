import http.client
import socket
from routes.common import *


FUNNEL_HOST = "funnel"
FUNNEL_PORT = 80


def send_email(rcpt, subject, msg):
    conn = None
    try:
        headers = {
            "Host": "tim",
            "Accept-Encoding": "text/plain",
            "Encoding": "text/html",
            "Connection": "close",
            "Subject": subject,
            "Rcpt-To": rcpt}
        conn = http.client.HTTPConnection(FUNNEL_HOST, port=FUNNEL_PORT)
        conn.request("POST", "/mail", body=msg.replace('\n', '<br>'), headers=headers)

        response = conn.getresponse()
        if response.status != 200:
            print(response.status, response.reason)
            data = response.read()
            print(data.decode())

    except (ConnectionError, socket.error, http.client.error) as e:
        print("Couldn't connect to funnel: " + str(e))

    finally:
        if conn is not None:
            conn.close()


def notify_doc_owner(doc_id, subject, msg):
    timdb = getTimDb()
    my_userid = getCurrentUserId()
    owner_group = timdb.documents.get_owner(doc_id)
    for user in timdb.users.get_users_in_group(owner_group):
        if user['id'] != my_userid:
            send_email(user['email'], subject, msg)
