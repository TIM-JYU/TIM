import http.client
from routes.common import *


FUNNEL_HOST = "funnel"
FUNNEL_PORT = 80


def send_email(rcpt, msg):
    conn = None
    try:
        headers = {"Host": "tim", "Encoding": "text/plain", "Rcpt-To": rcpt, "Msg-Data": msg}
        conn = http.client.HTTPConnection(FUNNEL_HOST, port=FUNNEL_PORT)
        conn.request("POST", "/mail", headers=headers)

        response = conn.getresponse()
        if response.status != 200:
            print(response.status, response.reason)
            data = response.read()
            print(data.decode())

    except ConnectionError as e:
        print("Couldn't connect to funnel: " + str(e))

    finally:
        if conn is not None:
            conn.close()


def notify_doc_owner(doc_id, msg):
    timdb = getTimDb()
    my_userid = getCurrentUserId()
    owner_group = timdb.documents.get_owner(doc_id)
    for user in timdb.users.get_users_in_group(owner_group):
        if user['id'] != my_userid:
            send_email(user['email'], msg)
