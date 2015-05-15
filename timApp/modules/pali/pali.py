#-*- coding: utf-8 -*-
__author__ = 'vesal'

import http.server
import socketserver
from http_params import *
import datetime

PORT = 5000

def log(self):
    t = datetime.datetime.now()
    agent = " :AG: " + self.headers["User-Agent"]
    if agent.find("ython") >= 0: agent = ""
    logfile = "/opt/tim/timApp/modules/pali/log/log.txt"
    try:
        open(logfile, 'a').write(t.isoformat(' ') + ": " + self.path + agent + " u:" + self.user_id + "\n")
    except Exception as e:
        print(e)
        return

    return


class PaliServer(http.server.BaseHTTPRequestHandler):
    def __init__(self, request, client_address, _server):
        super().__init__(request, client_address, _server)
        self.user_id = "--"

    def do_OPTIONS(self):
        print("do_OPTIONS ==============================================")
        do_headers(self, "text/plain")
        print(self.path)
        print(self.headers)

    def do_GET(self):
        # print("do_GET ==================================================")
        self.do_all(get_params(self))

    def do_POST(self):
        # print("do_POST =================================================")
        if self.path.find('/multihtml') < 0:
            self.do_all(post_params(self))
            return

        print("do_POST MULTIHML ==========================================")
        querys = multi_post_params(self)
        do_headers(self, "application/json")
        htmls = []
        self.user_id = get_param(querys[0], "user_id", "--")
        print("UserId:", self.user_id)
        log(self)
        # print(querys)

        for query in querys:
            # print(query.jso)
            # print(str(query))
            usercode = get_json_param(query.jso, "state", "usercode", None)
            if usercode: query.query["usercode"] = [usercode]
            userinput = get_json_param(query.jso, "state", "userinput", None)
            if userinput: query.query["userinput"] = [userinput]
            userargs = get_json_param(query.jso, "state", "userargs", None)
            if userargs: query.query["userargs"] = [userargs]
            ttype = get_param(query, "type", "cs").lower()
            s = get_html(ttype, query)
            # print(s)
            htmls.append(s)

        # print(htmls)
        sresult = json.dumps(htmls)
        self.wout(sresult)
        log(self)

    def do_PUT(self):
        # print("do_PUT =================================================")
        self.do_all(post_params(self))





# Kun debuggaa Windowsissa, pitää vaihtaa ThreadingMixIn
# Jos ajaa Linuxissa ThreadingMixIn, niin chdir vaihtaa kaikkien hakemistoa?
# Ongelmaa korjattu siten, että kaikki run-kommennot saavat prgpathin käyttöönsä

# if __debug__:
if True:
    class ThreadedHTTPServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
        """Handle requests in a separate thread."""

    print("Debug mode/ThreadingMixIn")
# else:
#    class ThreadedHTTPServer(socketserver.ForkingMixIn, http.server.HTTPServer):
#        """Handle requests in a separate thread."""
#    print("Normal mode/ForkingMixIn")


if __name__ == '__main__':
    server = ThreadedHTTPServer(('', PORT), PaliServer)
    print('Starting server, use <Ctrl-C> to stop')
    server.serve_forever()
