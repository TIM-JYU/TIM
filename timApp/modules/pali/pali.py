#-*- coding: utf-8 -*-
__author__ = 'vesal'
"""
Module for serving TIM example pali plugin.
Serving from local port 5000
"""

import http.server
import socketserver
from http_params import *
import datetime
import binascii
import re

PORT = 5000

def log(self):
    """
     Write notice to log-file.
    :return: nothing
    """
    t = datetime.datetime.now()
    agent = " :AG: " + self.headers["User-Agent"]
    if agent.find("ython") >= 0: agent = ""
    logfile = "/opt/tim/timApp/modules/pali/log/log.txt"
    try:
        open(logfile, 'a').write(t.isoformat(' ') + ": " + self.path + agent + " u:" + self.user_id + "\n")
    except Exception as e:
        print(e)


def get_html(query):
    """
    Return the html for this query. Params are dumbed as hexstring to avoid problems
    with html input and so on.
    :type query: QueryClass
    :rtype : str
    :param query: get or put params
    :return : html string for this markup
    """
    userword = get_json_param(query.jso, "state", "userword", None)
    if userword: query.query["userword"] = [userword]

    user_id = get_param(query, "user_id", "--")
    # print("UserId:", user_id)
    if user_id == "Anonymous": return '<p class="pluginError">The interactive plugin works only for users who are logged in</p><pre class="csRunDiv">' + get_param(query, "byCode", "") + '</pre>'

    jso = query_params_to_json(query.query)
    # print(jso)
    runner = 'pali-runner'
    r = runner
    hx = binascii.hexlify(jso.encode("UTF8"))
    s = '<' + r + '>xxxHEXJSONxxx' + hx.decode() + '</' + r + '>'
    return s


class PaliServer(http.server.BaseHTTPRequestHandler):
    """

    """
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
        if self.path.find('/reqs') >= 0: return self.do_reqs();
        if self.path.find('/favicon.ico') >= 0: return self.send_response(404)
        fname = self.path.split("?")[0]
        if fname.find('.css') >= 0:  return self.send_text_file(fname,"css","text/css")
        if fname.find('.js') >= 0:   return self.send_text_file(fname,"js","application/javascript")
        if fname.find('.html') >= 0: return self.send_text_file(fname,"html","text/html")
        return self.do_all(get_params(self))

    def do_POST(self):
        # print("do_POST =================================================")
        if self.path.find('/multihtml') < 0: return self.do_all(post_params(self))

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
            s = get_html(query)
            # print(s)
            htmls.append(s)

        # print(htmls)
        sresult = json.dumps(htmls)
        self.wout(sresult)
        log(self) # to measure time spend in doing all the html


    def do_PUT(self):
        # print("do_PUT =================================================")
        self.do_all(post_params(self))


    def wout(self, s):
        """
        Write s to servers output stream as UTF8
        :rtype : object
        :type self: PaliServer
        :type s: str
        :param s: string to write
        :return: nothing
        """
        self.wfile.write(s.encode("UTF-8"))


    def send_text_file(self, name, ftype, content_type):
        """
        Sends a file to server from directory ftype with contect_type
        :type name: str
        :type ftype: str
        :type content_type: str
        :param name: files name part, possible extra directories
        :param ftype: files type (js, html, css), specifies also the directory where to get the file
        :param content_type: files_content type
        :return: nothing
        """
        fname = re.sub(".*/","",name)
        do_headers(self, content_type)
        return self.wout(file_to_string(ftype+"/"+fname))


    def do_reqs(self):
        """
        Answer to /reqs route
        :type self: PaliServer
        """
        do_headers(self, "application/json")
        result_json = {"js": ["js/pali.js"], "angularModule": ["paliApp"],
                       "css": ["css/pali.css"]}
        result_str = json.dumps(result_json)
        return self.wout(result_str)


    def do_all(self, query):
        """
        Do all other routes
        :type query: QueryClass
        :param query: post and get params
        :return: nothing
        """

        if self.path.find('/html') >= 0:
            do_headers(self, 'text/html; charset=utf-8')
            s = get_html(query)
            return self.wout(s)

        if self.path.find('/answer') >= 0: return self.do_answer(query)

        do_headers(self, 'text/plain')
        return self.wout("Unknow query: " + self.path)


    def do_answer(self,query):
        """
        Do answer route
        :type query: QueryClass
        :param query: post and get params
        :return: nothing
        """
        do_headers(self, "application/json")
        result = {}
        save = {}
        web = {}
        result["web"] = web
        err = ""
        out = ""

        # userinput = get_json_param(query.jso, "state", "userinput", None)
        # if userinput: query.query["userinput"] = [userinput]

        userword = get_json_param(query.jso, "input", "userword", None)
        if userword: save["usercode"] = userword

        nosave = get_json_param(query.jso, "input", "nosave", None)
        if not nosave: result["save"] = save
        out = "saved"

        out = out[0:20000]
        web["console"] = out
        web["error"] = err

        sresult = json.dumps(result)
        self.wout(sresult)
        # print("Result ========")
        print(sresult)







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
