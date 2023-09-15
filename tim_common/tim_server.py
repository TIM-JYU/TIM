"""
Base class for TIM plugin server. THIS IS DEPRECATED, DO NOT USE IN NEW CODE!
Serving from local port 5000.
"""
import http.server
import json
import logging
import os
import socketserver

from tim_common.fileParams import (
    get_template,
    file_to_string,
    do_headers,
    multi_post_params,
    get_param,
    QueryClass,
    get_params,
    post_params,
)

PORT = 5000
PROGDIR = "."


class TimServer(http.server.BaseHTTPRequestHandler):
    """Base class for TIM-server. THIS IS DEPRECATED, DO NOT USE IN NEW CODE!"""

    def __init__(self, request, client_address, _server):
        super().__init__(request, client_address, _server)
        self.user_id = "--"

    def do_OPTIONS(self):
        """Do needed things for OPTIONS request.

        :return: nothing

        """
        print("do_OPTIONS ==============================================")
        do_headers(self, "text/plain")
        print(self.path)
        print(self.headers)

    def do_GET(self):
        """Do needed things for GET request.

        :return: nothing

        """
        # print("do_GET ==================================================")
        if self.path.find("/reqs") >= 0:
            return self.do_reqs()
        if self.path.find("/favicon.ico") >= 0:
            return self.send_response(404)
        if self.path.find("/template") >= 0:
            return self.send_text(self.do_template(get_params(self)), "text/plain")
        fname = self.path.split("?")[0]
        if fname.find(".css") >= 0:
            return self.send_text_file(fname, "css", "text/css")
        if fname.find(".js") >= 0:
            return self.send_text_file(fname, "js", "application/javascript")
        if fname.find(".html") >= 0:
            return self.send_text_file(fname, "html", "text/html")
        return self.do_all(get_params(self))

    def do_POST(self):
        """Do needed things for POST request This may be a f.ex a request single html-plugin or multiple plugins.

        :return: nothing

        """
        # print("do_POST =================================================")
        if self.path.find("/multihtml") < 0:
            return self.do_all(post_params(self))

        print("do_POST MULTIHTML ==========================================")
        queries = multi_post_params(self)
        do_headers(self, "application/json")
        htmls = []
        self.user_id = get_param(queries[0], "user_id", "--")
        print("UserId:", self.user_id)
        log(self)
        # print(queries)

        for query in queries:
            # print(query.jso)
            # print(str(query))
            s = self.get_html(query)
            # print(s)
            htmls.append(s)

        # print(htmls)
        sresult = json.dumps(htmls)
        self.wout(sresult + "\n")
        log(self)  # to measure time spend in doing all the html

    def do_PUT(self):
        """Do needed things for PUT request.

        :return: nothing

        """
        # print("do_PUT =================================================")
        self.do_all(post_params(self))

    def wout(self, s: str):
        """Write s to servers output stream as UTF8.

        :rtype : object
        :param s: string to write
        :return: nothing

        """
        self.wfile.write(s.encode("UTF-8"))

    def send_text_file(self, name: str, ftype: str, content_type: str):
        """Sends a file to server from directory ftype with content_type.

        :param name: files name part, possible extra directories
        :param ftype: files type (js, html, css), specifies also the directory where to get the file
        :param content_type: files_content type
        :return: nothing

        """
        # fname = re.sub(".*/", "", name)
        fname = os.path.basename(name)
        do_headers(self, content_type)
        return self.wout(file_to_string(ftype + "/" + fname))

    def send_text(self, txt: str, content_type: str):
        """Sends a txt to server.

        :param txt: text to send
        :param content_type: files_content type
        :return: nothing

        """
        # fname = re.sub(".*/", "", name)
        do_headers(self, content_type)
        return self.wout(txt)

    def get_html(self, query: QueryClass) -> str:
        """Return the html for this query. Params are dumbed as hexstring to avoid problems with html input and so on.

        :rtype : str
        :param query: get or put params
        :return : html string for this markup

        """
        return ""

    def get_reqs_result(self) -> dict:
        """
        :return: reqs result as json
        """
        return {}

    def do_reqs(self):
        """Answer to /reqs route.

        :type self: TimServer

        """
        do_headers(self, "application/json")
        result_json = self.get_reqs_result()
        result_str = json.dumps(result_json)
        return self.wout(result_str)

    def do_template(self, query: QueryClass):
        """Gets a template.

        :rtype : str
        :param query: get or put params
        :return: template result as json

        """
        tempfile = get_param(query, "file", "")
        tidx = get_param(query, "idx", "0")
        return get_template("templates", tidx, tempfile)

    def do_all(self, query: QueryClass):
        """Do all other routes.

        :param query: post and get params
        :return: nothing

        """

        if self.path.find("/html") >= 0:
            do_headers(self, "text/html; charset=utf-8")
            s = self.get_html(query)
            return self.wout(s)

        if self.path.find("/answer") >= 0:
            return self.do_answer(query)

        do_headers(self, "text/plain")
        return self.wout("Unknown query: " + self.path)

    def do_answer(self, query: QueryClass):
        """Do answer route.

        :param query: post and get params
        :return: nothing

        """


def log(request: TimServer):
    """Log the time and user.

    :param request:
    :return: Nothing

    """
    agent = " :AG: " + request.headers["User-Agent"]
    if agent.find("ython") >= 0:
        agent = ""
    logging.info(request.path + agent + " u:" + request.user_id)


# Kun debuggaa Windowsissa, pitää vaihtaa ThreadingMixIn
# Jos ajaa Linuxissa ThreadingMixIn, niin chdir vaihtaa kaikkien hakemistoa?
# Ongelmaa korjattu siten, että kaikki run-kommennot saavat prgpathin käyttöönsä


# if __debug__:
# if True:
class ThreadedHTTPServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
    """Handle requests in a separate thread."""

    print("Debug mode/ThreadingMixIn")


# else:
#    class ThreadedHTTPServer(socketserver.ForkingMixIn, http.server.HTTPServer):
#        """Handle requests in a separate thread."""
#    print("Normal mode/ForkingMixIn")


def start_server(http_server):
    if not os.path.exists("/var/log"):
        os.makedirs("/var/log")
    # Logging to file is disabled for now because Docker redirects stdin to an internal JSON file automatically
    # and setting ownership to volumes via Docker is not possible.
    # logging.basicConfig(filename='/var/log/' + logname + '.log', level=logging.INFO, format='%(asctime)s %(message)s')
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")

    server = ThreadedHTTPServer(("", PORT), http_server)
    print("Starting server, use <Ctrl-C> to stop")
    logging.info("Starting server")
    server.serve_forever()
