# -*- coding:utf-8 -*-

# server to get a file from selected range
# Get parameters (every param can have n after, f.ex file1=)
# file    = URL for file to get
# start   = regexp that much match to start printing (default = first line)
#   startcnt= int: how many times the start must match before start (default=1)
#   startn  = int: how many lines to move print forward or backward from start-point (default = 0)
#   end     = regexp to stop printing (default = last line)
#   endcnt  = int: how many times the end must match before end (default=1)
#   endn    = int: how many lines to move end forward or backward from end-point (default = 0)
#   linefmt = format for line number, f.exe linefmt={0:03d}%20 (default = "") 
#   maxn    = max number of lines to print (default=10000)
#   lastn   = last linenumber to print (default=1000000) 
#   url     = if 1, use same url as last file (default=)
#   include = after this file is printied, print this text, \n is new  line (default="")
#   replace = replace every line that match this, by the by-parameter (default="")
#   by      = by what text is the replace replaced (\n is new line), (default="")
#
# Examples
#    ?file=http://example.org/Hello.java&start=main&end=}       -> print from main to first }
#    ?file=http://example.org/Hello.java                        -> print whole file
#    ?file=http://example.org/Hello.java&start=startn=1&endn=-1 -> print file except first and last line
#    ?file=http://example.org/Hello.java&start=main&end=.       -> print only the first line where is main 
#    ?file=http://example.org/Hello.java&start=main&end=.&endn=1  -> print only the first line where is main and next line
#   
import http.server
import subprocess
# import nltk 
import re
from fileParams3 import *

PORT = 5000


def run_while_true(server_class=http.server.HTTPServer,
                   handler_class=http.server.BaseHTTPRequestHandler):
    """
    This assumes that keep_running() is a function of no arguments which
    is tested initially and after each request.  If its return value
    is true, the server continues.
    """
    server_address = ('', PORT)
    httpd = server_class(server_address, handler_class)
    while keep_running():
        httpd.handle_request()


def get_image_html(query):
    """
    Muodostaa kuvan näyttämiseksi tarvittavan HTML-koodin
    :param query: pyynnön paramterit
    :return: kuvan html-jono
    """
    url = get_clean_param(query, "file", "")
    w = get_clean_param(query, "width", "")
    h = get_clean_param(query, "height", "")
    if w: w = 'width="' + w + '" '
    if h: h = 'height="' + h + '" '
    result = get_surrounding_headers(query, '<img ' + w + h + 'src="' + url + '">')
    return result


def get_video_html(query):
    """
    Muodostaa videon näyttämiseksi tarvittavan HTML-koodin
    :param query: pyynnön paramterit
    :return: videon html-jono
    """
    iframe = get_param(query, "iframe", False) or True
    video_type = get_param(query, "type", "icon")
    # print ("iframe " + iframe + " url: " + url)
    video_app = True
    if video_type == "small":
        s = string_to_string_replace_attribute('<small-video-runner \n##QUERYPARAMS##\n></video-runner>', "##QUERYPARAMS##", query)
        return s
    if video_type == "list":
        s = string_to_string_replace_attribute('<list-video-runner \n##QUERYPARAMS##\n></video-runner>', "##QUERYPARAMS##", query)
        return s
    if video_app:
        s = string_to_string_replace_attribute('<video-runner \n##QUERYPARAMS##\n></video-runner>', "##QUERYPARAMS##", query)
        return s

    url = get_clean_param(query, "file", "")
    w = get_clean_param(query, "width", "")
    h = get_clean_param(query, "height", "")
    if w: w = 'width="' + w + '" '
    if h: h = 'height="' + h + '" '

    if iframe:
        return '<iframe class="showVideo" src="' + url + '" ' + w + h + 'autoplay="false" ></iframe>'

        #        result = '<video class="showVideo"  src="' + url + '" type="video/mp4" ' + w + h + 'autoplay="false" controls="" ></video>'
    result = '<video class="showVideo"  src="' + url + '" type="video/mp4" ' + w + h + ' controls="" ></video>'
    return result


class TIMServer(http.server.BaseHTTPRequestHandler):
    def do_OPTIONS(self):
        self.send_response(200, "ok")
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
        print(self.path)
        print(self.headers)

    def do_GET(self):
        self.do_all(get_params(self))

    def do_POST(self):
        if self.path.find('/multihtml') < 0:
            self.do_all(post_params(self))
            return

        print("do_POST MULTIHML ==========================================")
        querys = multi_post_params(self)
        do_headers(self, "application/json")
        htmls = []
        for query in querys:
            usercode = get_json_param(query.jso, "state", "usercode", None)
            if usercode: query.query["usercode"] = [usercode]
            ttype = get_param(query, "type", "console").lower()
            s = get_html(self, query, True)
            # print(s)
            htmls.append(s)

        sresult = json.dumps(htmls)
        self.wout(sresult)

    def do_PUT(self):
        self.do_all(post_params(self))

    def wout(self, s):
        self.wfile.write(s.encode("UTF-8"))

    def do_all(self, query):
        # print(self.path)
        # print(self.headers)
        # print query

        show_html = self.path.find('/html') >= 0
        is_css = self.path.find('/css') >= 0
        is_js = self.path.find('/js') >= 0
        is_reqs = self.path.find('/reqs') >= 0
        is_video_reqs = self.path.find('/video/reqs') >= 0

        content_type = 'text/plain'
        if is_reqs: content_type = "application/json"
        if show_html: content_type = 'text/html; charset=utf-8'
        if is_css: content_type = 'text/css'
        if is_js: content_type = 'application/javascript'

        do_headers(self,content_type)

        if self.path.find("refresh") >= 0:
            self.wout(get_chache_keys())
            clear_cache()
            return

        result_json = {"multihtml": True}

        if is_video_reqs:
            result_json = {"multihtml": True, "js": ["/svn/video/js/video.js"], "angularModule": ["videoApp"]}

        if is_reqs:
            result_str = json.dumps(result_json)
            self.wout(result_str)
            return

        if is_css:
            # printFileTo('cs.css',self.wfile)
            return

        if is_js:
            # print(content_type)
            self.wout(file_to_string('js/video.js'))
            return

        s = get_html(self, query, show_html)
        self.wout(s)
        return


def get_html(self, query, show_html):
    is_image = self.path.find('/image/') >= 0
    is_video = self.path.find('/video/') >= 0

    if is_image:
        s = get_image_html(query)
        return s

    if is_video:
        s = get_video_html(query)
        return s

    # Was none of special, so print the file(s) in query

    cla = get_param(query, "class", "")
    w = get_param(query, "width", "")
    if w: w = ' style="width:' + w + '"'
    if cla: cla = " " + cla

    s = ""
    ffn = get_param(query,"file","")
    fn = ffn
    i = ffn.rfind("/")
    if i >= 0: fn = ffn[i+1:]

    if show_html: s += '<pre class="showCode' + cla + '"' + w + '>'
    s += get_file_to_output(query, show_html)
    if show_html: s += '</pre><p class="smalllink"><a href="' + ffn + '" target="_blank">'+fn+'</a>'
    s = get_surrounding_headers(query, s)
    return s


def keep_running():
    return True


run_while_true(handler_class=TIMServer)
