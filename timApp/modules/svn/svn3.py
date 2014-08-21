# -*- coding:utf-8 -*-

# server to get a file from selected range
# Get parameters (every param can have n after, f.ex file1=)
# file    = URL for file to get
#   start   = regexp that much match to start printing (default = first line)
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


def show_image(self, query):
    """
    Muodostaa kuvan näyttämiseksi tarvittavan HTML-koodin
    :param self: olio josta löytyy tarvittava tietovirta
    :param query: pyynnön paramterit
    :return:
    """
    url = get_param(query, "file", "")
    w = get_param(query, "width", "")
    h = get_param(query, "height", "")
    if w: w = 'width="' + w + '" '
    if h: h = 'height="' + h + '" '
    result = '<img ' + w + h + 'src="' + url + '">'
    self.wfile.write(result.encode())
    return


def show_video(self, query):
    """
    Muodostaa videon näyttämiseksi tarvittavan HTML-koodin
    :param self: olio josta löytyy tarvittava tietovirta
    :param query: pyynnön paramterit
    :return:
    """
    url = get_param(query, "file", "")
    w = get_param(query, "width", "")
    h = get_param(query, "height", "")
    if w: w = 'width="' + w + '" '
    if h: h = 'height="' + h + '" '
    iframe = get_param(query, "iframe", False)
    iframe = True
    # print ("iframe " + iframe + " url: " + url)
    videoApp = True
    if videoApp:
        printStringToReplaceAttribute('<video-runner \n##QUERYPARAMS##\n></video-runner>', self.wfile, "##QUERYPARAMS##", query)
        return

    elif iframe:
        result = '<iframe class="showVideo" src="' + url + '" ' + w + h + 'autoplay="false" ></iframe>'
    else:
        #        result = '<video class="showVideo"  src="' + url + '" type="video/mp4" ' + w + h + 'autoplay="false" controls="" ></video>'
        result = '<video class="showVideo"  src="' + url + '" type="video/mp4" ' + w + h + ' controls="" ></video>'
    self.wfile.write(result.encode())
    return


class TIMServer(http.server.BaseHTTPRequestHandler):
    def do_OPTIONS(self):
        self.send_response(200, "ok")
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
        print(self.path)
        print(self.headers)

    def do_GET(self):
        self.doAll(getParams(self))

    def do_POST(self):
        self.doAll(postParams(self))

    def do_PUT(self):
        self.doAll(postParams(self))

    def doAll(self, query):
        print(self.path)
        print(self.headers)
        # print query

        show_html = self.path.find('/html') >= 0
        css = self.path.find('/css') >= 0
        js = self.path.find('/js') >= 0
        reqs = self.path.find('/reqs') >= 0
        image = self.path.find('/image/html') >= 0
        video = self.path.find('/video/html') >= 0
        video_reqs = self.path.find('/video/reqs') >= 0

        self.send_response(200)
        # self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, PUT, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")

        content_type = 'text/plain'
        if reqs: content_type = "application/json"
        if show_html: content_type = 'text/html'
        if css: content_type = 'text/css'
        if js: content_type = 'application/javascript'
        self.send_header('Content-type', content_type)
        self.end_headers()

        if image:
            show_image(self, query)
            return

        if video:
            show_video(self, query)
            return

        resultJSON = {}

        if video_reqs:
            resultJSON = {"js": ["http://tim-beta.it.jyu.fi/svn/video/js/video.js"], "angularModule": ["videoApp"]}

        if reqs:
            resultStr = json.dumps(resultJSON)
            self.wfile.write(resultStr.encode())
            return

        if css:
            # printFileTo('cs.css',self.wfile)
            return
        
        if js:
            print(content_type)
            printFileTo('js/video.js', self.wfile)
            return

        if show_html: self.wfile.write('<pre class="showCode">'.encode())

        p0 = FileParams(query, "", "")
        if p0.url == "":
            self.wfile.write("Must give file= -parameter".encode())
            if show_html: self.wfile.write('</pre>'.encode())
            return
        p0.printFile(self.wfile, show_html)
        p0.printInclude(self.wfile, show_html)
        u = p0.url
        for i in range(1, 10):
            p = FileParams(query, str(i), u)
            p.printFile(self.wfile, show_html)
            p.printInclude(self.wfile, show_html)
            if p.url: u = p.url

        if show_html: self.wfile.write('</pre>'.encode())


def keep_running():
    return True


run_while_true(handler_class=TIMServer)
