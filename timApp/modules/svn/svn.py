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
import BaseHTTPServer
import subprocess
# import nltk 
import re
from fileParams import *

PORT = 5000


def run_while_true(server_class=BaseHTTPServer.HTTPServer,
                   handler_class=BaseHTTPServer.BaseHTTPRequestHandler):
    """
    This assumes that keep_running() is a function of no arguments which
    is tested initially and after each request.  If its return value
    is true, the server continues.
    """
    server_address = ('', PORT)
    httpd = server_class(server_address, handler_class)
    while keep_running():
        httpd.handle_request()


class TIMServer(BaseHTTPServer.BaseHTTPRequestHandler):
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
        print
        self.path
        print
        self.headers
        # print query

        html = self.path.find('/html') >= 0
        css = self.path.find('/css') >= 0
        js = self.path.find('/js') >= 0
        reqs = self.path.find('/reqs') >= 0

        self.send_response(200)
        # self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, PUT, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
        self.send_header('Content-type', 'text/plain')
        self.end_headers()

        content_type = 'text/plain'
        if ( reqs ): content_type = "application/json"
        if ( html ): content_type = 'text/html'
        if ( css ): content_type = 'text/css'
        if ( js ): content_type = 'application/javascript'
        self.send_header('Content-type', content_type)

        if ( reqs ):
            resultJSON = {}
            resultStr = json.dumps(resultJSON)
            self.wfile.write(resultStr);
            return

        if ( css ):
            # printFileTo('cs.css',self.wfile)
            return
        if ( js ):
            # printFileTo('js/dir.js',self.wfile)
            return

        if ( html ): self.wfile.write('<pre class="showCode">')

        # self.wfile.write("kissa")
        # subprocess.call(["svn","export","https://svn.cc.jyu.fi/srv/svn/ohj2/esimerkit/k2014/luennot/live03/src/hello/Hello.java","--force"], shell=False)
        # filecontent = open("Hello.java").read()
        # url = "https://svn.cc.jyu.fi/srv/svn/ohj2/esimerkit/k2014/luennot/live03/src/hello/Hello.java"
        # print "startcnt {0} endcnt {1}".format(startcnt,endcnt)
        p0 = FileParams(query, "", "")
        if ( p0.url == "" ):
            self.wfile.write("Must give file= -parameter")
            return
        p0.printFile(self.wfile, html)
        p0.printInclude(self.wfile, html)
        u = p0.url;
        for i in range(1, 10):
            p = FileParams(query, str(i), u)
            p.printFile(self.wfile, html)
            p.printInclude(self.wfile, html)
            if ( p.url ): u = p.url

        if ( html ): self.wfile.write('</pre>')


def keep_running():
    return True


run_while_true(handler_class=TIMServer)
