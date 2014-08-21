# -*- coding: utf-8 -*-  

import http.server
import subprocess
# import nltk
import re
import os.path
import uuid
import io
import codecs
from os import kill
import signal
# from signal import alarm, signal, SIGALRM, SIGKILL
from subprocess import PIPE, Popen, check_output
from fileParams3 import *

print("Kaynnistyy")

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


def generate_filename():
    return str(uuid.uuid4())


def run(args, cwd=None, shell=False, kill_tree=True, timeout=-1, env=None):
    class Alarm(Exception):
        pass

    def alarm_handler(signum, frame):
        raise Alarm

    p = Popen(args, shell=shell, cwd=cwd, stdout=PIPE, stderr=PIPE, env=env)
    if timeout != -1:
        signal.signal(signal.SIGALRM, alarm_handler)
        signal.alarm(timeout)
    try:
        stdout, stderr = p.communicate()
        if timeout != -1:
            signal.alarm(0)
    except Alarm:
        pids = [p.pid]
        if kill_tree:
            pids.extend(get_process_children(p.pid))
        for pid in pids:
            # process might have died before getting to this line
            # so wrap to avoid OSError: no such process
            try:
                kill(pid, signal.SIGKILL)
            except OSError:
                pass
        return -9, '', ''
    return p.returncode, stdout, stderr


def get_process_children(pid):
    p = Popen('ps --no-headers -o pid --ppid %d' % pid, shell=True,
              stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    return [int(p) for p in stdout.split()]


def print_lines(file, lines, n1, n2):
    linefmt = "{0:03d} "
    n = len(lines)
    if n1 < 0: n1 = 0
    if n2 >= n: n2 = n - 1

    ni = 0
    for i in range(n1, n2 + 1):
        line = lines[i]
        ln = linefmt.format(i + 1)
        file.write((ln + line + "\n"))


def write_json_error(file, err):
    result = {"web": {"error": err}}
    result_str = json.dumps(result)
    file.write(result_str.encode())
    print("ERROR:======== ", err.encode("UTF8"))


def remove_before(what, s):
    print("=================================== WHAT ==============")
    print(what, " ", s)
    i = s.find(what)
    if i < 0: return s
    s = s[i + 1:]
    i = s.find("\n")
    if i < 0: return ""
    return s[i + 1:]


class TIMServer(http.server.BaseHTTPRequestHandler):
    def do_OPTIONS(self):
        print("do_OPTIONS ==============================================")
        self.send_response(200, "ok")
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
        print(self.path)
        print(self.headers)


    def do_GET(self):
        print("do_GET ==================================================")
        self.do_all(get_params(self))


    def do_POST(self):
        print("do_POST =================================================")
        self.do_all(post_params(self))


    def do_PUT(self):
        print("do_PUT =================================================")
        self.do_all(post_params(self))

    def wout(self, s):
        self.wfile.write(s.encode("UTF-8"))

    def remove(self,fname):
        try:
            os.remove(fname)
        except:
            return

    def get_html(self, ttype, query):
        if ttype == "console":
            s = string_to_string_replace_attribute('<cs-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-runner>',
                                                   "##QUERYPARAMS##", query)
        elif ttype == "comtest":
            s = string_to_string_replace_attribute(
                '<cs-comtest-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-runner>',
                "##QUERYPARAMS##", query)
        else:
            s = string_to_string_replace_attribute(
                '<cs-jypeli-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-jypeli-runner>',
                "##QUERYPARAMS##", query)
        return s


    def do_all(self, query):
        result = query.jso
        if not result: result = {}
        web = {}
        result["web"] = web

        print("doAll ===================================================")
        print(self.path)
        print(self.headers)
        # print query

        is_fullhtml = self.path.find('/fullhtml') >= 0
        is_html = self.path.find('/html') >= 0
        is_css = self.path.find('/css') >= 0
        is_js = self.path.find('/js') >= 0
        is_reqs = self.path.find('/reqs') >= 0
        is_iframe_param = get_param_del(query, "iframe", "")
        is_iframe = (self.path.find('/iframe') >= 0) or is_iframe_param
        is_answer = self.path.find('/answer') >= 0


        # korjaus kunnes byCode parametri tulee kokonaisena
        # tempBy = get_param(query, "b", "")
        # if ( tempBy ):
        # query["byCode"] = [tempBy];

        # print query
        self.send_response(200)
        # self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
        content_type = 'text/plain'
        if is_reqs: content_type = "application/json"
        if is_fullhtml or is_html: content_type = 'text/html'
        if is_css: content_type = 'text/css'
        if is_js or is_answer: content_type = 'application/javascript'
        self.send_header('Content-type', content_type)
        self.end_headers()
        # Get the template type
        ttype = get_param(query, "type", "console").lower()

        if is_reqs:
            result_json = {"js": ["http://tim-beta.it.jyu.fi/cs/js/dir.js"], "angularModule": ["csApp"],
                           "css": ["http://tim-beta.it.jyu.fi/cs/css/cs.css"]}
            result_str = json.dumps(result_json)
            self.wout(result_str)
            return

        # if ( query.jso != None and query.jso.has_key("state") and query.jso["state"].has_key("usercode") ):
        usercode = get_json_param(query.jso, "state", "usercode", None)
        if usercode: query.query["usercode"] = [usercode]

        print("Muutos ========")
        # pprint(query.__dict__, indent=2)

        if is_css:
            self.wout(file_to_string('cs.css'))
            return

        if is_js:
            if self.path.find('rikki') >= 0:
                self.wout(file_to_string('js/dirRikki.js'))
                return
            self.wout(file_to_string('js/dir.js'))
            return

        if is_html and not is_iframe:
            print("HTML:==============")
            self.wout(self.get_html(ttype, query))
            return

        if is_fullhtml:
            self.wout(file_to_string('begin.html'))
            self.wout(self.get_html(ttype, query))
            self.wout(file_to_string('end.html'))
            return

        if is_iframe:
            s = string_to_string_replace_url(
                '<iframe frameborder="0"  src="http://tim-beta.it.jyu.fi/cs/fullhtml?##QUERYPARAMS##" style="overflow:hidden;height:##HEIGHT##;width:100%"  seamless></iframe>',
                "##QUERYPARAMS##", query)
            self.wout(s)
            return

        # Generate random cs and exe filenames
        basename = generate_filename()
        csfname = "/tmp/%s.cs" % basename
        exename = "/tmp/%s.exe" % basename

        # Check query parameters
        p0 = FileParams(query, "", "")
        print("p0=")
        print(p0.replace)
        if p0.url == "" and p0.replace == "":
            self.wfile.write("Must give file= -parameter".encode())
            return

        print_file = get_param(query, "print", "")
        print("type=" + ttype)

        if ttype == "console":
            # Console program
            pass
        elif ttype == "jypeli":
            # Jypeli game
            bmpname = "/tmp/%s.bmp" % basename
            pngname = "/cs/images/%s.png" % basename
            pass
        elif ttype == "comtest":
            # ComTest test cases
            testcs = "/tmp/%sTest.cs" % basename
            testdll = "/tmp/%sTest.dll" % basename
        else:
            # Unknown template
            self.wfile.write(("Invalid project type given (type=" + ttype + ")").encode())
            return

        s = p0.get_file()
        # print("FILE: XXXXX",s)
        s += p0.get_include()
        u = p0.url
        for i in range(1, 10):
            p = FileParams(query, str(i), u)
            s += p.get_file()
            s += p.get_include()
            if p.url: u = p.url


        # Open the file and write it
        if print_file:
            self.wout(s)
            return
        csfile = codecs.open(csfname, "w", "utf-8")  # open(csfname, "w")
        csfile.write(s)
        csfile.close()

        if not os.path.isfile(csfname) or os.path.getsize(csfname) == 0:
            write_json_error(self.wfile, "Could not get the source file")
            # self.wfile.write("Could not get the source file\n")
            # print "=== Could not get the source file"
            return

        # Compile
        try:
            if ttype == "jypeli":
                cmdline = "mcs /out:%s /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/Jypeli.MonoGame.Framework.dll /r:/cs/jypeli/Jypeli.Physics2d.dll /r:/cs/jypeli/OpenTK.dll /r:/cs/jypeli/Tao.Sdl.dll /r:System.Drawing /cs/jypeli/Ohjelma.cs /cs/jypeli/Screencap.cs %s" % (
                    exename, csfname)
            elif ttype == "comtest":
                cmdline = "java -jar /tmp/ComTest.jar nunit %s && mcs /out:%s /target:library /reference:/usr/lib/mono/gac/nunit.framework/2.6.0.0__96d09a1eb7f44a77/nunit.framework.dll %s %s" % (
                    csfname, testdll, csfname, testcs)
            else:
                cmdline = "mcs /out:%s %s" % (exename, csfname)

            check_output([cmdline], stderr=subprocess.STDOUT, shell=True)
            # self.wfile.write("*** Success!\n")
            print("*** Success")
        except subprocess.CalledProcessError as e:
            '''
            self.wout("!!! Error code " + str(e.returncode) + "\n")
            self.wout(e.output)
            file = open(csfname, 'r')
            lines = file.read().splitlines()
            # self.wfile.write(file.read())
            printLines(self.wfile,lines,0,10000)
            '''
            error_str = "!!! Error code " + str(e.returncode) + "\n"
            error_str += e.output.decode("utf-8") + "\n"
            # errorStr = re.sub("^/tmp/.*cs\(\n", "tmp.cs(", errorStr, flags=re.M)
            error_str = error_str.replace(csfname,"tmp.cs")
            output = io.StringIO()
            file = codecs.open(csfname, 'r', "utf-8")
            lines = file.read().splitlines()
            file.close()
            print_lines(output, lines, 0, 10000)
            error_str += output.getvalue()
            output.close()

            self.remove(csfname)
            write_json_error(self.wfile, error_str)
            return

        if ttype == "jypeli":
            code, out, err = run(["mono", exename, bmpname], timeout=10)
            if type(out) != type(''): out = out.decode()
            run(["convert", "-flip", bmpname, pngname])
            self.remove(bmpname)
            # self.wfile.write("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/%s.png\n" % (basename))
            print("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/%s.png\n" % basename)
            # TODO: clean up screenshot directory
            p = re.compile('Number of joysticks:.*\n.*')
            # out = out.replace("Number of joysticks:.*","")
            out = p.sub("", out)
            if code == -9: 
                out = "Runtime exceeded, maybe loop forever\n" + out
            else:
                web["image"] = "http://tim-beta.it.jyu.fi/csimages/" + basename + ".png"
            self.remove(exename)
        elif ttype == "comtest":
            eri = -1		
            code, out, err = run(["nunit-console", "-nologo", "-nodots", testdll], timeout=10)
            if type(out) != type(''): out = out.decode()
            # print(code,out,err)
            out = remove_before("Execution Runtime:", out)
            if code == -9: 
                out = "Runtime exceeded, maybe loop forever\n" + out
                eri = 0
            # out = out[1:]  # alussa oleva . pois
            # out = re.sub("at .*", "", out, flags=re.M)
            # out = re.sub("\n\n+", "", out, flags=re.M)
            out = re.sub("^at .*\n", "", out, flags=re.M)
            out = re.sub("Errors and Failures.*\n", "", out, flags=re.M)
            out = out.strip(' \t\n\r')
            if eri < 0: eri = out.find("Test Failure")
            if eri < 0: eri = out.find("Test Error")
            web["testGreen"] = True
            if eri >= 0:
                web["testGreen"] = False
                web["testRed"] = True
                lni = out.find(", line ")
                if lni >= 0:
                    lns = out[lni + 7:]
                    lns = lns[0:lns.find("\n")]
                    lnro = int(lns)
                    lines = codecs.open(csfname, "r", "utf-8").readlines()
                    # print("Line nr: "+str(lnro))
                    ## out += "\n" + str(lnro) + " " + lines[lnro - 1]
                    web["comtestError"] = str(lnro) + " " + lines[lnro - 1]
            self.remove(testcs)
            self.remove(testdll)
        else:
            print("Exe: ", exename)
            code, out, err = run(["mono", exename], timeout=10)
            if type(out) != type(''): out = out.decode()
            if code == -9: 
                out = "Runtime exceeded, maybe loop forever\n" + out
            self.remove(exename)
			
        out = out[0:2000]
        web["console"] = out
        result["web"] = web

        # Clean up
        self.remove(csfname)

        # self.wfile.write(out)
        # self.wfile.write(err)
        sresult = json.dumps(result)
        self.wout(sresult)
        print("Result ========")
        print(sresult)
        print(out)
        print(err)


def keep_running():
    return True


run_while_true(handler_class=TIMServer)

