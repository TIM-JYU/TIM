# -*- coding: utf-8 -*-  

import http.server
import subprocess
# import nltk
import re
import os.path
import uuid
import io
import codecs
import binascii
from os import kill
import signal
import socketserver
# from signal import alarm, signal, SIGALRM, SIGKILL
from subprocess import PIPE, Popen, check_output
from fileParams3 import *

print("Kaynnistyy")

PORT = 5000


class ThreadingServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
    pass

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

    p = Popen(args, shell=shell, cwd=cwd, stdout=PIPE, stderr=PIPE, env=env) #, timeout=timeout)
    #if timeout != -1:
    #    signal.signal(signal.SIGALRM, alarm_handler)
    #    signal.alarm(timeout)
    print(p.pid)
    try:
        stdout, stderr = p.communicate(timeout=timeout)
        #if timeout != -1:
        #    signal.alarm(0)
    except  subprocess.TimeoutExpired:
        '''
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
        '''        
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


def write_json_error(file, err, result):
    result["web"] = {"error": err}
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


def get_html(ttype, query):
    js = query_params_to_map(query.query)
    jso = json.dumps(js)
    runner = 'cs-runner'
    if "comtest" in ttype: runner = 'cs-comtest-runner'
    if "tauno" in ttype: runner = 'cs-tauno-runner'
    if "jypeli" in ttype: runner = 'cs-jypeli-runner'
    s = '<' + runner + '>xxxJSONxxx' + jso + '</' + runner + '>'
    if ttype == "c1" or True: # c1 oli testejä varten ettei sinä aikana rikota muita.
        hx = binascii.hexlify(jso.encode("UTF8"))
        s = '<' + runner + '>xxxHEXJSONxxx' + hx.decode() + '</' + runner + '>'
    return s


class TIMServer(http.server.BaseHTTPRequestHandler):
    def do_OPTIONS(self):
        print("do_OPTIONS ==============================================")
        self.send_response(200, "ok")
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "version, X-Requested-With, Content-Type")
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

    def remove(self, fname):
        try:
            os.remove(fname)
        except:
            return


    def do_all(self, query):
        result = {}  # query.jso
        if not result: result = {}
        save = {}
        web = {}
        result["web"] = web

        print("doAll ===================================================")
        print(self.path)
        print(self.headers)
        # print query

        if self.path.find('/favicon.ico') >= 0:
            self.send_response(404)
            return

        if self.path.find('/login') >= 0:
            query = post_params(self)
            return

        is_fullhtml = self.path.find('/fullhtml') >= 0
        is_html = self.path.find('/html') >= 0 or self.path.find('.html') >= 0
        is_css = self.path.find('.css') >= 0
        is_js = self.path.find('.js') >= 0
        is_reqs = self.path.find('/reqs') >= 0
        is_iframe_param = get_param_del(query, "iframe", "")
        is_iframe = (self.path.find('/iframe') >= 0) or is_iframe_param
        is_answer = self.path.find('/answer') >= 0
        is_tauno = self.path.find('/tauno') >= 0
        is_ptauno = self.path.find('/ptauno') >= 0

        self.send_response(200)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "version, X-Requested-With, Content-Type")
        content_type = 'text/plain'
        if is_reqs  or is_answer: content_type = "application/json"
        if is_fullhtml or is_html: content_type = 'text/html; charset=utf-8'
        if is_css: content_type = 'text/css'
        if is_js: content_type = 'application/javascript'
        self.send_header('Content-type', content_type)
        self.end_headers()

        if self.path.find("refresh") >= 0:
            self.wout(get_chache_keys())
            clear_cache()
            return

        if is_ptauno:
            p = self.path.split("?")
            self.wout(file_to_string(p[0]))
            return


            # Get the template type
        ttype = get_param(query, "type", "console").lower()
        if is_tauno and not is_answer: ttype = 'tauno' # answe is newer tauno

        if is_reqs:
            result_json = {"js": ["http://tim-beta.it.jyu.fi/cs/js/dir.js"], "angularModule": ["csApp"],
                          "css": ["http://tim-beta.it.jyu.fi/cs/css/cs.css"]}
            #result_json = {"js": ["js/dir.js"], "angularModule": ["csApp"],
            #               "css": ["css/cs.css"]}
            result_str = json.dumps(result_json)
            return self.wout(result_str)

        # if ( query.jso != None and query.jso.has_key("state") and query.jso["state"].has_key("usercode") ):
        usercode = get_json_param(query.jso, "state", "usercode", None)
        if usercode: query.query["usercode"] = [usercode]
        print("USERCODE: XXXXX = ", usercode)

        print("Muutos ========")
        # pprint(query.__dict__, indent=2)

        if is_css:
            return self.wout(file_to_string('cs.css'))

        if is_js:
            if self.path.find('rikki') >= 0:
                return self.wout(file_to_string('js/dirRikki.js'))
            return self.wout(file_to_string('js/dir.js'))

        if is_html and not is_iframe:
            print("HTML:==============")
            s = get_html(ttype, query)
            print(s)
            return self.wout(s)

        if is_fullhtml:
            self.wout(file_to_string('begin.html'))
            self.wout(get_html(ttype, query))
            self.wout(file_to_string('end.html'))
            return

        if is_iframe:
            s = string_to_string_replace_url(
                '<iframe frameborder="0"  src="http://tim-beta.it.jyu.fi/cs/fullhtml?##QUERYPARAMS##" style="overflow:hidden;" height="##HEIGHT##" width="100%"  seamless></iframe>',
                "##QUERYPARAMS##", query)
            return self.wout(s)

        # Generate random cs and exe filenames
        basename = generate_filename()
        csfname = "/tmp/%s.cs" % basename
        exename = "/tmp/%s.exe" % basename

        # if ttype == "console":
        # Console program
        if "jypeli" in ttype:
            # Jypeli game
            bmpname = "/tmp/%s.bmp" % basename
            pngname = "/cs/images/%s.png" % basename
        if "comtest" in ttype:
            # ComTest test cases
            testcs = "/tmp/%sTest.cs" % basename
            testdll = "/tmp/%sTest.dll" % basename

            # Unknown template
            # self.wfile.write(("Invalid project type given (type=" + ttype + ")").encode())
            # return


        # Check query parameters
        p0 = FileParams(query, "", "")
        print("p0=")
        print(p0.replace)
        if p0.url == "" and p0.replace == "": p0.replace = "XXXX";

        print_file = get_param(query, "print", "")
        print("type=" + ttype)

        s = ""
        # if p0.url != "":
        s = get_file_to_output(query, False and print_file)

        # Open the file and write it
        if print_file: return self.wout(s)

        # /answer-path comes here
        usercode = get_json_param(query.jso, "input", "usercode", None)
        save["usercode"] = usercode
        result["save"] = save

        if not s.startswith("File not found"): codecs.open(csfname, "w", "utf-8").write(s)

        if not os.path.isfile(csfname) or os.path.getsize(csfname) == 0:
            return write_json_error(self.wfile, "Could not get the source file", result)
            # self.wfile.write("Could not get the source file\n")
            # print "=== Could not get the source file"

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
            error_str = error_str.replace(csfname, "tmp.cs")
            output = io.StringIO()
            file = codecs.open(csfname, 'r', "utf-8")
            lines = file.read().splitlines()
            file.close()
            print_lines(output, lines, 0, 10000)
            error_str += output.getvalue()
            output.close()

            self.remove(csfname)
            return write_json_error(self.wfile, error_str, result)

        lang = ""
        plang = get_param(query, "lang", "")
        env = dict(os.environ)
        if plang:
            if plang.find("fi") == 0: lang = "fi_FI.UTF-8"
            if plang.find("en") == 0: lang = "en_US.UTF-8"

        if lang:
            env["LANG"] = lang
            env["LC_ALL"] = lang
        print("Lang= ", lang)

        if ttype == "jypeli":
            code, out, err = run(["mono", exename, bmpname], timeout=10, env=env)
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
            code, out, err = run(["nunit-console", "-nologo", "-nodots", testdll], timeout=10, env=env)
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
                    # # out += "\n" + str(lnro) + " " + lines[lnro - 1]
                    web["comtestError"] = str(lnro) + " " + lines[lnro - 1]
            self.remove(testcs)
            self.remove(testdll)
        else:
            print("Exe: ", exename)
            code, out, err = run(["mono", exename], timeout=5, env=env)
            print(code, out, err)
            # if type(out) != type(''): out = out.decode()
            if out and out[0] in [254, 255]:
                out = out.decode('UTF16')
            elif type(out) != type(''):
                out = out.decode('utf-8-sig')
            if code == -9: out = "Runtime exceeded, maybe loop forever\n" + out
            # self.remove(exename)

        out = out[0:2000]
        web["console"] = out

        result["web"] = web

        # Clean up
        print("FILE NAME:", csfname)
        # self.remove(csfname)

        # self.wfile.write(out)
        # self.wfile.write(err)
        sresult = json.dumps(result)
        self.wout(sresult)
        print("Result ========")
        print(sresult)
        # print(out)
        print(err)


def keep_running():
    return True


# run_while_true(handler_class=TIMServer)

ThreadingServer(('', PORT), TIMServer).serve_forever()