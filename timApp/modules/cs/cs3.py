# -*- coding: utf-8 -*-  
# -*- coding: utf-8 -*-
import threading

import http.server
import subprocess
# import nltk
import re
import os.path
import os
import uuid
import io
import codecs
import binascii
import shutil
import shlex
from os import kill
import signal
import socketserver
# from signal import alarm, signal, SIGALRM, SIGKILL
from subprocess import PIPE, Popen, check_output
from fileParams3 import *
#from requests import Request, Session
from http import cookies
import datetime
import stat  
import sys  

PORT = 5000


def generate_filename():
    return str(uuid.uuid4())


def run(args, cwd=None, shell=False, kill_tree=True, timeout=-1, env=None, stdin=None, uargs =None):
    s_in = None
    if uargs: args.extend(shlex.split(uargs))
    if stdin: s_in = PIPE
    p = Popen(args, shell=shell, cwd=cwd, stdout=PIPE, stderr=PIPE, env=env, stdin=s_in)  # , timeout=timeout)
    try:
        if stdin:
            print(stdin)
            file = codecs.open(stdin, 'r', "utf-8")
            lines = file.read()
            print("Input ======")
            print(lines)
            print("===========")
            file.close()       
            # p.stdin.write(str.encode(lines))
            # p.stdin.close()
            stdout, stderr = p.communicate(str.encode(lines),timeout=timeout)
        else:     
            stdout, stderr = p.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        return -9, '', ''
    except IOError as e:    
        return -2, '', 'IO Error'.encode()
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
    # print("=================================== WHAT ==============")
    # print(what, " ", s)
    i = s.find(what)
    if i < 0: return s
    s = s[i + 1:]
    i = s.find("\n")
    if i < 0: return ""
    return s[i + 1:]


def get_html(ttype, query):
    js = query_params_to_map_check_parts(query)
    if "byFile" in js and not "byCode" in js:
        js["byCode"] = get_url_lines_as_string(js["byFile"])
    jso = json.dumps(js)
    print(jso)
    runner = 'cs-runner'
    #print(ttype)
    is_input = '';
    if "input" in ttype or "args" in ttype: is_input = '-input'
    if "comtest" in ttype or "junit" in ttype: runner = 'cs-comtest-runner'
    if "tauno" in ttype: runner = 'cs-tauno-runner'
    if "jypeli" in ttype: runner = 'cs-jypeli-runner'
    r = runner + is_input
    s = '<' + r + '>xxxJSONxxx' + jso + '</' + r + '>'
    # print(s)
    if ttype == "c1" or True:  # c1 oli testejä varten ettei sinä aikana rikota muita.
        hx = binascii.hexlify(jso.encode("UTF8"))
        s = '<' + r + '>xxxHEXJSONxxx' + hx.decode() + '</' + r + '>'
    return s


def log(self):
    t = datetime.datetime.now()
    agent = " :AG: " + self.headers["User-Agent"]
    if agent.find("ython") >= 0: agent = ""
    logfile = "/cs/images/log.txt"
    try:
        open(logfile, 'a').write(t.isoformat(' ') + ": " + self.path + agent + " u:" + self.user_id + "\n")
    except:
        return

    return


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
        is_tauno = self.path.find('/tauno') >= 0
        htmls = []
        for query in querys:
            print(query.jso);
            usercode = get_json_param(query.jso, "state", "usercode", None)
            if usercode: query.query["usercode"] = [usercode]
            userinput = get_json_param(query.jso, "state", "userinput", None)
            if userinput: query.query["userinput"] = [userinput]
            userargs = get_json_param(query.jso, "state", "userargs", None)
            if userargs: query.query["userargs"] = [userargs]
            ttype = get_param(query, "type", "console").lower()
            if is_tauno: ttype = 'tauno'
            s = get_html(ttype, query)
            # print(s)
            htmls.append(s)

        # print(htmls)
        sresult = json.dumps(htmls)
        self.wout(sresult)


    def do_PUT(self):
        # print("do_PUT =================================================")
        self.do_all(post_params(self))

    def wout(self, s):
        self.wfile.write(s.encode("UTF-8"))

    def remove(self, fname):
        try:
            os.remove(fname)
        except:
            return

    def mkdirs(self, path):
        if os.path.exists(path): return
        os.makedirs(path)


    def removedir(self, dirname):
        try:
            # os.chdir('/tmp')
            shutil.rmtree(dirname)
        except:
            return


    def do_all(self, query):
        print(threading.currentThread().getName())
        result = {}  # query.jso
        if not result: result = {}
        save = {}
        web = {}
        result["web"] = web

        # print("doAll ===================================================")
        # print(self.path)
        # print(self.headers)
        # print query

        if self.path.find('/favicon.ico') >= 0:
            self.send_response(404)
            return

        user_id = get_param(query, "user_id", None)
        log(self)
        '''
        if self.path.find('/login') >= 0:
            username = check_korppi_user(self,"tim")
            if not username: return 
            
            self.send_response(200)
            content_type = 'text/plain'
            self.send_header('Content-type', content_type)
            self.end_headers()
            self.wout("Username = " + username)
            return
        '''

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

        content_type = 'text/plain'
        if is_reqs or is_answer: content_type = "application/json"
        if is_fullhtml or is_html or is_ptauno or is_tauno: content_type = 'text/html; charset=utf-8'
        if is_css: content_type = 'text/css'
        if is_js: content_type = 'application/javascript'
        do_headers(self, content_type)

        if self.path.find("refresh") >= 0:
            self.wout(get_chache_keys())
            clear_cache()
            return

        if is_ptauno:
            # print("PTAUNO: " + content_type)
            p = self.path.split("?")
            self.wout(file_to_string(p[0]))
            return

            # Get the template type
        ttype = get_param(query, "type", "console").lower()
        if is_tauno and not is_answer: ttype = 'tauno'  # answer is newer tauno

        if is_reqs:
            result_json = {"js": ["http://tim-beta.it.jyu.fi/cs/js/dir.js"], "angularModule": ["csApp"],
                           "css": ["http://tim-beta.it.jyu.fi/cs/css/cs.css"], "multihtml": True}
            # result_json = {"js": ["cs/js/dir.js"], "angularModule": ["csApp"],
            #              "css": ["cs/css/cs.css"], "multihtml": True}
            #result_json = {"js": ["js/dir.js"], "angularModule": ["csApp"],
            #               "css": ["css/cs.css"]}
            result_str = json.dumps(result_json)
            return self.wout(result_str)

        if is_tauno and not is_answer:
            # print("PTAUNO: " + content_type)
            p = self.path.split("?")
            self.wout(file_to_string(p[0]))
            return


        # if ( query.jso != None and query.jso.has_key("state") and query.jso["state"].has_key("usercode") ):
        usercode = get_json_param(query.jso, "state", "usercode", None)
        if usercode: query.query["usercode"] = [usercode]
        userinput = get_json_param(query.jso, "state", "userinput", None)
        if userinput: query.query["userinput"] = [userinput]

        # print("USERCODE: XXXXX = ", usercode)

        # print("Muutos ========")
        # pprint(query.__dict__, indent=2)

        if is_css:
            return self.wout(file_to_string('cs.css'))

        if is_js:
            if self.path.find('rikki') >= 0:
                return self.wout(file_to_string('js/dirRikki.js'))
            return self.wout(file_to_string('js/dir.js'))

        if is_html and not is_iframe:
            # print("HTML:==============")
            s = get_html(ttype, query)
            # print(s)
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

        # Check if user name or temp name
        rndname = generate_filename()
        delete_tmp = True
        if get_param(query, "path", "") == "user" and user_id:
            basename = "user/" + hash_user_dir(user_id)
            delete_tmp = False
            self.mkdirs("/tmp/user")
        else:
            # Generate random cs and exe filenames
            basename = rndname
        filename = get_param(query, "filename", "prg")
        ifilename = get_param(query, "inputfilename", "input.txt")
        # csfname = "/tmp/%s.cs" % basename
        # exename = "/tmp/%s.exe" % basename
        csfname = "/tmp/%s/%s.cs" % (basename, filename)
        exename = "/tmp/%s/%s.exe" % (basename, filename)
        inputfilename = "/tmp/%s/%s" % (basename, ifilename)
        prgpath = "/tmp/%s" % basename
        filepath = prgpath

        # if ttype == "console":
        # Console program
        if ttype == "cc":
            csfname = "/tmp/%s/%s.c" % (basename, filename)

        if ttype == "c++":
            csfname = "/tmp/%s/%s.cpp" % (basename, filename)

        if ttype == "fs":
            csfname = "/tmp/%s/%s.fs" % (basename, filename)

        if ttype == "py":
            csfname = "/tmp/%s/%s.py" % (basename, filename)
            exename = csfname

        if ttype == "shell":
            csfname = "/tmp/%s/%s" % (basename, filename)
            exename = csfname

        if ttype == "jjs":
            csfname = "/tmp/%s/%s.js" % (basename, filename)
            exename = csfname

        if ttype == "clisp":
            csfname = "/tmp/%s/%s.lisp" % (basename, filename)
            exename = csfname

        if "jypeli" in ttype:
            # Jypeli game
            csfname = "/tmp/%s.cs" % basename
            exename = "/tmp/%s.exe" % basename
            bmpname = "/tmp/%s.bmp" % rndname
            pngname = "/cs/images/%s.png" % rndname
        if "comtest" in ttype:
            # ComTest test cases
            testcs = "/tmp/%s/%sTest.cs" % (basename, filename)
            testdll = "/tmp/%s/%sTest.dll" % (basename, filename)
        if "ccomtest" in ttype:
            # ComTest test cases
            csfname = "/tmp/%s/%s.cpp" % (basename, filename)
            testcs = "/tmp/%s/%s.cpp" % (basename, filename)
        if "text" in ttype:
            #text file
            csfname = "/tmp/%s/%s.txt" % (basename, filename)

            # Unknown template
            # self.wfile.write(("Invalid project type given (type=" + ttype + ")").encode())
            # return


        # Check query parameters
        p0 = FileParams(query, "", "")
        # print("p0=")
        # print(p0.replace)
        if p0.url == "" and p0.replace == "": p0.replace = "XXXX";

        print_file = get_param(query, "print", "")
        # print("type=" + ttype)

        s = ""
        # if p0.url != "":
        #
        if p0.breakCount > 0:
            parts = get_file_parts_to_output(query,False)
            print(parts)
            if print_file == "2": return self.wout(json.dumps(parts))
            s = join_file_parts(p0, parts)
        else:
            s = get_file_to_output(query, False and print_file)

        # Open the file and write it
        if print_file: return self.wout(s)

        self.mkdirs(prgpath)
        # os.chdir(prgpath)

        # /answer-path comes here
        usercode = get_json_param(query.jso, "input", "usercode", None)
        if usercode: save["usercode"] = usercode
        userinput = get_json_param(query.jso, "input", "userinput", None)
        if userinput: save["userinput"] = userinput
        userargs = get_json_param(query.jso, "input", "userargs", None)
        if userargs: save["userargs"] = userargs
        
        result["save"] = save

        if "java" in ttype or "jcomtest" in ttype or "junit" in ttype or "graphics" in ttype:
            # java
            package, classname = find_java_package(s);
            javaclassname = classname
            if package:
                filepath = prgpath + "/" + package.replace(".", "/")
                self.mkdirs(filepath)
                javaclassname = package + "." + classname
            javaname = filepath + "/" + classname + ".java"
            csfname = javaname
            bmpname = "/tmp/%s.bmp" % basename
            pngname = "/cs/images/%s.png" % basename
            # print("TYYPPI = " + ttype + " nimi = " + csfname + " class = " + javaclassname)

        if "jcomtest" in ttype:
            # ComTest test cases
            testcs = filepath + "/" + classname + "Test.java"
            testdll = javaclassname + "Test"

        if not s.startswith("File not found"):
            print("Write file: " + csfname)
            codecs.open(csfname, "w", "utf-8").write(s)

        isInput = get_json_param(query.jso, "input", "isInput", None)
        print(isInput)
        if isInput:
            print("Write input file: " + inputfilename)
            codecs.open(inputfilename, "w", "utf-8").write(userinput)

        if not os.path.isfile(csfname) or os.path.getsize(csfname) == 0:
            return write_json_error(self.wfile, "Could not get the source file", result)
            # self.wfile.write("Could not get the source file\n")
            # print "=== Could not get the source file"

        # print(ttype)
        # Compile
        try:
            log(self)

            if ttype == "jypeli":
                cmdline = "mcs /out:%s /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/Jypeli.MonoGame.Framework.dll /r:/cs/jypeli/Jypeli.Physics2d.dll /r:/cs/jypeli/OpenTK.dll /r:/cs/jypeli/Tao.Sdl.dll /r:System.Drawing /cs/jypeli/Ohjelma.cs /cs/jypeli/Screencap.cs %s" % (
                    exename, csfname)
            elif ttype == "comtest":
                cmdline = "java -jar /tmp/ComTest.jar nunit %s && mcs /out:%s /target:library /reference:/usr/lib/mono/gac/nunit.framework/2.6.0.0__96d09a1eb7f44a77/nunit.framework.dll %s %s" % (
                    csfname, testdll, csfname, testcs)
            elif ttype == "jcomtest":
                cmdline = "java comtest.ComTest %s && javac %s %s" % (csfname, csfname, testcs)
            elif ttype == "junit":
                cmdline = "javac %s" % javaname
            elif ttype == "java" or ttype == "graphics":
                cmdline = "javac -Xlint:all %s" % javaname
            elif ttype == "cc":
                cmdline = "gcc -Wall %s -o %s" % (csfname, exename)
            elif ttype == "c++":
                cmdline = "g++ -std=c++11 -Wall %s -o %s" % (csfname, exename)
            elif ttype == "py":
                cmdline = ""
            elif ttype == "clisp":
                cmdline = ""
            elif ttype == "ccomtest":
                cmdline = ""
            elif ttype == "text":
                cmdline = ""
            elif ttype == "shell":
                cmdline = ""
            elif ttype == "jjs":
                cmdline = ""
            elif ttype == "fs":
                cmdline = "fsharpc --out:%s %s" % (exename, csfname)
            else:
                cmdline = "mcs /out:%s %s" % (exename, csfname)

            compiler_output = ""
            if cmdline:
                compiler_output = check_output(["cd " + prgpath + " && " + cmdline], stderr=subprocess.STDOUT, shell=True).decode("utf-8")
                compiler_output = compiler_output.replace(prgpath, "")
            # self.wfile.write("*** Success!\n")
            print("*** Compile Success")
            print(compiler_output)
        except subprocess.CalledProcessError as e:
            '''
            self.wout("!!! Error code " + str(e.returncode) + "\n")
            self.wout(e.output)
            file = open(csfname, 'r')
            lines = file.read().splitlines()
            # self.wfile.write(file.read())
            printLines(self.wfile,lines,0,10000)
            '''
            print("directory = " + os.curdir)
            error_str = "!!! Error code " + str(e.returncode) + "\n"
            error_str += e.output.decode("utf-8") + "\n"
            # errorStr = re.sub("^/tmp/.*cs\(\n", "tmp.cs(", errorStr, flags=re.M)
            error_str = error_str.replace(prgpath, "")
            output = io.StringIO()
            file = codecs.open(csfname, 'r', "utf-8")
            lines = file.read().splitlines()
            file.close()
            if not get_param(query, "nocode", False):
                print_lines(output, lines, 0, 10000)
            error_str += output.getvalue()
            output.close()

            if delete_tmp: self.removedir(prgpath)
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
        # print("Lang= ", lang)

        err = ""
        code = 0
        out = ""

        stdin = get_param(query, "stdin", None)
        if stdin: stdin = "/tmp/%s/%s" % (basename, stdin)
        
        if ttype == "jypeli":
            code, out, err = run(["mono", exename, bmpname], cwd=prgpath, timeout=10, env=env)
            print(err)
            if type(out) != type(''): out = out.decode()
            if type(err) != type(''): err = err.decode()
            err = ""
            run(["convert", "-flip", bmpname, pngname], cwd=prgpath, timeout=20)
            # print(bmpname, pngname)
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
                # web["image"] = "http://tim-beta.it.jyu.fi/csimages/" + basename + ".png"
                web["image"] = "/csimages/" + basename + ".png"
            if delete_tmp:
                self.remove(csfname)
                self.remove(exename)
        elif ttype == "graphics":
            # code, out, err = run(["mono", exename, bmpname], timeout=10, env=env)
            code, out, err = run(["java", javaclassname, bmpname], cwd=prgpath, timeout=30, env=env)
            print(err)
            if type(out) != type(''): out = out.decode()
            if type(err) != type(''): err = err.decode()
            # err = ""
            run(["convert", "-flip", bmpname, pngname], cwd=prgpath, timeout=20)
            # print(bmpname, pngname)
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
                # web["image"] = "http://tim-beta.it.jyu.fi/csimages/" + basename + ".png"
                web["image"] = "/csimages/" + basename + ".png"
        elif ttype == "comtest":
            eri = -1
            code, out, err = run(["nunit-console", "-nologo", "-nodots", testdll], cwd=prgpath, timeout=10, env=env)
            if type(out) != type(''): out = out.decode()
            if type(err) != type(''): err = err.decode()
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
        elif ttype == "jcomtest" or ttype == "ccomtest" or ttype == "junit":
            eri = -1
            linenr_end = " "
            if ttype == "jcomtest":
                code, out, err = run(["java", "org.junit.runner.JUnitCore", testdll], cwd=prgpath, timeout=10, env=env)
            if ttype == "junit":
                code, out, err = run(["java", "org.junit.runner.JUnitCore", javaclassname], cwd=prgpath, timeout=10, env=env)
            if ttype == "ccomtest":
                code, out, err = run(["java", "-jar", "/cs/java/comtestcpp.jar", "-nq", testcs], cwd=prgpath, timeout=10, env=env)
                linenr_end = ":"
            if type(out) != type(''): out = out.decode()
            if type(err) != type(''): err = err.decode()
            print(code, out, err)
            out = remove_before("Execution Runtime:", out)
            if code == -9:
                out = "Runtime exceeded, maybe loop forever\n" + out
                eri = 0
            # print(javaclassname+"\n")    
            if ttype == "junit": out = re.sub("[\t ]*at " + javaclassname, "ERROR: " + javaclassname, out,
                                              flags=re.M)  # prevent remove by next "at"-word
            out = re.sub("\s+at .*\n", "\n", out, flags=re.M)
            out = re.sub("\n+", "\n", out, flags=re.M)
            out = re.sub("Errors and Failures.*\n", "", out, flags=re.M)
            out = re.sub(prgpath + "/", "", out, flags=re.M)
            out = out.strip(' \t\n\r')
            if ttype == "junit": out = re.sub("java:", "java line: ", out,
                                              flags=re.M)  # To get line: also in JUnit case where error is in format java:39
            if eri < 0: eri = out.find("FAILURES")  # jcomtest
            if eri < 0: eri = out.find("Test error")  # ccomtest
            if eri < 0: eri = out.find("ERROR:")  # ccomtest compile error
            web["testGreen"] = True
            if eri >= 0:
                web["testGreen"] = False
                web["testRed"] = True
                lni = out.find(" line: ")
                cterr = "";
                sep = "";
                while lni >= 0:
                    lns = out[lni + 7:]
                    lnro = getint(lns)
                    lines = codecs.open(csfname, "r", "utf-8").readlines()
                    # print("Line nr: "+str(lnro))
                    # # out += "\n" + str(lnro) + " " + lines[lnro - 1]
                    cterr += sep + str(lnro) + " " + lines[lnro - 1]
                    sep = "";
                    lni = out.find(" line: ", lni + 8)
                web["comtestError"] = cterr
            else:
                out = re.sub("^JUnit version.*\n", "", out, flags=re.M)
                out = re.sub("^Time: .*\n", "", out, flags=re.M)
                out = re.sub("^.*prg.*cpp.*\n", "", out, flags=re.M)
                out = re.sub("^ok$", "", out, flags=re.M)

        else:
            if ttype == "java":
                print("java: ", javaclassname)
                # code, out, err = run(["java" ,"-cp",prgpath, javaclassname], timeout=10, env=env, uargs = userargs)
                code, out, err = run(["java", javaclassname], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
            elif ttype == "shell":
                print("shell: ", exename)
                #os.chmod(exename, stat.S_IEXEC)
                os.system("chmod +x "+exename)
                try:
                    code, out, err = run([exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
                except OSError as e:
                    print(e)
                    code, out, err = (-1, "",str(e).encode())
            elif ttype == "jjs":
                print("jjs: ", exename)
                code, out, err = run(["jjs",exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
            elif ttype == "cc":
                print("c: ", exename)
                code, out, err = run([exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
            elif ttype == "c++":
                print("c++: ", exename)
                code, out, err = run([exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
            elif ttype == "py":
                print("py: ", exename)
                code, out, err = run(["python3", exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
            elif ttype == "clisp":
                print("clips: ", exename)
                code, out, err = run(["sbcl", "--script", exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
                # code, out, err = run(["sbcl", "--noinform --load " + exename + " --eval '(SB-EXT:EXIT)'"], timeout=10, env=env)
                # code, out, err = run(["clisp",exename], timeout=10, env=env)
            elif ttype == "py2":
                print("py2: ", exename)
                code, out, err = run(["python2", exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
            elif ttype == "text":
                print("text: ", csfname)
                code, out, err = (0, "".encode("utf-8"),"tallennettu".encode("utf-8")) 
            else:
                print("Exe: ", exename)
                code, out, err = run(["mono", exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)

            print(code, out, err, compiler_output)
            if code == -9:
                out = "Runtime exceeded, maybe loop forever\n" + out

            else:
                print(err)
                err = err.decode("utf-8") + compiler_output
                if ttype == "fs":
                    err = err.replace(
                        "F# Compiler for F# 3.0 (Open Source Edition)\nFreely distributed under the Apache 2.0 Open Source License\n",
                        "")

                if type(err) != type(''): err = err.decode()
                # if type(out) != type(''): out = out.decode()
                if out and out[0] in [254, 255]:
                    out = out.decode('UTF16')
                elif type(out) != type(''):
                    out = out.decode('utf-8-sig')

        if delete_tmp: self.removedir(prgpath)

        out = out[0:8000]
        web["console"] = out
        web["error"] = err

        result["web"] = web

        # Clean up
        # print("FILE NAME:", csfname)
        # self.remove(csfname)

        # self.wfile.write(out)
        # self.wfile.write(err)
        sresult = json.dumps(result)
        self.wout(sresult)
        # print("Result ========")
        print(sresult)
        # print(out)
        # print(err)


# Kun debuggaa Windowsissa, pitää vaihtaa ThreadingMixIn
# Jos ajaa Linuxissa ThreadingMixIn, niin chdir vaihtaa kaikkien hakemistoa?
# Ongelmaa korjattu siten, että kaikki run-kommennot saavat prgpathin käyttöönsä

#if __debug__:
if True:
    class ThreadedHTTPServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
        """Handle requests in a separate thread."""
    print("Debug mode/ThreadingMixIn")
#else:
#    class ThreadedHTTPServer(socketserver.ForkingMixIn, http.server.HTTPServer):
#        """Handle requests in a separate thread."""
#    print("Normal mode/ForkingMixIn")


if __name__ == '__main__':
    server = ThreadedHTTPServer(('', PORT), TIMServer)
    print('Starting server, use <Ctrl-C> to stop')
    server.serve_forever()
