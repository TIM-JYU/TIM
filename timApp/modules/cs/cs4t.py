import BaseHTTPServer
import subprocess 
import cStringIO
# import nltk
from urllib import urlopen, urlencode
import re
from urlparse import urlparse, parse_qs
import os.path
import uuid
from os import kill
from signal import alarm, signal, SIGALRM, SIGKILL
from subprocess import PIPE, Popen, check_output
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


def generate_filename():
    return str(uuid.uuid4())


def run(args, cwd=None, shell=False, kill_tree=True, timeout=-1, env=None):
    class Alarm(Exception):
        pass


    def alarm_handler(signum, frame):
        raise Alarm

    p = Popen(args, shell=shell, cwd=cwd, stdout=PIPE, stderr=PIPE, env=env)
    if timeout != -1:
        signal(SIGALRM, alarm_handler)
        alarm(timeout)
    try:
        stdout, stderr = p.communicate()
        if timeout != -1:
            alarm(0)
    except Alarm:
        pids = [p.pid]
        if kill_tree:
            pids.extend(get_process_children(p.pid))
        for pid in pids:
            # process might have died before getting to this line
            # so wrap to avoid OSError: no such process
            try:
                kill(pid, SIGKILL)
            except OSError:
                pass
        return -9, '', ''
    return p.returncode, stdout, stderr


def get_process_children(pid):
    p = Popen('ps --no-headers -o pid --ppid %d' % pid, shell=True,
              stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    return [int(p) for p in stdout.split()]


def printFileTo(name, f):
    fr = open(name, "r")
    lines = fr.readlines()
    for i in range(0, len(lines)):
        line = lines[i]
        f.write(line)
    fr.close()


def queryParamsToNG(query):
    result = ""
    for field in query.keys():
        result = result + field + "=\"" + query[field][0] + "\";\n"
    # print "QUERY" + str(query)
    return result


def queryParamsToAttribute(query, leaveAway):
    result = ""
    print
    "leaveAway " + leaveAway
    for field in query.keys():
        if ( not (leaveAway and field == leaveAway) ):
            result = result + field.lower() + "=\'" + query[field][0] + "\'\n"
    # print "QUERY" + str(query)
    return result + "";


def queryParamsToMap(query):
    result = {}
    for field in query.keys():
        result[field] = query[field][0]
    return result


def printFileToReplaceNG(name, f, whatToReplace, query):
    fr = open(name, "r")
    lines = fr.readlines()
    params = queryParamsToNG(query.query)
    for i in range(0, len(lines)):
        line = lines[i].replace(whatToReplace, params)
        f.write(line)
    fr.close()


def printFileToReplaceURL(name, f, whatToReplace, query):
    fr = open(name, "r")
    lines = fr.readlines()
    # params = queryParamsToURL(query)
    map = queryParamsToMap(query.query)
    params = urlencode(map)
    for i in range(0, len(lines)):
        line = lines[i].replace(whatToReplace, params)
        f.write(line)
    fr.close()


def printStringToReplaceURL(line, f, whatToReplace, query):
    map = queryParamsToMap(query.query)
    params = urlencode(map)
    line = line.replace(whatToReplace, params)
    height = get_param(query, "height", "100%")
    line = line.replace('##HEIGHT##', height)
    f.write(line)


def printFileToReplaceAttribute(name, f, whatToReplace, query):
    fr = open(name, "r")
    lines = fr.readlines()
    params = queryParamsToAttribute(query.query)
    for i in range(0, len(lines)):
        line = lines[i].replace(whatToReplace, params)
        f.write(line)
    fr.close()


def printStringToReplaceAttribute(line, f, whatToReplace, query):
    leaveAway = None;
    if ( line.index("##USERCODE##") >= 0 ): leaveAway = "byCode"
    params = queryParamsToAttribute(query.query, leaveAway)
    line = line.replace(whatToReplace, params)
    line = line.replace("##USERCODE##", get_param(query, "byCode", ""))
    f.write(line)
    print(line)


def printLines(file, lines, n1, n2):
    linefmt = "{0:03d} "
    n = len(lines)
    if n1 < 0:  n1 = 0
    if n2 >= n:    n2 = n - 1

    ni = 0
    for i in range(n1, n2 + 1):
        line = lines[i]
        ln = linefmt.format(i + 1)
        file.write(ln + line + "\n")


def jsOnError(file, err):
    result = {"web": {"error": err}}
    resultStr = json.dumps(result)
    file.write(resultStr);
    print
    "ERROR:======== " + err

	
def remove_before(what,s):
	i = s.find(what)
	if i < 0: return s
	s = s[i+1:]
	i = s.find("\n")
	if i < 0: return ""
	return s[i+1:]
	

class TIMServer(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_OPTIONS(self):
        print
        "do_OPTIONS =============================================="
        self.send_response(200, "ok")
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
        print
        self.path
        print
        self.headers

    def do_GET(self):
        print
        "do_GET =================================================="
        self.doAll(getParams(self))

    def do_POST(self):
        print
        "do_POST ================================================="
        self.doAll(postParams(self))

    def do_PUT(self):
        print
        "do_PUT ================================================="
        self.doAll(postParams(self))


    def doAll(self, query):
        result = query.jso
        if ( not result ): result = {}
        web = {}
        result["web"] = web

        print
        "doAll ==================================================="
        print
        self.path
        print
        self.headers
        # print query

        fullhtml = self.path.find('/fullhtml') >= 0
        html = self.path.find('/html') >= 0
        css = self.path.find('/css') >= 0
        js = self.path.find('/js') >= 0
        reqs = self.path.find('/reqs') >= 0
        iframeParam = get_param_del(query, "iframe", "")
        iframe = (self.path.find('/iframe') >= 0) or ( iframeParam )
        answer = self.path.find('/answer') >= 0


        # korjaus kunnes byCode parametri tulee kokonaisena
        #tempBy = get_param(query, "b", "")
        #if ( tempBy ):
        #	query["byCode"] = [tempBy];

        # print query

        self.send_response(200)
        # self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With, Content-Type")
        type = 'text/plain'
        if ( reqs ): type = "application/json" 
        if ( fullhtml or html ): type = 'text/html'
        if ( css ): type = 'text/css'
        if ( js or answer ): type = 'application/javascript'
        self.send_header('Content-type', type)
        self.end_headers()
        # Get the template type
        ttype = get_param(query, "type", "console").lower()

        if ( reqs ):
            resultJSON = {"js": ["js/dir.js"], "angularModule": ["csApp"], "css": ["css/cs.css"]}
            resultStr = json.dumps(resultJSON)
            self.wfile.write(resultStr);
            return

        # if ( query.jso != None and query.jso.has_key("state") and query.jso["state"].has_key("usercode") ):
        usercode = getJSOParam(query.jso, "state", "usercode", None)
        if ( usercode ): query.query["usercode"] = [usercode]

        print
        "Muutos ========"
        pprint(query.__dict__, indent=2)

        if ( css ):
            printFileTo('cs.css', self.wfile)
            return
        if ( js ):
            if self.path.find('rikki') >= 0 : 
                printFileTo('js/dirRikki.js', self.wfile)
            printFileTo('js/dir.js', self.wfile)
            return
        if ( html and not iframe ):
            print
            "HTML:=============="
            if ( ttype == "console" ):
                printStringToReplaceAttribute('<cs-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-runner>', self.wfile,
                                              "##QUERYPARAMS##", query)
            elif ( ttype == "comtest" ):
                printStringToReplaceAttribute('<cs-comtest-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-runner>',
                                              self.wfile, "##QUERYPARAMS##", query)
            else:
                printStringToReplaceAttribute('<cs-jypeli-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-jypeli-runner>',
                                              self.wfile, "##QUERYPARAMS##", query)
            return
        if ( fullhtml ):
            printFileTo('begin.html', self.wfile)
            if ( ttype == "console" ):
                printStringToReplaceAttribute('<cs-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-runner>', self.wfile,
                                              "##QUERYPARAMS##", query)
            elif ( ttype == "comtest" ):
                printStringToReplaceAttribute('<cs-comtest-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-runner>',
                                              self.wfile, "##QUERYPARAMS##", query)
            else:
                printStringToReplaceAttribute('<cs-jypeli-runner \n##QUERYPARAMS##\n>##USERCODE##</cs-jypeli-runner>',
                                              self.wfile, "##QUERYPARAMS##", query)
            printFileTo('end.html', self.wfile)
            return
        if ( iframe ):
            printStringToReplaceURL(
                '<iframe frameborder="0"  src="http://tim-beta.it.jyu.fi/cs/fullhtml?##QUERYPARAMS##" style="overflow:hidden;height:##HEIGHT##;width:100%"  seamless></iframe>',
                self.wfile, "##QUERYPARAMS##", query)
            return

        # Generate random cs and exe filenames
        basename = generate_filename()
        csfname = "/tmp/%s.cs" % (basename)
        exename = "/tmp/%s.exe" % (basename)

        # Check query parameters
        p0 = FileParams(query, "", "")
        print
        "p0="
        print
        p0.replace
        if ( p0.url == "" and p0.replace == "" ):
            self.wfile.write("Must give file= -parameter")
            return

        printFile = get_param(query, "print", "")
        print
        "type=" + ttype

        if ( ttype == "console" ):
            # Console program
            pass
        elif ( ttype == "jypeli" ):
            # Jypeli game
            bmpname = "/tmp/%s.bmp" % (basename)
            pngname = "/cs/images/%s.png" % (basename)
            pass
        elif ( ttype == "comtest" ):
            # ComTest test cases
            testcs = "/tmp/%sTest.cs" % (basename)
            testdll = "/tmp/%sTest.dll" % (basename)
        else:
            # Unknown template
            self.wfile.write("Invalid project type given (type=" + ttype + ")")
            return

        # Open the file and write it
        if ( printFile ):
            csfile = self.wfile
        else:
            csfile = open(csfname, "w")
        p0.printFile(csfile)
        p0.printInclude(csfile)
        u = p0.url;
        for i in range(1, 10):
            p = FileParams(query, str(i), u)
            p.printFile(csfile)
            p.printInclude(csfile)
            if ( p.url ): u = p.url

        if ( printFile ): return

        csfile.close()
        if not os.path.isfile(csfname) or os.path.getsize(csfname) == 0:
            jsOnError(self.wfile, "Could not get the source file");
            # self.wfile.write("Could not get the source file\n")
            # print "=== Could not get the source file"
            return

        # Compile
        try:
            if ( ttype == "jypeli" ):
                cmdline = "mcs /out:%s /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/Jypeli.MonoGame.Framework.dll /r:/cs/jypeli/Jypeli.Physics2d.dll /r:/cs/jypeli/OpenTK.dll /r:/cs/jypeli/Tao.Sdl.dll /r:System.Drawing /cs/jypeli/Ohjelma.cs /cs/jypeli/Screencap.cs %s" % (
                    exename, csfname)
            elif ( ttype == "comtest" ):
                cmdline = "java -jar /tmp/ComTest.jar nunit %s && mcs /out:%s /target:library /reference:/usr/lib/mono/gac/nunit.framework/2.6.0.0__96d09a1eb7f44a77/nunit.framework.dll %s %s" % (
                    csfname, testdll, csfname, testcs)
            else:
                cmdline = "mcs /out:%s %s" % (exename, csfname)

            check_output([cmdline], stderr=subprocess.STDOUT, shell=True)
            # self.wfile.write("*** Success!\n")
            print
            "*** Success"
        except subprocess.CalledProcessError as e:
            '''
			self.wfile.write("!!! Error code " + str(e.returncode) + "\n" )
			self.wfile.write(e.output)
			file = open(csfname, 'r')
			lines = file.read().splitlines()
			# self.wfile.write(file.read())
			printLines(self.wfile,lines,0,10000)
			'''
            errorStr = "!!! Error code " + str(e.returncode) + "\n"
            errorStr += e.output
            output = cStringIO.StringIO()
            file = open(csfname, 'r')
            lines = file.read().splitlines()
            file.close()
            printLines(output, lines, 0, 10000)
            errorStr += output.getvalue()
            output.close()

            os.remove(csfname)
            jsOnError(self.wfile, errorStr)
            return

        if ( ttype == "jypeli" ):
            code, out, err = run(["mono", exename, bmpname], timeout=10)
            run(["convert", "-flip", bmpname, pngname])
            os.remove(bmpname)
            # self.wfile.write("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/%s.png\n" % (basename))
            print("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/%s.png\n" % (basename))
            # TODO: clean up screenshot directory
            web["image"] = "http://tim-beta.it.jyu.fi/csimages/" + basename + ".png"
            p = re.compile('Number of joysticks:.*\n.*')
            # out = out.replace("Number of joysticks:.*","")
            out = p.sub("", out)
            os.remove(exename)
        elif ( ttype == "comtest" ):
            code, out, err = run(["nunit-console", testdll], timeout=10)
            out = remove_before("Execution Runtime:",out)
            out = out[1:] # alussa oleva . pois
            out = re.sub("at .*","",out,flags=re.M)
            out = re.sub("\n\n+","",out,flags=re.M)
            eri = out.find("Test Failure");
            if eri >= 0:
                lni = out.find(", line ") 
                if ( lni >= 0 ): 
                    lns = out[lni+7:]
                    lns = lns[0:lns.find("\n")]
                    lnro = int(lns)
                    lines = open(csfname,"r").readlines()
                    # print("Line nr: "+str(lnro))
                    out += "\n" + str(lnro) + " " + lines[lnro-1]
            os.remove(testcs)
            os.remove(testdll)
        else:
            code, out, err = run(["mono", exename], timeout=10)
            os.remove(exename)

        web["console"] = out
        result["web"] = web

	# Clean up
        os.remove(csfname)

        # self.wfile.write(out)
        # self.wfile.write(err)
        sresult = json.dumps(result)
        self.wfile.write(sresult)
        print
        "Result ========"
        print
        sresult
        print
        out
        print
        err


def keep_running():
    return True


run_while_true(handler_class=TIMServer)

