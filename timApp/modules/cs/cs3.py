# -*- coding: utf-8 -*-
import threading
import time

import http.server
import subprocess
# import nltk
import os
import uuid
import io
import shutil
import shlex
import signal
import socketserver
# from signal import alarm, signal, SIGALRM, SIGKILL
from subprocess import PIPE, Popen, check_output
from fileParams3 import *
# from requests import Request, Session
import datetime
import cgi
import logging

# cs3.py: WWW-palvelin portista 5000 (ulospäin 56000) joka palvelee csPlugin pyyntöjä
#
# Ensin käynistettävä 
# ./startPlugins.sh             - käynnistää dockerin cs3.py varten
# ./startAll.sh                 - ajetaan dockerin sisällä cs3.py (ajetaan edellisestä)
# Muut tarvittavat skriptit:
# rcmd.sh          - käynistetään ajettavan ohjelman kontin sisälle ajamaan
# cs3.py tuottama skripti
#
# Hakemistot:
#  tim-koneessa
#     /opt/cs               - varsinainen csPluginin hakemisto, skirptit yms
#     /opt/cs/templates     - pluginin templatet editoria varten 
#     /opt/cs/java          - javan tarvitsemat tavarat
#     /opt/cs/images/cs     - kuvat jotka syntyvät csPlugin ajamista ohjelmista
#     /opt/cs/jypeli        - jypelin tarvitsemat tiedostot  
#     /tmp/uhome            - käyttäjän hakemistoja ohjelmien ajamisen ajan
#     /tmp/uhome/user       - käyttäjän hakemistoja ohjelmien ajamisen ajan
#     /tmp/uhome/user/HASH  - yhden käyttäjän hakemisto joka säilyy ajojen välillä
#     /tmp/uhone/run        - tänne kirjoitetaan käynnistyskomento demonia varten
#     /tmp/uhome/cs:        - c#-jypeli tiedostot
#
# tim-koneesta käynnistetään cs3 docker-kontti nimelle csPlugin (./startPlugins.sh), jossa
# mountataan em. hakemistoja seuraavasti:
#
#   /opt/cs  ->          /cs/          read only
#   /opt/cs/images/cs -> /csimages/    kuvat
#   /tmp/uhome:          /tmp/         käyttäjien hakemistot ja ajokomennot tänne
#   /tmp/uhome/cs:       /tmp/cs       c#-jypeli tiedostot, Jypeli ohjelmat myös laitetaan tänne
#   /tmp/uhome/user:     /tmp/user     käyttäjän alihakemistot
#
# Käyttäjistä (csPlugin-kontissa) tehdään /tmp/user/HASHCODE
# tai /tmp/HASHCODE nimiset hakemistot (USERPATH=user/HASHCODE tai HASHCODE), 
# joihin tehdään ohjelmien käännökset ja joista ohjelmat ajetaan.
# HASHCODE = käyttäjätunnuksesta muodostettu hakemiston nimi.  
# Mikäli käyttäjätunnusta ei ole, on tiedoston nimi satunnainen.
# 
# Kääntämisen jälkeen luodaan /tmp/USERPATH hakemistoon
# täydellinen ajokomento run/URNDFILE.sh
# Tämä jälkeen tehdään /tmp/run -hakemistoon tiedosto
# RNDNAME johon on kirjoitettu "USERPATH run/URNDFILE.sh" 
# Tähän ajokonttiin mountataan tim-koneesta
#
#   /opt/cs  ->            /cs/          read only
#   /tmp/uhome/USERPATH -> /home/me      käyttäjän "kotihakemisto"
#   /tmp/uhome/cs ->       /home/cs      c#-jypeli tiedostot
#
# Docker-kontin käynnistyessä suoritetaan komento /cs/rcmd.sh
# joka alustaa "näytön" ja luo sopivat ympäristömuuttajat mm. 
# javan polkuja varten ja sitten vaihtaa hakemistoon /home/me
# ja ajaa sieltä komennon ./run/URNDFILE.sh
# stdout ja stderr tulevat tiedostoihin ./run/URNDFILE.in ja ./run/URNDFILE.err
# Kun komento on ajettu, docker-kontti häviää.  Ajon lopuksi tuohotaan
# ./run/URNDFILE.sh
# ja kun tämä on hävinnyt, luetaan stdin ja stderr ja tiedot lähetetään
# selaimelle (Timin kautta)
#

PORT = 5000


def generate_filename():
    return str(uuid.uuid4())


def tquote(s):
    if s.startswith("$"): return s
    r = shlex.quote(s)
    if r.find("$") < 0: return r
    return r.replace("'",'"')     


def run(args, cwd=None, shell=False, kill_tree=True, timeout=-1, env=None, stdin=None, uargs=None, code="utf-8"):
    """
    Alkuperäinen ajaminen, jossa ajo suoritetaan tavallisen prosessina
    :type code: str
    :type kill_tree: bool
    """
    s_in = None
    if uargs and len(uargs): args.extend(shlex.split(uargs))
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
            stdout, stderr = p.communicate(str.encode(lines), timeout=timeout)
        else:
            stdout, stderr = p.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        return -9, '', ''
    except IOError as e:
        return -2, '', ('IO Error ' + str(e)).encode()
    return p.returncode, stdout, stderr


def run2(args, cwd=None, shell=False, kill_tree=True, timeout=-1, env=None, stdin=None, uargs=None, code="utf-8",
         extra="", ulimit=None):
    """
    Run that is done by opening a new docker instance to run the command.  A script rcmd.sh is needed
    to fullfill the run inside docker.
    :param args: run arguments for the command
    :param cwd: in whinch directory the command should start
    :param shell: maybe not needed any more???
    :param kill_tree: maybe not needed anymore
    :param timeout: how long the run is allowed to run
    :param env: environment varibales for run
    :param stdin: what file to use for stdin
    :param uargs: user arguments for the run
    :param code: which coding schema to use ("utf-8" is default)
    :param extra: extra command used for the run
    :return: error code, stdout text, stderr text
    """
    s_in = ""
    pwd = ""
    if not ulimit: ulimit = "ulimit -f 1000 -t 10 -s 100 "  # -v 2000 -s 100 -u 10
    if uargs and len(uargs): args.extend(shlex.split(uargs))
    if stdin: s_in = " <" + stdin
    mkdirs(cwd + "/run")
    tmpname = generate_filename()
    urndname = "run/" + tmpname  # ohjaustiedostojen nimet
    stdoutf = urndname + ".in"
    stderrf = urndname + ".err"
    cmdf = cwd + "/" + urndname + ".sh"  # varsinaisen ajoskriptin nimi
    cmnds = ' '.join(tquote(arg) for arg in args)  # otetaan args listan jonot yhteen
    # tehdään komentojono jossa suuntaukset
    cmnds = "#!/bin/bash\n" + ulimit + "\n" + extra + cmnds + " 1>" + "~/" + stdoutf + " 2>" + "~/" + stderrf + s_in + "\n"
    print("============")
    print(cwd)
    print(stdoutf)
    print(stderrf)
    print(cmdf)
    print(cmnds)
    print("============")
    codecs.open(cmdf, "w", "utf-8").write(cmnds)  # kirjoitetaan komentotiedosto
    mkdirs("/tmp/run")  # varmistetaan run-hakemisto
    udir = cwd.replace("/tmp/", "")  # koska mountattu eri tavalla, poistetaan tmp alusta
    # print(udir,"\nWait for run")

    # os.system("inotifywait "+ cmdf  + " -e delete")
    # p = Popen("inotifywait "+ cmdf  + " -e delete", shell=True) # skripti tuhoaa ajojonon kun se valmis
    # ./docker-run-timeout.sh 10s -v /opt/cs:/cs/:ro  -v $USERPATH:/home/agent/ -w /home/agent cs3 /cs/rcmd.sh $CMDNAME 

    dargs = ["docker", "run", "--name", tmpname, "--rm=true", "-v", "/opt/cs:/cs/:ro", "-v",
             "/tmp/uhome/" + udir + "/:/home/agent/",
             # dargs = ["/cs/docker-run-timeout.sh", "10s", "-v", "/opt/cs:/cs/:ro", "-v", "/tmp/uhome/" + udir + ":/home/agent/",
             # "-w", "/home/agent", "ubuntu", "/cs/rcmd.sh", urndname + ".sh"]
             "-w", "/home/agent", "cs3", "/cs/rcmd.sh", urndname + ".sh"]
    print(dargs)
    p = Popen(dargs, shell=shell, cwd="/cs", stdout=PIPE, stderr=PIPE, env=env)  # , timeout=timeout)
    try:
        stdout, stderr = p.communicate(timeout=timeout)
        print("stdout: ", stdout[:100])
        print("stderr: ", stderr)
        print("Run2 done!")
        try:
            pwd = codecs.open(cwd + '/pwd.txt', 'r', "utf-8").read()  # .encode("utf-8")
        except:
            pwd = ""

        if ( stderr ):
            remove(cwd + "/" + stdoutf)
            remove(cwd + "/" + stderrf)
            err = str(stderr)
            if "File size limit" in err: err = "File size limit exceeded"
            if "Killed" in err: err = "Timeout. Too long loop?"
            return -3, '', ("Run error: " + err).encode(), pwd
        try:
            stdout = codecs.open(cwd + "/" + stdoutf, 'r', code).read().encode("utf-8")  # luetaan stdin ja err
            stderr = codecs.open(cwd + "/" + stderrf, 'r', "utf-8").read().encode("utf-8")
        except UnicodeDecodeError:
            stdout = codecs.open(cwd + "/" + stdoutf, 'r', "iso-8859-15").read().encode(
                "iso-8859-15")  # luetaan stdin ja err
            stderr = codecs.open(cwd + "/" + stderrf, 'r', "utf-8").read().encode("iso-8859-15")

        remove(cwd + "/" + stdoutf)
        remove(cwd + "/" + stderrf)
        remove(cwd + '/pwd.txt')
        # print(stdout)
        print("stderr", stderr)
    except subprocess.TimeoutExpired:
        # p.kill()
        remove(cwd + "/" + stdoutf)
        remove(cwd + "/" + stderrf)
        remove(cwd + '/pwd.txt')
        os.system("docker rm -f " + tmpname)
        return -9, '', '', pwd
    except IOError as e:
        remove(cwd + "/" + stdoutf)
        remove(cwd + "/" + stderrf)
        remove(cwd + '/pwd.txt')
        return -2, '', ("IO Error" + str(e)).encode()
    return 0, stdout, stderr, pwd


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

    for i in range(n1, n2 + 1):
        line = lines[i]
        ln = linefmt.format(i + 1)
        file.write((ln + line + "\n"))


def write_json_error(file, err, result, points_rule):
    return_points(points_rule, result)

    result["web"] = {"error": err}
    result_str = json.dumps(result)
    file.write(result_str.encode())
    print("ERROR:======== ", err.encode("UTF8"))
    print(result_str)


def remove_before(what, s):
    # print("=================================== WHAT ==============")
    # print(what, " ", s)
    i = s.find(what)
    if i < 0: return s
    s = s[i + 1:]
    i = s.find("\n")
    if i < 0: return ""
    return s[i + 1:]


def remove(fname):
    # noinspection PyBroadException
    try:
        os.remove(fname)
    except:
        return


def mkdirs(path):
    if os.path.exists(path): return
    os.makedirs(path)


def removedir(dirname):
    # noinspection PyBroadException
    try:
        # os.chdir('/tmp')
        shutil.rmtree(dirname)
    except:
        return


def get_html(ttype, query):
    user_id = get_param(query, "user_id", "--")
    # print("UserId:", user_id)
    if user_id == "Anonymous":
        allow_anonymous = str(get_param(query, "anonymous", "false")).lower()
        jump = get_param(query, "taskID", "")
        # print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX jump: ", jump)
        if allow_anonymous != "true":
            return NOLAZY + '<p class="pluginError"><a href="/login?anchor='+jump+'">Please login to interact with this component</a></p><pre class="csRunDiv">' + get_param(
                query, "byCode", "") + '</pre>'
    do_lazy = is_lazy(query)
    # do_lazy = False
    # print("do_lazy",do_lazy,type(do_lazy))

    js = query_params_to_map_check_parts(query)
    # print(js)
    if "byFile" in js and not ("byCode" in js):
        js["byCode"] = get_url_lines_as_string(js["byFile"])  # TODO: Tähän niin että jos tiedosto puuttuu, niin parempi tieto
    bycode = ""  
    if "byCode" in js: bycode = js["byCode"]
    if get_param(query, "noeditor", False): bycode = ""

    qso = json.dumps(query.jso)
    print(qso)
    uf = get_param(query, "uploadedFile", None)
    ut = get_param(query, "uploadedType", None)
    uf = get_json_eparam(query.jso, "state", "uploadedFile", uf)
    ut = get_json_eparam(query.jso, "state", "uploadedType", ut)
    if uf and ut:
        js["uploadedFile"] = uf;
        js["uploadedType"] = ut;
    
    jso = json.dumps(js)
    print(jso)
    runner = 'cs-runner'
    # print(ttype)
    is_input = ''
    if "input" in ttype or "args" in ttype: is_input = '-input'
    if "comtest" in ttype or "junit" in ttype: runner = 'cs-comtest-runner'
    if "tauno" in ttype: runner = 'cs-tauno-runner'
    if "simcir" in ttype:
        runner = 'cs-simcir-runner'
        bycode = ''
    if "parsons" in ttype: runner = 'cs-parsons-runner'
    if "jypeli" in ttype or "graphics" in ttype or "alloy" in ttype: runner = 'cs-jypeli-runner'
    if "sage" in ttype: runner = 'cs-sage-runner'

    if is_review(query):
        usercode = get_json_eparam(query.jso, "state", "usercode", "")
        userinput = get_json_eparam(query.jso, "state", "userinput", None)
        userargs = get_json_eparam(query.jso, "state", "userargs", None)
        s = ""
        if ( userinput != None ): s = s + '<p>Input:</p><pre>' + userinput + '</pre>'
        if ( userargs != None ): s = s + '<p>Args:</p><pre>' + userargs + '</pre>'
        result = NOLAZY + '<div class="review" ng-non-bindable><pre>' + usercode + '</pre>'+s+'</div>'
        return result

    r = runner + is_input
    s = '<' + r + '>xxxJSONxxx' + jso + '</' + r + '>'
    # print(s)
    lazy_visible = ""
    lazy_class = ""
    lazy_start = ""
    lazy_end = ""

    if "csconsole" in ttype:  # erillinen konsoli
        r = "cs-console"

    if do_lazy:
        # r = LAZYWORD + r;    
        if type(bycode) != type(''): 
            print("Ei ollut string: ", bycode, jso)
            bycode = '' + str(bycode)       
        ebycode = html.escape(bycode)
        lazy_visible = '<div class="lazyVisible csRunDiv no-popup-menu" >' + get_surrounding_headers(query,
                                                                                                    '<div class="csRunCode csEditorAreaDiv csrunEditorDiv csRunArea csInputArea csLazyPre" ng-non-bindable><pre>' + ebycode + '</pre></div>') + '</div>'
        # lazyClass = ' class="lazyHidden"'
        lazy_start = LAZYSTART
        lazy_end = LAZYEND

    if ttype == "c1" or True:  # c1 oli testejä varten ettei sinä aikana rikota muita.
        hx = binascii.hexlify(jso.encode("UTF8"))
        s = lazy_start + '<' + r + lazy_class + '>xxxHEXJSONxxx' + hx.decode() + '</' + r + '>' + lazy_end
        s += lazy_visible
    return s


def wait_file(f1):
    """
    Wait until the file is ready or 10 tries has been done
    :param f1: filename to wait
    :return: sthe file status if it became ready, otherwise False
    """
    count = 0
    while count < 10:
        count += 1
        if os.path.isfile(f1):
            s1 = os.stat(f1)
            if s1.st_size > 50: return s1
            print(s1.st_size, " ??????????????????????? ")
        time.sleep(0.05)
    return False


def copy_file(f1, f2, remove_f1=False, is_optional=False):
    """
    Copy file.  This function is done, because basic copy2 seems to fail in some
    cases or to be more specific, the f1 may not be ready before starting copy.
    First if the file is not optional, it is waited to appear.  After
    appering it should be more than 43 bytes long (seems the not ready file is many
    times 43 bytes long)
    :param f1: file name to copyt
    :param f2:
    :param remove_f1:
    :param is_optional:
    :return:
    """
    try:
        print(f1, f2)
        count = 0
        while count < 10:
            count += 1
            if not os.path.isfile(f1) and is_optional: return False, ""
            s1 = wait_file(f1)
            if not s1:
                print("No file:", f1)
                return False, "No file: " + f1
            shutil.copy2(f1, f2)
            # os.system("cp " + f1 + " " + f2)
            s2 = os.stat(f2)
            print(s1.st_size, " ?? ", s2.st_size)
            if s1.st_size == s2.st_size:
                if remove_f1: remove(f1)
                return True, ""
            print(s1.st_size, " != ", s2.st_size)
        print("Copy error!!!")
        return False, "Copy error!!!"
    except OSError as e:
        # err = err + "\n" + str(e) + "\n" + out
        print(e)
        return False, e


def debug_str(s):
    t = datetime.datetime.now()
    print(t.isoformat(' ') + ": " + s)


def log(self):
    t = datetime.datetime.now()
    agent = " :AG: " + self.headers["User-Agent"]
    if agent.find("ython") >= 0: agent = ""
    logfile = "/csimages/log.txt"
    try:
        open(logfile, 'a').write(t.isoformat(' ') + ": " + self.path + agent + " u:" + self.user_id + "\n")
    except Exception as e:
        print(e)
        return

    return


def check_code(out, err, compiler_output, ttype):
    err = err.decode("utf-8") + compiler_output
    if ttype == "fs":
        err = err.replace(
            "F# Compiler for F# 3.0 (Open Source Edition)\nFreely distributed under the Apache 2.0 Open Source License\n",
            "")

    if type('') != type(err): err = err.decode()
    # if type(out) != type(''): out = out.decode()
    # noinspection PyBroadException
    try:
        if out and out[0] in [254, 255]:
            out = out.decode('UTF16')
        elif type('') != type(out):
            out = out.decode('utf-8-sig')
    except:
        out = out.decode('iso-8859-1')
    return out, err
    
def give_points(points_rule, rule, default=0):
    if not points_rule: return
    if rule in points_rule or default != 0: 
        points_rule["valid"] = True  # rule found
    p = points_rule.get(rule, default)
    if not points_rule.get("cumulative", True):
        points_rule["result"] = max(points_rule.get("result", 0), p)
        return
    print("rule: ", rule)
    ptstype = "run"
    if "test" in rule: ptstype = "test"
    if "doc" in rule: ptstype = "doc"
    # if "code" in rule: ptstype = "code"
    pts = points_rule.get("points", None)
    if pts:
        ptype = pts.get(ptstype, 0)
        print(ptstype, "===" , pts[ptstype], p)
        pts[ptstype] = ptype + p
    else:
        pts = {}
        points_rule["points"] = pts
        pts[ptstype] = p
    points_rule["result"] = pts.get("run", 0) + pts.get("test", 0) + pts.get("doc", 0) + pts.get("code", 0)


def get_points_rule(points_rule, key, default):
    if not points_rule: return default
    return points_rule.get(key, default)


def return_points(points_rule, result):
    if not points_rule: return
    if "save" not in result: return
    # if not points_rule.get("valid", False) and "points" not in points_rule: return # no rule found
    if "result" in points_rule:
        tim_info = {"points": points_rule["result"]}
        result["tim_info"] = tim_info
    if "points" in points_rule: result["save"]["points"] = points_rule["points"]

    
def get_imgsource(query):
    result = get_param(query, "imgsource", "")
    if result: return result
    result = get_param(query, "bmpname", "") # backwards compability
    

class TIMServer(http.server.BaseHTTPRequestHandler):
    def __init__(self, request, client_address, _server):
        # print(request,client_address) 
        super().__init__(request, client_address, _server)
        self.user_id = "--"

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

        if self.path.find('/multihtml') < 0:
            self.do_all(post_params(self))
            return

        print("do_POST MULTIHML ==========================================")
        t1 = time.clock()
        querys = multi_post_params(self)
        do_headers(self, "application/json")
        is_tauno = self.path.find('/tauno') >= 0
        is_simcir = self.path.find('/simcir') >= 0
        is_parsons = self.path.find('/parsons') >= 0
        htmls = []
        self.user_id = get_param(querys[0], "user_id", "--")
        print("UserId:", self.user_id)
        log(self)
        # print(querys)

        global_anonymous = False
        for query in querys:
            ga = get_param(query, "GlobalAnonymous", None)
            if ga: global_anonymous = True
            if global_anonymous: query.query["anonymous"] = [True]
            # print(query.jso)
            # print(str(query))
            usercode = get_json_param(query.jso, "state", "usercode", None)
            if usercode: query.query["usercode"] = [usercode]
            userinput = get_json_param(query.jso, "state", "userinput", None)
            if userinput: query.query["userinput"] = [userinput]
            userargs = get_json_param(query.jso, "state", "userargs", None)
            if userargs: query.query["userargs"] = [userargs]
            selected_language = get_json_param(query.jso, "state", "selectedLanguage", None)
            if selected_language: query.query["selectedLanguage"] = [selected_language]
            ttype = get_param(query, "type", "cs").lower()
            if is_tauno: ttype = 'tauno'
            if is_simcir: ttype = 'simcir'
            if is_parsons: ttype = 'parsons'
            s = get_html(ttype, query)
            # print(s)
            htmls.append(s)

        # print(htmls)
        sresult = json.dumps(htmls)
        self.wout(sresult)
        log(self)
        t2 = time.clock()
        ts = "multihtml: %7.4f" % (t2 - t1)
        print(ts)

    def do_PUT(self):
        print("do_PUT =================================================")
        t1put = time.time()
        self.do_all(post_params(self))
        t2 = time.time()
        ts = "do_PUT: %7.4f" % (t2 - t1put)
        print(ts)

    def wout(self, s):
        self.wfile.write(s.encode("UTF-8"))

    # see: http://stackoverflow.com/questions/366682/how-to-limit-execution-time-of-a-function-call-in-python        
    def signal_handler(self, signum, frame):
        print("Timed out1!")
        raise Exception("Timed out1!")

    def do_all(self, query):
        try:
            signal.signal(signal.SIGALRM, self.signal_handler)
            signal.alarm(20)   # Ten seconds
        except Exception  as e:
            print("No signal", e)
        try:
            self.do_all_t(query)
        except Exception  as e:
            print("Timed out2!", e)
            logging.exception("Timed out 2 trace")
        
        
    def do_all_t(self, query):
        pwd = ""
        print(threading.currentThread().getName())
        result = {}  # query.jso
        if not result: result = {}
        save = {}
        web = {}
        result["web"] = web

        # print("doAll ===================================================")
        # print(self.path)
        # print(self.headers)

        if self.path.find('/favicon.ico') >= 0:
            self.send_response(404)
            return

        # print(query)
        self.user_id = get_param(query, "user_id", "--")
        print("UserId:", self.user_id)
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

        is_template = self.path.find('/template') >= 0
        is_fullhtml = self.path.find('/fullhtml') >= 0
        is_gethtml = self.path.find('/gethtml') >= 0
        is_html = (self.path.find('/html') >= 0 or self.path.find('.html') >= 0) and not is_gethtml
        is_css = self.path.find('.css') >= 0
        is_js = self.path.find('.js') >= 0
        is_reqs = self.path.find('/reqs') >= 0
        is_iframe_param = get_param_del(query, "iframe", "")
        is_iframe = (self.path.find('/iframe') >= 0) or is_iframe_param
        is_answer = self.path.find('/answer') >= 0
        is_tauno = self.path.find('/tauno') >= 0
        is_simcir = self.path.find('/simcir') >= 0
        is_parsons = self.path.find('/parsons') >= 0
        is_ptauno = self.path.find('/ptauno') >= 0
        is_rikki = self.path.find('rikki') >= 0
        print_file = get_param(query, "print", "")

        content_type = 'text/plain'
        if is_js: content_type = 'application/javascript'
        if is_fullhtml or is_gethtml or is_html or is_ptauno or is_tauno: content_type = 'text/html; charset=utf-8'
        if is_reqs or is_answer: content_type = "application/json"
        if is_css: content_type = 'text/css'
        do_headers(self, content_type)

        if is_template:
            tempfile = get_param(query, "file", "")
            tidx = get_param(query, "idx", "0")
            print("tempfile: ", tempfile, tidx)
            # return self.wout(file_to_string('templates/' + tempfile))        
            return self.wout(get_template('templates', tidx, tempfile))

        if self.path.find("refresh") >= 0:
            self.wout(get_chache_keys())
            clear_cache()
            return

        if is_gethtml:
            scripts = get_param(query, "scripts", "")
            p = self.path.split("?")
            print(p, scripts)
            self.wout(replace_scripts(file_to_string(p[0]), scripts, "%INCLUDESCRIPTS%"))
            return

        if is_ptauno:
            # print("PTAUNO: " + content_type)
            p = self.path.split("?")
            self.wout(file_to_string(p[0]))
            return

            # Get the template type
        ttype = get_param(query, "type", "cs").lower()

        if is_tauno and not is_answer: ttype = 'tauno'  # answer is newer tauno
        if is_simcir: ttype = 'simcir'

        if is_reqs:
            # result_json = {"js": ["http://tim-beta.it.jyu.fi/cs/js/dir.js"], "angularModule": ["csApp"],
            #               "css": ["http://tim-beta.it.jyu.fi/cs/css/cs.css"], "multihtml": True}
            # result_json = {"js": ["/cs/js/dir.js"], "angularModule": ["csApp","csConsoleApp"],
            # result_json = {"js": ["/cs/js/dir.js", "https://static.jsbin.com/js/embed.js", "/static/scripts/bower_components/ace-builds/src-min-noconflict/ext-language_tools.js"],
            # result_json = {"js": ["/cs/js/dir.js", "/static/scripts/bower_components/ace-builds/src-min-noconflict/ext-language_tools.js"],
            # result_json = {"js": ["/cs/js/dir.js","https://tim.it.jyu.fi/csimages/html/chart/Chart.min.js","https://sagecell.sagemath.org/static/embedded_sagecell.js"],
            templs = {}
            if not (is_tauno or is_rikki or is_parsons or is_simcir): templs = get_all_templates('templates')
            result_json = {"js": ["/cs/js/dir.js",
                                  "/static/scripts/jquery.ui.touch-punch.min.js",
                                  "/cs/cs-parsons/csparsons.js",
                                  #"https://tim.it.jyu.fi/csimages/html/chart/Chart.min.js",
                                  "https://sagecell.sagemath.org/static/embedded_sagecell.js"],
                                  #"/cs/js/embedded_sagecell.js"],
                           "angularModule": ["csApp", "csConsoleApp"],
                           "css": ["/cs/css/cs.css"], "multihtml": True}
            if is_parsons:
                result_json = {"js": ["/cs/js/dir.js",
                                      # "https://tim.it.jyu.fi/csimages/html/chart/Chart.min.js",
                                      "https://sagecell.sagemath.org/static/embedded_sagecell.js",
                                      # "/cs/js/embedded_sagecell.js",
                                      "/static/scripts/jquery.ui.touch-punch.min.js",
                                      "/cs/cs-parsons/csparsons.js",
                                      "/cs/js-parsons/lib/underscore-min.js",
                                      "/cs/js-parsons/lib/lis.js",
                                      "/cs/js-parsons/parsons.js",
                                      "/cs/js-parsons/lib/skulpt.js",
                                      "/cs/js-parsons/lib/skulpt-stdlib.js",
                                      "/cs/js-parsons/lib/prettify.js"
                ],
                               "angularModule": ["csApp", "csConsoleApp"],
                               "css": ["/cs/css/cs.css", "/cs/js-parsons/parsons.css",
                                       "/cs/js-parsons/lib/prettify.css"], "multihtml": True}
            if is_simcir:
                result_json = {"js": ["/cs/js/dir.js",
                                      "/cs/simcir/simcir.js",
                                      "/cs/simcir/simcir-basicset.js",
                                      "/cs/simcir/simcir-library.js",
                ],
                               "angularModule": ["csApp"],
                               "css": ["/cs/css/cs.css", "/cs/simcir/simcir.css", "/cs/simcir/simcir-basicset.css"],
                               "multihtml": True}
            result_json.update(templs)
            # result_json = {"js": ["js/dir.js"], "angularModule": ["csApp"],
            #               "css": ["css/cs.css"]}
            result_str = json.dumps(result_json)
            return self.wout(result_str)

        if is_tauno and not is_answer:
            # print("PTAUNO: " + content_type)
            p = self.path.split("?")
            self.wout(file_to_string(p[0]))
            return

        # answer-route

        try:
            # if ( query.jso != None and query.jso.has_key("state") and query.jso["state"].has_key("usercode") ):
            uploadedFile = get_json_param(query.jso, "input", "uploadedFile", None)
            uploadedType = get_json_param(query.jso, "input", "uploadedType", None)
            if uploadedFile and uploadedType: 
                save["uploadedFile"] = uploadedFile
                save["uploadedType"] = uploadedType
            usercode = get_json_param(query.jso, "state", "usercode", None)
            if usercode: query.query["usercode"] = [usercode]
            userinput = get_json_param(query.jso, "state", "userinput", None)
            if userinput: query.query["userinput"] = [userinput]
            selected_language = get_json_param(query.jso, "input", "selectedLanguage", None)
            if selected_language: save["selectedLanguage"] = selected_language
            userargs = get_json_param(query.jso, "input", "userargs", None)
            is_doc = get_json_param3(query.jso, "input", "markup", "document", False)

            if userargs: save["userargs"] = userargs

            # print("USERCODE: XXXXX = ", usercode)

            # print("Muutos ========")
            # pprint(query.__dict__, indent=2)

            if is_css:
                # return self.wout(file_to_string('cs.css'))
                return self.wout(file_to_string(self.path))

            if is_js:
                if is_rikki:
                    return self.wout(file_to_string('js/dirRikki.js'))
                # return self.wout(file_to_string('js/dir.js'))
                return self.wout(file_to_string(self.path))

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

            if is_iframe and not print_file and not ttype == "js":
                s = string_to_string_replace_url(
                    '<iframe frameborder="0"  src="https://tim.it.jyu.fi/cs/fullhtml?##QUERYPARAMS##" ' +
                    'style="overflow:hidden;" height="##HEIGHT##" width="100%"  seamless></iframe>',
                    "##QUERYPARAMS##", query)
                return self.wout(s)

            # Check if user name or temp name
            rndname = generate_filename()
            delete_tmp = True
            opt = get_param(query, "opt", "")
            timeout = get_param(query, "timeout", 10)
            task_id = get_param(query, "taskID", "")
            doc_id, dummy = (task_id + "NONE.none").split(".", 1)
            
            upath = get_param(query, "path", "")  # from user/sql do user and /sql
            epath = "/" + doc_id
            if "/" in upath:                      # if user/ do just user and ""
                upath, epath = upath.split("/",1)
                if epath: epath = "/" + epath
            
            if upath == "user" and self.user_id:
                userpath = "user/" + hash_user_dir(self.user_id)
                mustpath = "/tmp/" + userpath
                basename =  userpath + epath
                fullpath = "/tmp/" + basename # check it is sure under userpath
                if not os.path.abspath(fullpath).startswith(mustpath): basename = userpath + "/ERRORPATH"
                delete_tmp = False
                mkdirs("/tmp/user")
                print(task_id, doc_id, fullpath)
            else:
                # Generate random cs and exe filenames
                basename = "tmp/" + rndname
                mkdirs("/tmp/tmp")
            filename = get_param(query, "filename", "prg")

            ifilename = get_param(query, "inputfilename", "/input.txt")
            
            # TODO: Validoi filename, ifilename yms jossa voi olla mm ..
            
            # csfname = "/tmp/%s.cs" % basename
            # exename = "/tmp/%s.exe" % basename
            csfname = "/tmp/%s/%s.cs" % (basename, filename)
            exename = "/tmp/%s/%s.exe" % (basename, filename)
            pure_exename = "./%s.exe" % filename
            inputfilename = "/tmp/%s/%s" % (basename, ifilename)
            prgpath = "/tmp/%s" % basename
            filepath = prgpath

            print("ttype: ", ttype)
            fileext = "cs"

            before_code = get_param(query, "beforeCode", "")

            # if ttype == "console":
            # Console program
            if ttype == "cc":
                if filename.endswith(".h") or filename.endswith(".c") or filename.endswith(".cc"):
                    csfname = "/tmp/%s/%s" % (basename, filename)
                else:
                    csfname = "/tmp/%s/%s.c" % (basename, filename)
                fileext = "c"

            if ttype == "c++":
                if filename.endswith(".h") or filename.endswith(".hpp") or filename.endswith(".cpp"):
                    csfname = "/tmp/%s/%s" % (basename, filename)
                else:
                    csfname = "/tmp/%s/%s.cpp" % (basename, filename)
                fileext = "cpp"

            if ttype == "scala":
                csfname = "/tmp/%s/%s.scala" % (basename, filename)
                fileext = "scala"

            if ttype == "fs":
                csfname = "/tmp/%s/%s.fs" % (basename, filename)
                fileext = "fs"

            if ttype == "py":
                csfname = "/tmp/%s/%s.py" % (basename, filename)
                exename = csfname
                pure_exename = "./%s.py" % filename
                fileext = "py"
                pngname = "/csimages/%s.png" % rndname
                imgsource = get_imgsource(query)

            if ttype == "octave":
                csfname = "/tmp/%s/%s.oct" % (basename, filename)
                exename = csfname
                pure_exename = "./%s.oct" % filename
                fileext = "oct"
                pngname = "/csimages/%s.png" % rndname
                imgsource = get_imgsource(query)
                wavsource = get_param(query, "wavsource", "")
                wavdest = "/csimages/%s/%s" % (self.user_id,wavsource)
                wavname = "%s/%s" % (self.user_id,wavsource)
                mkdirs("/csimages/%s" % self.user_id)


            if ttype == "alloy":
                csfname = "/tmp/%s/%s.als" % (basename, filename)
                exename = csfname
                pure_exename = "./%s.als" % filename
                imgsource = "%s/mm.png" % prgpath
                pngname = "/csimages/%s.png" % rndname

            if ttype == "shell":
                csfname = "/tmp/%s/%s.sh" % (basename, filename)
                exename = csfname
                pure_exename = "/home/agent/%s.sh" % filename
                fileext = "sh"

            if ttype == "run":
                csfname = "/tmp/%s/%s" % (basename, filename)
                exename = csfname
                pure_exename = "/home/agent/%s" % filename
                pngname = "/csimages/%s.png" % rndname
                imgsource = get_imgsource(query)


            if ttype == "r":
                debug_str("r alkaa")
                prgpath = "/tmp/%s/r" % basename
                filepath = prgpath
                csfname = "%s/%s.r" % (prgpath, filename)
                fileext = "r"
                exename = csfname
                mkdirs(filepath)
                image_ext = "png"
                pure_exename = "./%s.r" % filename
                imgsource = "%s/Rplot001.%s" % (prgpath, image_ext)
                pure_pngname = u"{0:s}.{1:s}".format(rndname, image_ext)
                pngname = "/csimages/%s.%s" % (rndname, image_ext)

            if ttype == "jjs":
                csfname = "/tmp/%s/%s.js" % (basename, filename)
                exename = csfname
                pure_exename = u"./{0:s}.js".format(filename)
                fileext = "js"
                if before_code == "":  # Jos ei ole vamista koodia, niin tehdään konsoli johon voi tulostaa
                    before_code = ('var console={};'
                                   'console.log = function(s) {'
                                   '    var res = "", sep = "";'
                                   '    for (var i=0; i<arguments.length; i++) { res += sep + arguments[i]; sep = " "; } '
                                   '    print(res);'
                                   '};')
            print(before_code)

            if ttype == "sql" or ttype == "psql":
                csfname = "/tmp/%s/%s.sql" % (basename, filename)
                exename = csfname
                pure_exename = u"{0:s}.sql".format(filename)
                fileext = "sql"
                dbname = get_param(query, "dbname", "db")

            if ttype == "clisp":
                csfname = "/tmp/%s/%s.lisp" % (basename, filename)
                exename = csfname
                fileext = "lisp"
                pure_exename = u"./{0:s}.lisp".format(filename)

            if "jypeli" in ttype:
                # Jypeli game
                mainfile = "/cs/jypeli/Ohjelma.cs"
                csfname = "/tmp/%s/%s.cs" % (basename, filename)
                exename = "/tmp/%s/%s.exe" % (basename, filename)
                imgsource = "/tmp/%s/output.bmp" % (basename)
                fileext = "cs"
                pure_bmpname = "./%s.bmp" % filename
                pngname = "/csimages/%s.png" % rndname
                pure_exename = u"{0:s}.exe".format(filename)
                pure_pngname = u"{0:s}.png".format(rndname)
            if "comtest" in ttype:
                # ComTest test cases
                testcs = "/tmp/%s/%sTest.cs" % (basename, filename)
                fileext = "cs"
                # testdll = "/tmp/%s/%sTest.dll" % (basename, filename)
                testdll = u"./{0:s}Test.dll".format(filename)
            if "ccomtest" in ttype:
                # ComTest test cases
                csfname = "/tmp/%s/%s.cpp" % (basename, filename)
                fileext = "cpp"
                # testcs = "/tmp/%s/%s.cpp" % (basename, filename)
                testcs = u"{0:s}.cpp".format(filename)
            if "text" in ttype or "xml" in ttype or "css" in ttype:
                # text file
                if userargs: filename = userargs
                csfname = "/tmp/%s/%s" % (basename, filename)
                pure_exename = u"./{0:s}".format(filename)

                # Unknown template
                # self.wfile.write(("Invalid project type given (type=" + ttype + ")").encode())
                # return

            # Check query parameters
            p0 = FileParams(query, "", "")
            # print("p0=")
            # print(p0.replace)
            if p0.url == "" and p0.replace == "": p0.replace = "XXXX"

            # print("type=" + ttype)

            # s = ""
            # if p0.url != "":
            #
            if p0.breakCount > 0:
                parts = get_file_parts_to_output(query, False)
                print(parts)
                if print_file == "2": return self.wout(json.dumps(parts))
                s = join_file_parts(p0, parts)
            else:
                s = get_file_to_output(query, False and print_file)

            # Open the file and write it
            # print(print_file,"Haetaan")
            if print_file:
                return self.wout(s)

            mkdirs(prgpath)
            # os.chdir(prgpath)

            # /answer-path comes here
            usercode = get_json_param(query.jso, "input", "usercode", None)
            if usercode: save["usercode"] = usercode
            userinput = get_json_param(query.jso, "input", "userinput", None)

            if userinput:
                save["userinput"] = userinput
                if userinput[-1:] != "\n": userinput += "\n"
            nosave = get_param(query, "nosave", None)     
            nosave = get_json_param(query.jso, "input", "nosave", nosave)

            if not nosave: result["save"] = save

            if ttype == "scala":
                csfname = "/tmp/%s/%s.scala" % (basename, filename)
                classname = filename
                fileext = "scala"

            if "java" in ttype or "jcomtest" in ttype or "junit" in ttype or "graphics" in ttype:
                # java
                classpath = get_param(query, "-cp", ".") + ":$CLASSPATH"
                print("classpath=", classpath)
                package, classname = find_java_package(s)
                javaclassname = classname
                if not classname: classname = "Prg"
                if package:
                    filepath = prgpath + "/" + package.replace(".", "/")
                    mkdirs(filepath)
                    javaclassname = package + "." + classname
                filename = javaclassname + ".java"    
                javaname = filepath + "/" + classname + ".java"
                fileext = "java"
                csfname = javaname
                # imgsource = "capture.png" % basename
                imgsource = "%s/run/capture.png" % prgpath
                # pngname = "/csimages/%s.png" % basename
                pngname = "/csimages/%s.png" % rndname
                # print("TYYPPI = " + ttype + " nimi = " + csfname + " class = " + javaclassname)

            if "jcomtest" in ttype:
                # ComTest test cases
                testcs = filepath + "/" + classname + "Test.java"
                fileext = "java"
                testdll = javaclassname + "Test"

            if not s.startswith("File not found"):
                print(os.path.dirname(csfname))
                mkdirs(os.path.dirname(csfname))
                print("Write file: " + csfname)
                if s == "": s = "\n"
                codecs.open(csfname, "w", "utf-8").write(before_code + s)
                slines = s

            is_optional_image = get_json_param(query.jso, "markup", "optional_image", False)
            is_input = get_json_param(query.jso, "input", "isInput", None)
            # print(isInput)
            if is_input:
                # print("Write input file: " + inputfilename)
                codecs.open(inputfilename, "w", "utf-8").write(userinput)

            if not os.path.isfile(csfname) or os.path.getsize(csfname) == 0:
                return write_json_error(self.wfile, "Could not get the source file", result)
                # self.wfile.write("Could not get the source file\n")
                # print "=== Could not get the source file"

            nocode = get_param(query, "nocode", False)

            is_test = ""
            if "test" in ttype: is_test = "test"
            points_rule = get_param(query, "pointsRule", None)
            if points_rule:
                points_rule["points"] = get_json_param(query.jso, "state", "points", None)
                points_rule["result"] = 0
                if points_rule["points"]:
                    if is_test:
                        points_rule["points"]["test"] = 0
                    elif is_doc:
                        points_rule["points"]["doc"] = 0
                        print("doc points: ",points_rule["points"]["doc"])
                    else:
                        points_rule["points"]["run"] = 0

                expect_code = get_points_rule(points_rule, is_test + "expectCode", None)
                if expect_code and not is_doc:
                    if expect_code == "byCode": expect_code = get_param(query, "byCode", "")
                    excode = re.compile(expect_code.rstrip('\n'), re.M)
                    if excode.match(usercode): give_points(points_rule, "code", 1)
            print(points_rule)


            # print(ttype)
            # ########################## Compiling programs ###################################################
            try:
                log(self)
                cmdline = ""

                if is_doc:
                    # doxygen
                    #  ./doxygen/csdoc.sh /tmp/user/4d859744c28dbca8348fc24833ece03aa3050371f98a882bbd4b54e5da617114/3 /csimages/docs/vesal/abcd /csimages/docs/vesal
                    # http://tim3/csimages/cs/docs/vesal/abcd/html/index.html
                    #
                    userdoc = "/csimages/docs/%s" % self.user_id
                    docrnd = generate_filename()
                    doccmd = "/cs/doxygen/csdoc.sh %s %s/%s %s" % (prgpath, userdoc, docrnd, userdoc)
                    p = re.compile('\.java')
                    docfilename = p.sub("", filename)
                    p = re.compile('[^.]*\.')
                    docfilename = p.sub("", docfilename)
                    docfilename = docfilename.replace("_","__") # jostakin syystä tekee näin
                    dochtml = "/csimages/cs/docs/%s/%s/html/%s_8%s.html" % (self.user_id, docrnd, docfilename, fileext)
                    docfile = "%s/%s/html/%s_8%s.html" % (userdoc, docrnd, docfilename, fileext)
                    print("XXXXXXXXXXXXXXXXXXXXXX",filename)
                    print("XXXXXXXXXXXXXXXXXXXXXX",docfilename)
                    print("XXXXXXXXXXXXXXXXXXXXXX",dochtml)
                    print("XXXXXXXXXXXXXXXXXXXXXX",docfile)
                    doc_output = check_output([doccmd], stderr=subprocess.STDOUT, shell=True).decode("utf-8")
                    if not os.path.isfile(docfile): # There is maybe more files with same name and it is difficult to guess the name
                        dochtml = "/csimages/cs/docs/%s/%s/html/%s" % (self.user_id, docrnd, "files.html")
                        print("XXXXXXXXXXXXXXXXXXXXXX",dochtml)
                        
                    web["docurl"] = dochtml
                    give_points(points_rule, "doc")

                elif ttype == "jypeli":
                    if s.find(" Main(") >= 0: mainfile = ""
                    #cmdline = "mcs /out:%s /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/MonoGame.Framework.dll /r:/cs/jypeli/Jypeli.Physics2d.dll /r:/cs/jypeli/OpenTK.dll /r:/cs/jypeli/Tao.Sdl.dll /r:System.Drawing /cs/jypeli/Ohjelma.cs %s" % (
                    cmdline = "mcs /out:%s /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/MonoGame.Framework.dll /r:/cs/jypeli/Jypeli.Physics2d.dll /r:/cs/jypeli/OpenTK.dll /r:/cs/jypeli/Tao.Sdl.dll /r:System.Numerics /r:System.Drawing %s %s" % (
                        exename, mainfile, csfname)
                elif ttype == "comtest":
                    jypeliref = "/r:System.Numerics /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/MonoGame.Framework.dll /r:/cs/jypeli/Jypeli.Physics2d.dll /r:/cs/jypeli/OpenTK.dll /r:/cs/jypeli/Tao.Sdl.dll /r:System.Drawing"
                    cmdline = ("java -jar /cs/java/cs/ComTest.jar nunit %s && mcs /out:%s /target:library " + jypeliref +" /reference:/usr/lib/mono/gac/nunit.framework/2.6.0.0__96d09a1eb7f44a77/nunit.framework.dll %s %s") % (
                        csfname, testdll, csfname, testcs)
                elif ttype == "jcomtest":
                    cmdline = "java comtest.ComTest %s && javac %s %s" % (csfname, csfname, testcs)
                elif ttype == "junit":
                    cmdline = "javac %s" % javaname
                elif ttype == "java" or ttype == "graphics":
                    cmdline = "javac -Xlint:all -cp %s %s" % (classpath, javaname)
                elif ttype == "scala":
                    cmdline = "scalac %s" % (csfname)
                elif ttype == "cc":
                    cmdline = "gcc -Wall %s %s -o %s -lm" % (opt, csfname, exename)
                elif ttype == "c++":
                    cmdline = "g++ -std=c++11 -Wall %s %s -o %s -lm" % (opt, csfname, exename)
                elif ttype == "py":
                    cmdline = ""
                elif ttype == "clisp":
                    cmdline = ""
                elif ttype == "ccomtest":
                    cmdline = ""
                elif ttype == "text":
                    cmdline = ""
                elif ttype == "xml":
                    cmdline = ""
                elif ttype == "css":
                    cmdline = ""
                elif ttype == "shell":
                    cmdline = ""
                elif ttype == "jjs":
                    cmdline = ""
                elif ttype == "sql":
                    cmdline = ""
                elif ttype == "psql":
                    cmdline = ""
                elif ttype == "alloy":
                    cmdline = ""
                elif ttype == "run":
                    cmdline = ""
                elif ttype == "md":
                    cmdline = ""
                elif ttype == "js":
                    cmdline = ""
                elif ttype == "simcir":
                    cmdline = ""
                elif ttype == "sage":
                    cmdline = ""
                elif ttype == "r":
                    cmdline = ""
                elif ttype == "fs":
                    cmdline = "fsharpc --out:%s %s" % (exename, csfname)
                elif ttype == "cs":
                    cmdline = "mcs /r:System.Numerics /out:%s %s" % (exename, csfname)
                else:
                    cmdline = ""
                    
                if get_param(query, "justSave", False):
                    cmdline = ""

                compiler_output = ""
                if cmdline:
                    compiler_output = check_output(["cd " + prgpath + " && " + cmdline], stderr=subprocess.STDOUT,
                                                   shell=True).decode("utf-8")
                    compiler_output = compiler_output.replace(prgpath, "")
                    give_points(points_rule, is_test + "compile")
                    
                # self.wfile.write("*** Success!\n")
                print("*** Compile Success")
                if nocode and ttype != "jcomtest": 
                    print("Poistetaan ", ttype, csfname)
                    remove(csfname)
                # print(compiler_output)
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
                if not nocode: print_lines(output, lines, 0, 10000)
                error_str += output.getvalue()
                output.close()

                if delete_tmp: removedir(prgpath)
                give_points(points_rule, is_test + "notcompile")
                return write_json_error(self.wfile, error_str, result, points_rule)


            # ########################## Running programs ###################################################
            # delete_tmp = False

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
            # if stdin: stdin = "/tmp/%s/%s" % (basename, stdin)
            # if stdin: stdin = "/tmp/%s/%s" % (basename, stdin)
            if ttype == "sql" or ttype == "psql": stdin = pure_exename

            pwd = ""

            if is_doc:
                pass  # jos doc ei ajeta
            elif get_param(query, "justSave", False):
                showname = csfname.replace(basename,"").replace("/tmp//","")
                if showname == "prg": showname = ""
                code, out, err, pwd = (0, "", ("tallennettu " + showname), "")
            elif get_param(query, "justCompile", False) and ttype.find("comtest") < 0:
                #code, out, err, pwd = (0, "".encode("utf-8"), ("Compiled " + filename).encode("utf-8"), "")
                code, out, err, pwd = (0, "", ("Compiled " + filename), "")
            elif ttype == "jypeli":
                code, out, err, pwd = run2(["mono", pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                           uargs=userargs, ulimit="ulimit -f 80000")
                if type('') != type(out): out = out.decode()
                if type('') != type(err): err = err.decode()
                err = re.sub("^ALSA.*\n", "", err, flags=re.M)
                print(err)
                # err = ""
                wait_file(imgsource)
                #statinfo = os.stat(imgsource)
                #print("bmpsize: ", statinfo.st_size)
                run(["convert", "-flip", imgsource, pngname], cwd=prgpath, timeout=20)
                #print(imgsource, pngname)
                remove(imgsource)
                # self.wfile.write("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/cs/%s.png\n" % (basename))
                print("*** Screenshot: https://tim.it.jyu.fi/csimages/cs/%s\n" % pure_pngname)
                # TODO: clean up screenshot directory
                p = re.compile('Number of joysticks:.*\n.*')
                # out = out.replace("Number of joysticks:.*","")
                out = p.sub("", out)
                if code == -9:
                    out = "Runtime exceeded, maybe loop forever\n" + out
                else:
                    # web["image"] = "http://tim-beta.it.jyu.fi/csimages/cs/" + basename + ".png"
                    web["image"] = "/csimages/cs/" + pure_pngname
                    give_points(points_rule, "run")
                if delete_tmp:
                    remove(csfname)
                    remove(exename)
            elif ttype == "graphics":
                a = []
                delay = get_json_param(query.jso, "markup", "delay", "0")
                if delay is not None: a.extend(["--delay", str(delay)])
                rect = get_json_param(query.jso, "markup", "rect", None)
                if rect: a.extend(["--rect", rect])
                print(a)
                # bmplname = "/home/me/capture.png"
                bmplname = "run/capture.png"
                runcmd = ["java", "sample.Runner", javaclassname, "--captureName", bmplname]
                runcmd.extend(a)
                code, out, err, pwd = run2(runcmd, cwd=prgpath, timeout=10, env=env, stdin=stdin, uargs=userargs)
                print(err)
                if type('') != type(out): out = out.decode()
                if type('') != type(err): err = err.decode()
                # err = ""
                # run(["convert", "-flip", imgsource, pngname], cwd=prgpath, timeout=20)
                image_ok, e = copy_file(imgsource, pngname)
                if e:
                    err = err + "\n" + str(e) + "\n" + out
                    print(e)

                    # self.wfile.write("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/cs/%s.png\n" % (basename))
                print("*** Screenshot: http://tim-beta.it.jyu.fi/csimages/cs/%s.png\n" % rndname)
                # TODO: clean up screenshot directory
                p = re.compile('Xlib:  extension "RANDR" missing on display ":1"\.\n')
                # out = out.replace("Number of joysticks:.*","")
                err = p.sub("", err)
                if code == -9:
                    out = "Runtime exceeded, maybe loop forever\n" + out
                else:
                    # web["image"] = "http://tim-beta.it.jyu.fi/csimages/cs/" + basename + ".png"
                    web["image"] = "/csimages/cs/" + rndname + ".png"
                    give_points(points_rule, "run")

            elif ttype == "r":
                debug_str("r ajoon")
                code, out, err, pwd = run2(["Rscript", "--save", "--restore", pure_exename], cwd=prgpath, timeout=10,
                                           env=env, stdin=stdin,
                                           uargs=userargs, ulimit="ulimit -f 80000")
                debug_str("r ajo valmis")
                if type('') != type(out): out = out.decode()
                if type('') != type(err): err = err.decode()
                print(err)
                #  wait_file(imgsource)
                debug_str("r kuvan kopiointi aloitetaan")
                image_ok, e = copy_file(imgsource, pngname, True, True)  # is_optional_image)
                remove(imgsource)
                debug_str("r kuva kopioitu")
                print("*** Screenshot: https://tim.it.jyu.fi/csimages/cs/%s\n" % pure_pngname)
                if code == -9:
                    out = "Runtime exceeded, maybe loop forever\n" + out
                else:
                    if image_ok:
                        web["image"] = "/csimages/cs/" + pure_pngname
                        give_points(points_rule, "run")
                if delete_tmp:
                    remove(csfname)
                    remove(exename)
                debug_str("r tiedostot poistettu")

            elif ttype == "alloy":
                runcmd = ["java", "-cp", "/cs/java/alloy-dev.jar:/cs/java", "RunAll", pure_exename]
                code, out, err, pwd = run2(runcmd, cwd=prgpath, timeout=10, env=env, stdin=stdin, uargs=userargs)
                print(err)
                if type('') != type(out): out = out.decode()
                if type('') != type(err): err = err.decode()
                # imgsource = "run/capture.png"
                if code == -9:
                    out = "Runtime exceeded, maybe loop forever\n" + out
                else:
                    # web["image"] = "http://tim-beta.it.jyu.fi/csimages/cs/" + basename + ".png"
                    image_ok, e = copy_file(imgsource, pngname, True, is_optional_image)
                    print(is_optional_image, image_ok)
                    if image_ok:
                        web["image"] = "/csimages/cs/" + rndname + ".png"
                        give_points(points_rule, "run")

            elif ttype == "comtest":
                eri = -1
                code, out, err, pwd = run2(["nunit-console", "-nologo", "-nodots", testdll], cwd=prgpath, timeout=10,
                                           env=env)
                if type('') != type(out): out = out.decode()
                if type('') != type(err): err = err.decode()
                # print(code, out, err)
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
                give_points(points_rule, "testrun")
                web["testGreen"] = True
                if eri >= 0:
                    web["testGreen"] = False
                    web["testRed"] = True
                    lni = out.find(", line ")
                    if lni >= 0:  #  and not nocode:
                        lns = out[lni + 7:]
                        lns = lns[0:lns.find("\n")]
                        lnro = int(lns)
                        # lines = codecs.open(csfname, "r", "utf-8").readlines()
                        lines = slines.split("\n")
                        # print("Line nr: "+str(lnro)) 
                        # # out += "\n" + str(lnro) + " " + lines[lnro - 1]
                        web["comtestError"] = str(lnro) + " " + lines[lnro - 1]
                else:
                    give_points(points_rule, "test")
            elif ttype == "jcomtest" or ttype == "ccomtest" or ttype == "junit":
                eri = -1
                # linenr_end = " "
                if ttype == "jcomtest":
                    code, out, err, pwd = run2(["java", "org.junit.runner.JUnitCore", testdll], cwd=prgpath, timeout=10,
                                               env=env)
                if ttype == "junit":
                    code, out, err, pwd = run2(["java", "org.junit.runner.JUnitCore", javaclassname], cwd=prgpath,
                                               timeout=10,
                                               env=env)
                if ttype == "ccomtest":
                    code, out, err, pwd = run2(["java", "-jar", "/cs/java/comtestcpp.jar", "-nq", testcs], cwd=prgpath,
                                               timeout=10, env=env)
                    # linenr_end = ":"
                if type('') != type(out): out = out.decode()
                if type('') != type(err): err = err.decode()
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
                p = re.compile('Xlib:  extension "RANDR" missing on display ":1"\.\n')
                err = p.sub("", err)
                web["testGreen"] = True
                give_points(points_rule, "testrun")
                if eri >= 0:
                    web["testGreen"] = False
                    web["testRed"] = True
                    lni = out.find(" line: ")
                    cterr = ""
                    sep = ""
                    while lni >= 0:
                        lns = out[lni + 7:]
                        lnro = getint(lns)
                        lines = codecs.open(csfname, "r", "utf-8").readlines()
                        # print("Line nr: "+str(lnro))
                        # # out += "\n" + str(lnro) + " " + lines[lnro - 1]
                        cterr += sep + str(lnro) + " " + lines[lnro - 1]
                        sep = ""
                        lni = out.find(" line: ", lni + 8)
                    web["comtestError"] = cterr
                else:
                    out = re.sub("^JUnit version.*\n", "", out, flags=re.M)
                    out = re.sub("^Time: .*\n", "", out, flags=re.M)
                    out = re.sub("^.*prg.*cpp.*\n", "", out, flags=re.M)
                    out = re.sub("^ok$", "", out, flags=re.M)
                    give_points(points_rule, "test")

            else:
                runcommand = get_param(query, "cmd", "");
                if ttype != "run" and (runcommand or get_param(query, "cmds", "")):
                    print("runcommand: ", runcommand)
                    # code, out, err, pwd = run2([runcommand], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                    #                               uargs=get_param(query, "runargs", "") + " " + userargs)
                    cmd = shlex.split(runcommand)
                    extra = get_param(query, "cmds", "").format(pure_exename)
                    if extra != "": cmd = []
                    print("run: ", cmd, extra, pure_exename, csfname)
                    try:
                        code, out, err, pwd = run2(cmd, cwd=prgpath, timeout=10, env=env, stdin=stdin, 
                                                   uargs=get_param(query, "runargs", "") + " " + userargs,
                                                   extra=extra)
                    except Exception as e:
                        print(e)
                        code, out, err = (-1, "", str(e).encode())
                    print("Run2: ", imgsource, pngname)    
                    if code == -9:
                        out = "Runtime exceeded, maybe loop forever\n" + out
                    else:
                        if imgsource and pngname:
                            image_ok, e = copy_file(filepath + "/" + imgsource, pngname, True, is_optional_image)
                            if e: err = (str(err) + "\n" + str(e) + "\n" + str(out)).encode("utf-8")
                            # web["image"] = "http://tim-beta.it.jyu.fi/csimages/cs/" + basename + ".png"
                            print(is_optional_image, image_ok)
                            remove(imgsource)
                            if image_ok: web["image"] = "/csimages/cs/" + rndname + ".png"
                elif ttype == "java":
                    print("java: ", javaclassname)
                    # code, out, err = run2(["java" ,"-cp",prgpath, javaclassname], timeout=10, env=env, uargs = userargs)
                    code, out, err, pwd = run2(["java", "-cp", classpath, javaclassname], cwd=prgpath, timeout=10,
                                               env=env, stdin=stdin, ulimit="ulimit -f 10000",
                                               uargs=userargs)
                elif ttype == "scala":
                    print("scala: ", classname)
                    # code, out, err = run2(["java" ,"-cp",prgpath, javaclassname], timeout=10, env=env, uargs = userargs)
                    code, out, err, pwd = run2(["scala", classname], cwd=prgpath, timeout=10,
                                               env=env, stdin=stdin, ulimit="ulimit -f 10000",
                                               uargs=userargs)
                elif ttype == "shell":
                    print("shell: ", pure_exename)
                    # os.chmod(exename, stat.S_IEXEC)
                    os.system('chmod +x ' + exename)
                    # if stdin: stdin = stdin
                    extra = "cd $PWD\nsource "
                    try:
                        # code, out, err = run2([pure_exename], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs)
                        code, out, err, pwd = run2([pure_exename], cwd=prgpath, timeout=timeout, env=env, stdin=stdin,
                                                   uargs=userargs,
                                                   extra=extra)
                    except OSError as e:
                        print(e)
                        code, out, err = (-1, "", str(e).encode())
                elif ttype == "run": 
                    cmd = shlex.split(get_param(query, "cmd", "ls -la") + " " + pure_exename)
                    extra = get_param(query, "cmds", "").format(pure_exename)
                    if extra != "": cmd = []
                    print("run: ", cmd, extra, pure_exename, csfname)
                    print("Run1: ", imgsource, pngname)    
                    try:
                        code, out, err, pwd = run2(cmd, cwd=prgpath, timeout=10, env=env, stdin=stdin, uargs=userargs,
                                                   extra=extra)
                    except Exception as e:
                        print(e)
                        code, out, err = (-1, "", str(e).encode())
                    print("Run2: ", imgsource, pngname)    
                    if code == -9:
                        out = "Runtime exceeded, maybe loop forever\n" + out
                    else:
                        if imgsource and pngname:
                            image_ok, e = copy_file(filepath + "/" + imgsource, pngname, True, is_optional_image)
                            if e: err = (str(err) + "\n" + str(e) + "\n" + str(out)).encode("utf-8")
                            # web["image"] = "http://tim-beta.it.jyu.fi/csimages/cs/" + basename + ".png"
                            print(is_optional_image, image_ok)
                            remove(imgsource)
                            if image_ok: web["image"] = "/csimages/cs/" + rndname + ".png"

                elif ttype == "jjs":
                    print("jjs: ", exename)
                    code, out, err, pwd = run2(["jjs", pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                               uargs=userargs)
                elif ttype == "sql":
                    print("sql: ", exename)
                    code, out, err, pwd = run2(["sqlite3", dbname], cwd=prgpath, timeout=timeout, env=env, stdin=stdin,
                                               uargs=userargs)
                elif ttype == "psql":
                    print("psql: ", exename)
                    code, out, err, pwd = run2(["psql", "-h", dbname, "-U", "$psqluser"], cwd=prgpath, timeout=10,
                                               env=env, stdin=stdin,
                                               uargs=userargs)
                    # code, out, err = run2(["sqlite3",dbname], cwd=prgpath, timeout=10, env=env, stdin = stdin, uargs = userargs, code='iso-8859-1')
                elif ttype == "cc":
                    print("c: ", exename)
                    code, out, err, pwd = run2([pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                               uargs=userargs)
                elif ttype == "c++":
                    print("c++: ", exename)
                    code, out, err, pwd = run2([pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                               uargs=userargs)
                elif ttype == "py":
                    print("py: ", exename)
                    code, out, err, pwd = run2(["python3", pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                               uargs=userargs)
                    if imgsource and pngname:
                        image_ok, e = copy_file(filepath + "/" + imgsource, pngname, True, is_optional_image)
                        if e: err = (str(err) + "\n" + str(e) + "\n" + str(out)).encode("utf-8")
                        # web["image"] = "http://tim-beta.it.jyu.fi/csimages/cs/" + basename + ".png"
                        print(is_optional_image, image_ok)
                        remove(imgsource)
                        if image_ok: web["image"] = "/csimages/cs/" + rndname + ".png"
                        
                elif ttype == "octave":
                    print("octave: ", exename)
                    code, out, err, pwd = run2(["octave", "-qf", pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                               uargs=userargs)
                    if imgsource and pngname:
                        image_ok, e = copy_file(filepath + "/" + imgsource, pngname, True, is_optional_image)
                        if e: err = (str(err) + "\n" + str(e) + "\n" + str(out)).encode("utf-8")
                        # web["image"] = "http://tim-beta.it.jyu.fi/csimages/cs/" + basename + ".png"
                        print(is_optional_image, image_ok)
                        remove(imgsource)
                        if image_ok: web["image"] = "/csimages/cs/" + rndname + ".png"
                    if wavsource and wavdest:
                        wav_ok, e = copy_file(filepath + "/" + wavsource, wavdest, True, is_optional_image)
                        if e: err = (str(err) + "\n" + str(e) + "\n" + str(out)).encode("utf-8")
                        print("WAV: ",is_optional_image, wav_ok, wavname,wavsource,wavdest)
                        remove(wavsource)
                        if wav_ok: web["wav"] = "/csimages/cs/" + wavname
                        
                elif ttype == "clisp":
                    print("clips: ", exename)
                    code, out, err, pwd = run2(["sbcl", "--script", pure_exename], cwd=prgpath, timeout=10, env=env,
                                               stdin=stdin,
                                               uargs=userargs)
                    # code, out, err = run(["sbcl", "--noinform --load " + exename + " --eval '(SB-EXT:EXIT)'"], timeout=10, env=env)
                    # code, out, err = run(["clisp",exename], timeout=10, env=env)
                elif ttype == "py2":
                    print("py2: ", exename)
                    code, out, err, pwd = run2(["python2", pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                               uargs=userargs)
                elif ttype == "text" or ttype == "xml" or ttype == "css":
                    print(ttype,": ", csfname)
                    showname = filename
                    if showname == "prg": showname = ""
                    code, out, err, pwd = (0, "".encode("utf-8"), ("tallennettu " + showname).encode("utf-8"), "")
                elif ttype == "fs":
                    print("Exe: ", exename)
                    code, out, err, pwd = run2(["mono", pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                               uargs=userargs)
                elif ttype == "md":
                    code, out, err = (0, "".encode(), "".encode())
                elif ttype == "js":
                    code, out, err = (0, "".encode(), "".encode())
                elif ttype == "simcir":
                    code, out, err = (0, "".encode(), "".encode())
                elif ttype == "sage":
                    code, out, err = (0, "".encode(), "".encode())
                elif ttype == "cs":
                    print("Exe: ", exename)
                    code, out, err, pwd = run2(["mono", pure_exename], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                                               uargs=userargs)
                elif ttype == "upload":
                    code, out, err = (0, "".encode(), "".encode())
                else:
                    out = "Unknown run type: " + ttype + "\n"

                if not err: give_points(points_rule, "run")
                print(code, out, err, pwd, compiler_output)

                if code == -9:
                    out = "Runtime exceeded, maybe loop forever\n" + out

                else:
                    print(err)
                    err = err.decode("utf-8") + compiler_output
                    if ttype == "fs":
                        err = err.replace(
                            "F# Compiler for F# 3.0 (Open Source Edition)\nFreely distributed under the Apache 2.0 Open Source License\n",
                            "")

                    if type('') != type(err): err = err.decode()
                    # if type(out) != type(''): out = out.decode()
                    # noinspection PyBroadException
                    try:
                        if out and out[0] in [254, 255]:
                            out = out.decode('UTF16')
                        elif type('') != type(out):
                            out = out.decode('utf-8-sig')
                    except:
                        out = out.decode('iso-8859-1')

        except Exception as e:
            print("run: ", e)
            code, out, err = (-1, "", str(e))  # .encode())

        if is_doc:
            pass  # jos doc, ei ajeta
        else:
            expect_output = get_points_rule(points_rule, is_test + "expectOutput", None)
            if expect_output:
                exout = re.compile(expect_output.rstrip('\n'), re.M)
                if exout.match(out): give_points(points_rule, "output", 1)

        return_points(points_rule, result)

        if delete_tmp: removedir(prgpath)

        out = out[0:20000]
        web["console"] = out
        web["error"] = err
        web["pwd"] = pwd.strip()

        result["web"] = web
        print(result)

        # Clean up
        # print("FILE NAME:", csfname)
        # remove(csfname)

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

if __debug__:
# if True:
    class ThreadedHTTPServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
        """Handle requests in a separate thread."""

    print("Debug mode/ThreadingMixIn")
else:
    class ThreadedHTTPServer(socketserver.ForkingMixIn, http.server.HTTPServer):
        """Handle requests in a separate thread."""
    print("Normal mode/ForkingMixIn")


if __name__ == '__main__':
    server = ThreadedHTTPServer(('', PORT), TIMServer)
    print('Starting server, use <Ctrl-C> to stop')
    server.serve_forever()
