from subprocess import check_output
from typing import Optional

import requests

from points import *
from run import *
from os.path import splitext
from cs_sanitizer import check_not_script
from base64 import b64encode

sys.path.insert(0, '/py')  # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon

from fileParams import *  # noqa

"""
Adding new language to csPlugin:

0. Install new compiler to cs/Dockerfile and build new Docker container from that
    - in /opt/tim directory run ./dc build csplugin
1. Add language name to languages list at the bottom of this file
    - remember to use lowercase letters
2. Add the language class starting with capital letter
3. Mimic some existing language when creating the new class
    - the simplest one is CC that works when just compiler name end extensions are enough to change 
4. Add language to csPlugin.ts languageTypes.runTypes list
   and to exactly same place the Ace editor highlighter name to languageTypes.aceModes
     - if there is a shorter language name in the list, add a new name before the
       shorter name.  For example there is "r", so every language name starting with "r"
       must be before "r" in the list (TODO: fix the list to be not dependent on the order)
"""

cmdline_whitelist = "A-Za-z\\-/\\.åöäÅÖÄ 0-9_"
filename_whitelist = "A-Za-z\\-/\\.åöäÅÖÄ 0-9_"


def sanitize_filename(s):
    global cmdline_whitelist
    return re.sub("[^" + filename_whitelist + "]", "", s)


def sanitize_cmdline(s):
    global cmdline_whitelist
    return re.sub("[^" + cmdline_whitelist + "]", "", s)


def illegal_cmdline_chars(_s):
    global cmdline_whitelist
    return ""
    # return re.sub("[" + cmdline_whitelist + "]", "", s)


def df(value, default):
    if value is not None:
        return value
    return default


def is_compile_error(out, err):
    return out.find("Compile error") >= 0 or err.find("Compile error") >= 0


class Language:
    def __init__(self, query: Optional[QueryClass], sourcecode):
        """
        :param self: object reference
        :param query: query to use
        :param sourcecode: source code as a string
        """
        self.query = query
        self.stdin = None
        self.query = query
        self.user_id = '--'
        if query.jso:
            self.user_id = df(query.jso.get('info'), {}).get('user_id', '--')
        self.rndname = generate_filename()
        self.delete_tmp = True
        self.opt = get_param(query, "opt", "")
        self.timeout = get_param(query, "timeout", 10)
        self.task_id = get_param(query, "taskID", "")
        self.doc_id, self.dummy = (self.task_id + "NONE.none").split(".", 1)
        self.no_x11 = get_json_param(query.jso, "markup", "noX11", False)
        self.env = dict(os.environ)
        self.userargs = get_json_param(query.jso, "input", "userargs", None)
        if not self.userargs:
            self.userargs = get_json_param(query.jso, "markup", "userargs", None)
        self.dockercontainer = get_json_param(query.jso, "markup", "dockercontainer", f"timimages/cs3:{CS3_TAG}")
        self.ulimit = get_param(query, "ulimit", None)
        self.savestate = get_param(query, "savestate", "")
        self.soucecode = sourcecode
        self.opt = get_param(query, "opt", "")
        self.is_optional_image = get_json_param(query.jso, "markup", "optional_image", False)
        self.hide_compile_out = False
        self.run_points_given = False  # Put this on if give run or test points
        self.readpoints_default = None  # what is default string for readpoints
        self.compile_commandline = ""
        self.just_compile = False
        self.imgname = get_param(query, "imgname", None)
        self.imgsource = get_imgsource(query)
        self.imgext = '.png'
        if self.imgsource:
            n, e = splitext(self.imgsource)
            if e:
                self.imgext = e

        # Check if user name or temp name

        self.upath = get_param(query, "path", "")  # from user/sql do user and /sql
        self.epath = "/" + self.doc_id
        if "/" in self.upath:  # if user/ do just user and ""
            self.upath, self.epath = self.upath.split("/", 1)
            if self.epath:
                self.epath = "/" + self.epath

        if self.upath == "user" and self.user_id:
            self.userpath = "user/" + hash_user_dir(self.user_id)
            self.mustpath = "/tmp/" + self.userpath
            self.basename = self.userpath + self.epath
            self.fullpath = "/tmp/" + self.basename  # check it is sure under userpath
            if not os.path.abspath(self.fullpath).startswith(self.mustpath):
                self.basename = self.userpath + "/ERRORPATH"
            self.delete_tmp = False
            mkdirs("/tmp/user")
            # print(self.task_id, self.doc_id, self.fullpath)
        else:
            # Generate random cs and exe filenames
            self.basename = "tmp/" + self.rndname
            mkdirs("/tmp/tmp")

        self.fullpath = "/tmp/" + self.basename  # check it is sure under userpath
        fname = "prg"
        # noinspection PyBroadException
        try:  # Let's do filename that depends on the taskID. Needed when many autorun plugins in the same page.
            tid = query.jso.get("taskID", "prg")
            i = tid.find(".")
            if i >= 0:
                tid = tid[i + 1:]
            asciified = re.sub(r"[^A-Za-z0-9_]", "", tid)
            # taskID variable may end with a dot (when the plugin doesn't have a task id),
            # so need to ensure asciified is not empty. Can also happen if task id has only non-ascii chars.
            if asciified:
                fname = asciified
        except AttributeError:
            pass
        self.filename = get_param(query, "filename", fname)
        self.ifilename = get_param(query, "inputfilename", "/input.txt")

        self.fileext = ""
        self.filedext = ""

        self.extension()

        self.sourcefilename = "/tmp/%s/%s%s" % (self.basename, self.filename, self.filedext)
        self.exename = "/tmp/%s/%s.exe" % (self.basename, self.filename)
        # self.sourcefilename = "./%s%s" % (self.filename, self.filedext)
        # self.exename = "./%s.exe" % self.filename
        self.pure_exename = "/home/agent/%s.exe" % self.filename
        self.inputfilename = "/tmp/%s/%s" % (self.basename, self.ifilename)
        self.prgpath = "/tmp/%s" % self.basename
        self.filepath = self.prgpath
        # self.imgsource = ""
        self.imgdest = ""

        self.before_code = get_param(query, "beforeCode", "")

    def extension(self):
        return

    def check_extension(self, extensions, ext, _exe):
        self.fileext = ext
        self.filedext = ext
        for ex in extensions:
            if self.filename.endswith(ex):
                self.fileext = ex.replace(".", "")
                self.filedext = ex
                self.filename = self.filename[:-len(ex)]
                break

    def get_cmdline(self, sourcecode):
        return ""

    def set_stdin(self, userinput):
        stdin_default = None

        is_input = get_json_param(self.query.jso, "input", "isInput", None)
        # print(isInput)
        if is_input:
            # print("Write input file: " + inputfilename)
            if not userinput:
                userinput = ""
            if self.inputfilename.find('input.txt') >= 0:
                stdin_default = 'input.txt'
            codecs.open(self.inputfilename, "w", "utf-8").write(userinput)
        self.stdin = get_param(self.query, "stdin", stdin_default)

    def before_save(self, s):
        return s

    def run(self, result, sourcelines, points_rule):
        return 0, "", "", ""

    def save(self, result): # when used without run, this can change result if needed
        return


    def deny_attributes(self):
        """
        :return: list of attribute not to be copied to typescipt/javascript client
        """
        return None

    def state_copy(self):
        """
        :return: list of state attribute names to be copied to .ts client code
        """
        return []

    def runner_name(self):
        """
        :return: runner name if it differs from cs-runner.  Then normally a new .ts file is also needed
        """
        return ""

    def js_files(self):
        """
        :return: list of needed js-files (maybe copiled from ts-files)
        """

    def css_files(self):
        """
        :return: list of needed css-files (maybe copiled from scss-files)
        """

    def convert(self, sourcelines):
        return 0, sourcelines, "", ""

    def modify_usercode(self, s):
        return s

    def clean_error(self, err):
        return err

    def can_give_task(self):
        return False

    def iframehtml(self, result, sourcelines, points_rule):
        return ""

    def runself(self, args, cwd=None, shell=None, kill_tree=None, timeout=None, env=None, stdin=None, uargs=None,
                code=None, extra=None, ulimit=None, no_x11=None, savestate=None, dockercontainer=None,
                no_uargs=False):
        if self.imgname:  # this should only come from cache run
            self.imgdest = self.imgname + self.imgext
        uargs = df(uargs, self.userargs)
        if no_uargs:
            uargs = None
        if self.just_compile:
            args = []
        code, out, err, pwddir = run2(args,
                                      cwd=df(cwd, self.prgpath),
                                      shell=df(shell, False),
                                      kill_tree=df(kill_tree, True),
                                      timeout=df(timeout, self.timeout),
                                      env=df(env, self.env),
                                      stdin=df(stdin, self.stdin),
                                      uargs=uargs,
                                      code=df(code, "utf-8"),
                                      extra=df(extra, ""),
                                      ulimit=df(ulimit, self.ulimit),
                                      no_x11=df(no_x11, self.no_x11),
                                      savestate=df(savestate, self.savestate),
                                      dockercontainer=df(dockercontainer, self.dockercontainer),
                                      compile_commandline=self.compile_commandline)
        if self.just_compile and not err:
            return code, "", "Compiled " + self.filename, pwddir
        return code, out, err, pwddir

    def copy_image(self, result, code, out, err, points_rule):
        if err:
            return out, err
        if code == -9:
            out = "Runtime exceeded, maybe loop forever\n" + out
            return out, err
        if self.imgsource and self.imgdest:
            _, imgext = splitext(self.imgsource)
            if not imgext:
                imgext = '.png'
            destname, destext = splitext(self.imgdest)
            destname = destname + imgext
            ims = self.imgsource
            if not ims.startswith("/"):
                ims = self.filepath + "/" + ims
            image_ok, e = copy_file(ims, destname, True, self.is_optional_image)
            if e:
                err = (str(err) + "\n" + str(e) + "\n" + str(out))
            # print(self.is_optional_image, image_ok)
            remove(self.imgsource)
            if image_ok:
                web = result["web"]
                if self.imgname:
                    web["image"] = self.imgdest
                else:
                    web["image"] = "/csgenerated/" + self.rndname + imgext
                give_points(points_rule, "run")
                self.run_points_given = True
        return out, err

    def get_default_before_open(self):
        return ''

class CS(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "csc"
        self.fileext = "cs"
        self.filedext = ".cs"
        self.sourcefilename = "/tmp/%s/%s.cs" % (self.basename, self.filename)
        self.exename = "/tmp/%s/%s.exe" % (self.basename, self.filename)

    def before_save(self, s):
        mockconsole = get_param(self.query, "mockconsole", True)
        if mockconsole:
            s = s.replace('System.Console.ReadLine', 'TIMconsole.ReadLine')
            s = s.replace('Console.ReadLine', 'TIMconsole.ReadLine')
        return s

    def get_cmdline(self, sourcecode):
        cmdline = "%s /r:System.Numerics.dll /out:%s %s /cs/jypeli/TIMconsole.cs" % (
            self.compiler, self.exename, self.sourcefilename)
        return cmdline

    def run(self, result, sourcelines, points_rule):
        # result["tim_info"]["valid"] = False  # kokeilu että valit toimii
        # result["tim_info"]["validMsg"] = "Et voi enää saada pisteitä kun katsoit vastauksen"
        return self.runself(["mono", "-O=all", self.pure_exename])

class Jypeli(CS):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.imgsource = "/tmp/%s/output.bmp" % self.basename
        self.pure_bmpname = "./%s.bmp" % self.filename
        self.imgdest = "/csgenerated/%s.png" % self.rndname
        self.pure_exename = u"{0:s}.exe".format(self.filename)
        self.pure_mgdest = u"{0:s}.png".format(self.rndname)

    def get_cmdline(self, sourcecode):
        mainfile = "/cs/jypeli/Ohjelma.cs"
        if sourcecode.find(" Main(") >= 0:
            mainfile = ""
        else:
            classname = find_cs_class(sourcecode)
            if classname != "Peli":
                maincode = codecs.open(mainfile, 'r', "utf-8").read()
                maincode = re.sub("Peli", classname, maincode, flags=re.M)
                mainfile = "/tmp/%s/%s.cs" % (self.basename, "Ohjelma")
                codecs.open(mainfile, "w", "utf-8").write(maincode)

        # cmdline = "%s /out:%s /r:/cs/jypeli/Jypeli.dll
        # /r:/cs/jypeli/MonoGame.Framework.dll /r:/cs/jypeli/Jypeli.Physics2d.dll
        # /r:/cs/jypeli/OpenTK.dll /r:/cs/jypeli/Tao.Sdl.dll /r:System.Drawing.dll
        # /cs/jypeli/Ohjelma.cs %s" % (
        cmdline = ("%s /out:%s /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/MonoGame.Framework.dll "
                   "/r:/cs/jypeli/Jypeli.Physics2d.dll  "
                   "/r:System.Numerics.dll /r:System.Drawing.dll %s %s") % (
                      self.compiler, self.exename, mainfile, self.sourcefilename)
        # /r:/cs/jypeli/Tao.Sdl.dll  /r:/cs/jypeli/OpenTK.dll
        return cmdline

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["mono", self.pure_exename],
                                              ulimit=df(self.ulimit, "ulimit -f 80000"))
        if err.find("Compile") >= 0:
            return code, out, err, pwddir
        err = re.sub("^ALSA.*\n", "", err, flags=re.M)
        err = re.sub("^W: \\[pulse.*\n", "", err, flags=re.M)
        err = re.sub("^AL lib:.*\n", "", err, flags=re.M)
        out = re.sub("^Could not open AL device - OpenAL Error: OutOfMemory.*\n", "", out, flags=re.M)

        wait_file(self.imgsource)
        run(["convert", "-flip", self.imgsource, self.imgdest], cwd=self.prgpath, timeout=20)
        remove(self.imgsource)
        # print("*** Screenshot: https://tim.it.jyu.fi/csgenerated/%s\n" % self.pure_imgdest)
        out = re.sub('Number of joysticks:.*\n.*', "", out)
        if code == -9:
            out = "Runtime exceeded, maybe loop forever\n" + out
        else:
            web = result["web"]
            if self.imgname:
                web["image"] = self.imgdest
            else:
                web["image"] = self.imgdest
            give_points(points_rule, "run")
            self.run_points_given = True
        if self.delete_tmp:
            remove(self.sourcefilename)
            remove(self.exename)
        return code, out, err, pwddir


class CSComtest(CS):
    nunit = None

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.testdll = u"./{0:s}Test.dll".format(self.filename)
        self.hide_compile_out = True

    def get_cmdline(self, sourcecode):
        testcs = "/tmp/%s/%sTest.cs" % (self.basename, self.filename)
        if not CSComtest.nunit:
            frms = os.listdir("/usr/lib/mono/gac/nunit.framework/")
            CSComtest.nunit = "/usr/lib/mono/gac/nunit.framework/" + frms[0] + "/nunit.framework.dll"
        jypeliref = ("/r:System.Numerics.dll /r:/cs/jypeli/Jypeli.dll /r:/cs/jypeli/MonoGame.Framework.dll "
                     "/r:/cs/jypeli/Jypeli.Physics2d.dll /r:/cs/jypeli/OpenTK.dll "
                     "/r:/cs/jypeli/Tao.Sdl.dll /r:System.Drawing.dll")
        cmdline = ("java -jar /cs/java/cs/ComTest.jar nunit %s && %s /out:%s /target:library " +
                   jypeliref +
                   " /reference:%s %s %s /cs/jypeli/TIMconsole.cs") % \
                  (self.sourcefilename, self.compiler, self.testdll, CSComtest.nunit, self.sourcefilename, testcs)
        return cmdline

    def run(self, result, sourcelines, points_rule):
        eri = -1
        code, out, err, pwddir = self.runself(["nunit-console", "-nologo", "-nodots", self.testdll])
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
        if eri < 0:
            eri = out.find("Test Failure")
        if eri < 0:
            eri = out.find("Test Error")
        if is_compile_error(out, err):
            return code, out, err, pwddir
        give_points(points_rule, "testrun")
        self.run_points_given = True
        web = result["web"]
        web["testGreen"] = True
        if eri >= 0:
            web["testGreen"] = False
            web["testRed"] = True
            lni = out.find(", line ")
            if lni >= 0:  # and not nocode:
                lns = out[lni + 7:]
                lns = lns[0:lns.find("\n")]
                lnro = int(lns)
                # lines = codecs.open(sourcefilename, "r", "utf-8").readlines()
                lines = sourcelines.split("\n")
                # print("Line nr: "+str(lnro))
                # # out += "\n" + str(lnro) + " " + lines[lnro - 1]
                web["comtestError"] = str(lnro) + " " + lines[lnro - 1]
        else:
            give_points(points_rule, "test")
            self.run_points_given = True
        return code, out, err, pwddir


class Shell(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.sh" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = "/home/agent/%s.sh" % self.filename
        self.fileext = "sh"

    # noinspection PyBroadException
    def run(self, result, sourcelines, points_rule):
        try:
            os.system('chmod +x ' + self.exename)
        except OSError:
            print("Ei oikeuksia: " + self.exename)
        extra = ""  # ""cd $PWD\nsource "
        try:
            code, out, err, pwddir = self.runself([self.pure_exename], extra=extra)
            # print(pwddir)
        except OSError as e:
            print(e)
            code, out, err, pwddir = (-1, "", str(e), "")
        return code, out, err, pwddir


class Ping(Shell):
    def run(self, result, sourcelines, points_rule):
        return 0, "Ping", "", ""


class Java(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.classpath = get_param(query, "-cp", ".") + ":$CLASSPATH"
        self.fileext = "java"
        # print("classpath=", self.classpath)
        self.package, self.classname = find_java_package(sourcecode)
        self.javaclassname = self.classname
        if not self.classname:
            self.classname = "Prg"
        if self.package:
            self.filepath = self.prgpath + "/" + self.package.replace(".", "/")
            mkdirs(self.filepath)
            self.javaclassname = self.package + "." + self.classname

        self.filename = self.javaclassname + ".java"
        self.javaname = self.filepath + "/" + self.classname + ".java"
        self.sourcefilename = self.javaname

    def get_cmdline(self, sourcecode):
        return "javac --module-path /javafx-sdk-11.0.1/lib --add-modules=ALL-MODULE-PATH -Xlint:all -cp %s %s" % (
            self.classpath, self.javaname)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["java", "--module-path", "/javafx-sdk-11.0.1/lib",
                                               "--add-modules=ALL-MODULE-PATH", "-cp",
                                               self.classpath, self.javaclassname],
                                              ulimit=df(self.ulimit, "ulimit -f 10000"))
        return code, out, err, pwddir


class Kotlin(Java):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.fileext = "kt"
        self.filename = self.classname + "." + self.fileext
        self.javaname = self.filepath + "/" + self.filename
        self.sourcefilename = self.javaname
        self.jarname = self.classname + ".jar"

    def get_cmdline(self, sourcecode):
        return "kotlinc  %s -include-runtime -d %s" % (self.filename, self.jarname)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["java", "-jar", self.jarname],
                                              ulimit=df(self.ulimit, "ulimit -f 10000"))
        return code, out, err, pwddir


def check_comtest(self, ttype, code, out, err, result, points_rule):
    if is_compile_error(out, err):
        return out, err
    eri = -1
    out = remove_before("Execution Runtime:", out)
    if code == -9:
        out = "Runtime exceeded, maybe loop forever\n" + out
        eri = 0
    # print(javaclassname+"\n")
    if ttype == "junit":
        out = re.sub("[\t ]*at " + self.javaclassname, "ERROR: " + self.javaclassname, out,
                     flags=re.M)  # prevent remove by next "at"-word
    out = re.sub("\\s+at .*\n", "\n", out, flags=re.M)
    out = re.sub("\n+", "\n", out, flags=re.M)
    out = re.sub("Errors and Failures.*\n", "", out, flags=re.M)
    out = re.sub(self.prgpath + "/", "", out, flags=re.M)
    out = out.strip(' \t\n\r')
    if ttype == "junit":
        out = re.sub("java:", "java line: ", out,
                     flags=re.M)  # To get line: also in JUnit case where error is in format java:39
    if eri < 0:
        eri = out.find("FAILURES")  # jcomtest
    if eri < 0:
        eri = out.find("Test error")  # ccomtest
    if eri < 0:
        eri = out.find("ERROR:")  # ccomtest compile error
    p = re.compile('Xlib: {2}extension "RANDR" missing on display ":1"\\.\n')
    err = p.sub("", err)
    web = result["web"]
    web["testGreen"] = True
    give_points(points_rule, "testrun")
    self.run_points_given = True
    if eri >= 0:
        web["testGreen"] = False
        web["testRed"] = True
        lni = out.find(" line: ")
        cterr = ""
        sep = ""
        while lni >= 0:
            lns = out[lni + 7:]
            lnro = getint(lns)
            lines = codecs.open(self.sourcefilename, "r", "utf-8").readlines()
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
        self.run_points_given = True
    return out, err


class JComtest(Java):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.testcs = self.filepath + "/" + self.classname + "Test.java"
        self.testdll = self.javaclassname + "Test"
        self.hide_compile_out = True

    def get_cmdline(self, sourcecode):
        return "java comtest.ComTest %s && javac %s %s" % (self.sourcefilename, self.sourcefilename, self.testcs)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["java", "org.junit.runner.JUnitCore", self.testdll], no_uargs=True)
        out, err = check_comtest(self, "jcomtest", code, out, err, result, points_rule)
        return code, out, err, pwddir


class JUnit(Java):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)

    def get_cmdline(self, sourcecode):
        return "javac %s" % self.javaname

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["java", "org.junit.runner.JUnitCore", self.javaclassname])
        out, err = check_comtest(self, "junit", code, out, err, result, points_rule)
        return code, out, err, pwddir


class Graphics(Java):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.imgsource = "%s/run/capture.png" % self.prgpath
        self.imgdest = "/csgenerated/%s.png" % self.rndname

    def run(self, result, sourcelines, points_rule):
        a = []
        delay = get_json_param(self.query.jso, "markup", "delay", "0")
        if delay is not None:
            a.extend(["--delay", str(delay)])
        rect = get_json_param(self.query.jso, "markup", "rect", None)
        if rect:
            a.extend(["--rect", rect])
        # print(a)
        runcmd = ["java", "--module-path", "/javafx-sdk-11.0.1/lib",
                  "--add-modules=ALL-MODULE-PATH", "sample.Runner",
                  self.javaclassname, "--captureName", "run/capture.png"]
        runcmd.extend(a)
        code, out, err, pwddir = self.runself(runcmd, cwd=self.prgpath)
        out, err = self.copy_image(result, code, out, err, points_rule)
        err = re.sub('Xlib: {2}extension "RANDR" missing on display ":1"\\.\n', "", err)
        return code, out, err, pwddir


class Scala(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.scala" % (self.basename, self.filename)
        self.classname = self.filename
        self.fileext = "scala"

    def get_cmdline(self, sourcecode):
        return "scalac %s" % self.sourcefilename

    def run(self, result, sourcelines, points_rule):
        return self.runself(["scala", self.classname], ulimit=df(self.ulimit, "ulimit -f 10000"))


class CC(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "gcc"

    def extension(self):
        self.check_extension([".h", ".c", ".cc"], ".c", ".exe")

    def get_cmdline(self, sourcecode):
        return self.compiler + " -Wall %s %s -o %s -lm" % (self.opt, self.sourcefilename, self.exename)

    def run(self, result, sourcelines, points_rule):
        return self.runself([self.pure_exename])


class CPP(CC):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "g++ -std=c++14"

    def extension(self):
        self.check_extension([".h", ".hpp", ".hh", ".cpp", ".cc"], ".cpp", ".exe")


class CComtest(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.cpp" % (self.basename, self.filename)
        self.fileext = "cpp"
        self.testcs = u"{0:s}.cpp".format(self.filename)
        self.hide_compile_out = True

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["java", "-jar", "/cs/java/comtestcpp.jar", "-nq", self.testcs])
        out, err = check_comtest(self, "ccomtest", code, out, err, result, points_rule)
        return code, out, err, pwddir


class Fortran(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        extension = os.path.splitext(self.filename)[1]
        if extension.startswith(".f"):
            self.fileext = extension[1:]
            self.sourcefilename = "/tmp/%s/%s" % (self.basename, self.filename)
        else:
            self.fileext = 'f'
            self.sourcefilename = "/tmp/%s/%s.f" % (self.basename, self.filename)
        self.compiler = "gfortran"

    def get_cmdline(self, sourcecode):
        return self.compiler + " -Wall %s %s -o %s -lm" % (self.opt, self.sourcefilename, self.exename)

    def run(self, result, sourcelines, points_rule):
        return self.runself([self.pure_exename])


class PY3(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.py" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = "./%s.py" % self.filename
        self.fileext = "py"
        self.imgdest = "/csgenerated/%s.png" % self.rndname

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["python3", self.pure_exename])
        if err:
            err = re.sub("/usr/lib/python3/dist-packages/matplotlib/font_manager(.*\n)*.*This may take a moment.'\\)",
                         "", err, flags=re.M)
            err = err.strip()
            if err:
                return code, out, err, pwddir
        out, err = self.copy_image(result, code, out, err, points_rule)
        err = err.strip()
        return code, out, err, pwddir


class PY2(PY3):
    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["python2", self.pure_exename])
        out, err = self.copy_image(result, code, out, err, points_rule)
        return code, out, err, pwddir


class Swift(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.swift" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = "./%s.swift" % self.filename
        self.fileext = "swift"
        self.imgdest = "/csgenerated/%s.png" % self.rndname
        self.imgsource = get_imgsource(query)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["swift", self.pure_exename],
                                              ulimit=df(self.ulimit, "ulimit -f 80000 -t 10 -s 600"))
        out, err = self.copy_image(result, code, out, err, points_rule)
        return code, out, err, pwddir


class Lua(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.lua" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = "./%s.lua" % self.filename
        self.fileext = "lua"
        self.imgdest = "/csgenerated/%s.png" % self.rndname
        self.imgsource = get_imgsource(query)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["lua", self.pure_exename])
        out, err = self.copy_image(result, code, out, err, points_rule)
        return code, out, err, pwddir


class CLisp(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.lisp" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.fileext = "lisp"
        self.pure_exename = u"./{0:s}.lisp".format(self.filename)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["sbcl", "--script", self.pure_exename])
        # p = re.compile("WARNING:\n"
        #               "Couldn't re-execute SBCL with proper personality flags (/proc isn't mounted? setuid?)\n"
        #               "Trying to continue anyway.")
        err = re.sub("WARNING:.*\n.*\nTrying to continue anyway.\n", "", err, flags=re.M)
        return code, out, err, pwddir


class Quorum(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.q" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.fileext = "q"
        self.pure_exename = u"./{0:s}.jar".format(self.filename)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["/cs/java/quorum/runquorum", self.filename])
        out = re.sub("Quorum 7.0\nBuild Successful\n", "", out, flags=re.M)
        return code, out, err, pwddir


class Text(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        if self.userargs:
            self.filename = self.userargs
        self.sourcefilename = "/tmp/%s/%s" % (self.basename, self.filename)
        self.pure_exename = u"./{0:s}".format(self.filename)

    def run(self, result, sourcelines, points_rule):
        showname = self.filename
        if showname == "prg":
            showname = ""
        saved_text = get_param(self.query, "savedText", "Saved {0}")
        code, out, err, pwddir = (0, "", saved_text.format(showname), "")
        return code, out, err, pwddir


class XML(Text):
    def run(self, result, sourcelines, points_rule):
        convert = self.query.jso.get('markup', {}).get('convert', None)
        if not convert:
            return super(XML, self).run(result, sourcelines, points_rule)

        language_class = languages.get(convert.lower(), Language)
        language = language_class(self.query, sourcelines)
        return language.convert(sourcelines)


class Css(Text):
    pass


class JJS(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.js" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = u"./{0:s}.js".format(self.filename)
        self.fileext = "js"
        if self.before_code == "":  # Jos ei ole valmista koodia, niin tehdään konsoli johon voi tulostaa
            self.before_code = ('var console={};'
                                'console.log = function(s) {'
                                '    var res = "", sep = "";'
                                '    for (var i=0; i<arguments.length; i++) { res += sep + arguments[i]; sep = " "; } '
                                '    print(res);'
                                '};')

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["jjs", self.pure_exename])
        return code, out, err, pwddir


class JS(Language):
    def run(self, result, sourcelines, points_rule):
        return 0, "", "", ""


class Glowscript(JS):
    pass


class Processing(JS):
    pass


class WeScheme(JS):

    def runner_name(self):
        return "cs-wescheme-runner"


class VPython(JS):
    pass


class SQL(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.sql" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = u"{0:s}.sql".format(self.filename)
        self.fileext = "sql"
        self.dbname = get_param(query, "dbname", "db")
        self.stdin = self.pure_exename

    def set_stdin(self, userinput):
        self.stdin = self.pure_exename

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["sqlite3", self.dbname])
        if not out:
            empty_result = get_param(self.query, "emptyResult", "No result")
            out = empty_result
        return code, out, err, pwddir


class PSQL(SQL):
    def run(self, result, sourcelines, points_rule):
        return self.runself(["psql", "-h", self.dbname, "-U", "$psqluser"])


class Alloy(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.als" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = "./%s.als" % self.filename
        self.imgsource = "%s/mm.png" % self.prgpath
        self.imgdest = "/csgenerated/%s.png" % self.rndname

    def run(self, result, sourcelines, points_rule):
        runcmd = ["java", "-cp", "/cs/java/alloy-dev.jar:/cs/java", "RunAll", self.pure_exename]
        code, out, err, pwddir = self.runself(runcmd)
        out, err = self.copy_image(result, code, out, err, points_rule)
        return code, out, err, pwddir


class Run(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = "/home/agent/%s" % self.filename
        self.imgdest = "/csgenerated/%s.png" % self.rndname
        self.imgsource = get_imgsource(query)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself([])
        uargs = self.userargs
        cmd = shlex.split(get_param(self.query, "cmd", "ls -la") + " " + self.pure_exename)
        extra = get_param(self.query, "cmds", "").format(self.pure_exename, uargs)
        if extra != "":
            cmd = []
            uargs = ""
        # print("run: ", cmd, extra, self.pure_exename, self.sourcefilename)
        # print("Run1: ", self.imgsource, self.imgdest)
        try:
            code, out, err, pwddir = self.runself(cmd, uargs=uargs, extra=extra)
        except Exception as e:
            print(e)
            code, out, err = (-1, "", str(e))
        # print("Run2: ", self.imgsource, self.imgdest)
        out, err = self.copy_image(result, code, out, err, points_rule)
        return code, out, err, pwddir


class MD(Language):
    pass


class HTML(Language):
    pass


class SimCir(Language):
    pass


class Sage(Language):

    def runner_name(self):
        return "sage-runner"


class Stack(Language):
    def can_give_task(self):
        return True

    def runner_name(self):
        return "stack-runner"

    def js_files(self):
        return ["/cs/js/build/stack.js", "/cs/stack/ServerSyncValues.js"]

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.txt" % (self.basename, self.filename)
        self.fileext = "txt"
        self.readpoints_default = 'Score: (.*)'
        self.delete_tmp = False

    def modify_usercode(self, s):
        if not s.startswith("{"):
            return s
        s = s.replace("&quot;", '"')
        js = json.loads(s)
        res = ''
        for key in js:
            res += js[key] + "\n"
        return res

    def run(self, result, sourcelines, points_rule):
        get_task = self.query.jso.get("input", {}).get("getTask", False)
        url = "http://stack-api-server/api/endpoint.php"
        data = self.query.jso.get("input").get("stackData")
        stack_data = self.query.jso.get('markup').get('-stackData')
        if not stack_data:
            stack_data = self.query.jso.get('markup').get('stackData')
        if not stack_data:
            err = "stackData missing from plugin"
            return 0, "", err, ""
        seed = stack_data.get("seed", 0)
        userseed = seed
        state = self.query.jso.get("state", {})
        if isinstance(state, dict):
            userseed = state.get("seed", seed)
        nosave = self.query.jso.get('input', {}).get('nosave', False)
        stack_data["seed"] = userseed
        q = stack_data.get("question", "")
        if not isinstance(q, str):
            q = json.dumps(q)
            stack_data["question"] = q

        if not self.query.jso.get('markup').get('stackjsx') and q.find("[[jsxgraph") >= 0:  # make jsxgraph replace
            q = q.replace('[[jsxgraph ', '[[jsxgraphapi ')
            q = q.replace('[[jsxgraph]]', '[[jsxgraphapi]]')
            q = q.replace('[[/jsxgraph]]', '[[/jsxgraphapi]]')
            stack_data["question"] = q

        if nosave or get_task:
            stack_data['score'] = False
            stack_data['feedback'] = False
        stack_data["answer"] = data.get("answer")
        stack_data["prefix"] = data.get("prefix")
        stack_data["verifyvar"] = data.get("verifyvar", "")
        stack_data["ploturl"] = '/stackserver/plots/'
        if stack_data["verifyvar"]:
            stack_data["score"] = False
        else:
            save = result["save"]
            save["seed"] = userseed

        for key in stack_data:
            s = stack_data[key]
            if True:  # TODO: here maybe a list of checked fields
                try:
                    check_not_script(s)
                except Exception as e:
                    result['nosave'] = True
                    return 1, "", str(e) + " " + str(key) + ": " + html.escape(str(s)), ""

        r = requests.post(url=url, data=json.dumps(stack_data))  # json.dumps(data_to_send, cls=TimJsonEncoder))
        # r = requests.get(url="http://stack-test-container/api/endpoint.html")

        try:
            r = r.json()
        except json.JSONDecodeError:
            return 1, "", str(r.content.decode()), ""
        out = "Score: %s" % r.get("score", 0)
        # r['questiontext'] = tim_sanitize(r['questiontext'])

        if nosave:
            out = ""
            result["nosave"] = True
        web = result["web"]
        web["stackResult"] = r
        return 0, out, "", ""

    def convert(self, sourcelines):
        url = "http://stack-api-server/api/xmltoyaml.php"
        data = {'xml': sourcelines}
        r = requests.post(url=url, data=json.dumps(data))
        r = r.json()
        return 0, r.get('yaml'), "", ""


GEOGEBRA_DEFAULT_SRC_HTML = """
<!DOCTYPE html>
<html>
<head>
  <title>Apps with Toolbar: Graphing Calculator</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
</head>
<body>
<div>
<script type="text/javascript" src="https://cdn.geogebra.org/apps/deployggb.js"></script>
<script type="text/javascript" src="/cs/geogebra/timgeo.js"></script>

<script type="text/javascript">
function perspective(p){
    updateHelp(p);
    ggbApplet.setPerspective(p);
}

var ggbApplet;

var P = {
borderColor: "#FFFFFF",
preventFocus: true,
//GEOWIDTH
//GEOHEIGHT
//GEOMATERIALID
//GGBBASE64
//GEOFILENAME
};


P.getData = function(){ 
   return {"data": ggbApplet.getBase64()}; 
}

P.setDataInit = function (api, geostate) {
    timgeo.setState(api, geostate);
}

P.setData = function(geostate) {
    P.setDataInit(ggbApplet, geostate);
}

//GEOJAVASCRIPT

var geostate = GEOSTATE;

P.appletOnLoad = function(api) {
    if ( !P.setDataInit ) return;
    ggbApplet = api;
    var g = atob(geostate);
    var state = JSON.parse(g);
    // timgeo.setState(api, state);
    P.setDataInit(api, state);
}



var applet = new GGBApplet(P, '5.0', 'geogebra_container');
//  when used with Math Apps Bundle, uncomment this:
//  applet.setHTML5Codebase('GeoGebra/HTML5/5.0/web3d/');

  window.onload = function() { applet.inject('geogebra_container');
}


function getData(){ 
    if ( P.getData )
        return P.getData();
}

function setData(geostate) {
    if ( P.setData )
        P.setData(geostate);
}

</script>

GEOPREHTML

<div id="geogebra_container"></div>

GEOPOSTHTML
</div>
</body>
</html>
"""

GEOGEBRA_PARAMETERS_INIT = """
P.appName = "graphing";
P.width = 1200;
P.height = 600;
P.showMenuBar = true;
P.showAlgebraInput = true;
P.showToolBar = true;
P.customToolBar = "0 77 73 62 | 1 501 67 , 5 19 , 72 75 76 | 2 15 45 , 18 65 , 7 37 | 4 3 8 9 , 13 44 , 58 , 47 | 16 51 64 , 70 | 10 34 53 11 , 24  20 22 , 21 23 | 55 56 57 , 12 | 36 46 , 38 49  50 , 71  14  68 | 30 29 54 32 31 33 | 25 17 26 60 52 61 | 40 41 42 , 27 28 35 , 6";
P.showToolBarHelp = true;
P.showResetIcon = true;
P.enableLabelDrags = true;
P.enableShiftDragZoom = true;
P.enableRightClick = false;
P.errorDialogsActive = false;
P.useBrowserForJS = false;
P.allowStyleBar = false;
P.preventFocus = true;
P.showZoomButtons = true;
P.capturingThreshold = 3;
{}
"""

GEOGEBRA_TOOL_HTML = """
<script type="text/javascript">
function objName() { return document.getElementById("objName").value; }
function resultArea() { return document.getElementById("resultArea"); }
function setArea(s) { resultArea().value = s; }
function helpArea() { return document.getElementById("helpText"); }
function setHelp(s) { helpArea().value = s; }
var api;
function evalJS(s) {
   api = ggbApplet;
   try {
       var res = eval(s);
       if ( res ) return res;
       return "";
   } catch(err) {
       return err.message;
   }
}
function loadMaterial() {
    let mat = document.getElementById("materialId").value;
    if ( mat.indexOf("/") >= 0 ) {
        P['filename'] = mat;
        delete P['material_id'];
        setHelp('"filename": "' + mat +'"')
    } else {
        P['material_id'] = mat;
        delete P['filename'];
        setHelp('"material_id": "' + mat +'"')
    }
    delete P['ggbBase64'];
    applet = new GGBApplet(P, '5.0', 'geogebra_container');
    applet.inject('geogebra_container');
}
</script>

<p>Eval code:
<textarea name="evalarea" id="evalarea" cols=40 rows=3></textarea>
Eval:
<a href="javascript:;" onclick="ggbApplet.evalCommand(document.getElementById('evalarea').value); setHelp('ggbApplet.evalCommand(cmds)')">commands</a>
<a href="javascript:;" onclick="setArea(ggbApplet.evalCommandGetLabels(document.getElementById('evalarea').value)); setHelp('ggbApplet.evalCommandGetLabels(cmds)')">labels</a>
<a href="javascript:;" onclick="setArea(ggbApplet.evalCommandCAS(document.getElementById('evalarea').value)); setHelp('ggbApplet.evalCommandCAS(cmds)')">CAS</a>
<a href="javascript:;" onclick="setArea(evalJS(document.getElementById('evalarea').value)); setHelp('eval(cmds)')">JS</a>
</p>

<ul>
  <li>Get constraction:
    <a href="javascript:;" onclick="setArea(ggbApplet.getBase64()); setHelp('ggbApplet.getBase64()')">GGB</a>
    <a href="javascript:;" onclick="setArea(ggbApplet.getXML()); setHelp('ggbApplet.getXML()')">XML</a>
    <a href="javascript:;" onclick="setArea(ggbApplet.getAllObjectNames()); setHelp('ggbApplet.getAllObjectNames()')">Names</a>
    <a href="javascript:;" onclick="setArea(timgeo.getConstructionState(ggbApplet)); setHelp('timgeo.getConstructionState(ggbApplet)')">State</a>
    <a href="javascript:;" onclick="setArea(timgeo.getCommands(ggbApplet)); setHelp('timgeo.getCommands(ggbApplet)')">Commands</a>
  </li>
  <li>Obj name:
    <input id="objName" size="6"/>
    Get obj:
    <a href="javascript:;" onclick="setArea(timgeo.getObjXML(ggbApplet,objName())); setHelp('timgeo.getObjXML(ggbApplet,\\''+objName()+'\\')')">XML</a>
    <a href="javascript:;" onclick="setArea(timgeo.getObjCommand(ggbApplet,objName())); setHelp('timgeo.getObjCommand(ggbApplet,\\''+objName()+'\\')')">Command</a>
    Value:
    <a href="javascript:;" onclick="setArea(timgeo.getNumberValue(ggbApplet,objName())); setHelp('timgeo.getNumberValue(ggbApplet,\\''+objName()+'\\')')">Number</a>
    <a href="javascript:;" onclick="setArea(timgeo.getObjValue(ggbApplet,objName())); setHelp('timgeo.getObjValue(ggbApplet,\\''+objName()+'\\')')">String</a>
    <a href="javascript:;" onclick="setArea(timgeo.getPureValue(ggbApplet,objName())); setHelp('timgeo.getPureValue(ggbApplet,\\''+objName()+'\\')')">Pure</a>
  </li>
</ul>
  JS: <input id="helpText" title="Javascript command" size="70"/>
  <a href="javascript:timgeo.copyArea(helpArea())">Copy</a>
<br>
<textarea name="resultArea" id="resultArea" cols=90 rows=10></textarea>
<ul>
   <li><a href="javascript:timgeo.copyArea(resultArea())">Copy area</a></li>
   <li>Set constraction:
      <a href="javascript:;" onclick="ggbApplet.setBase64(resultArea().value); setHelp('ggbApplet.setBase64(ggb)')">GGB</a>
      <a href="javascript:;" onclick="ggbApplet.setXML(resultArea().value); setHelp('ggbApplet.setXML(xml)')">XML</a>
      <a href="javascript:;" onclick="ggbApplet.reset(); setHelp('ggbApplet.reset()')">Reset</a>
      &nbsp;<input id="materialId" size="45" title="Write material ID or GGB-file URL"/>
      <a href="javascript:;" onclick="loadMaterial()">Load</a>
   </li>
   <li><a href="javascript:;" onclick="ggbApplet.evalXML(resultArea().value); setHelp('ggbApplet.evalXML(xml)')">Set Obj XML</a></li>
</ul>
"""

def get_by_id(jso, item_id, default=None):
    val = jso.get(item_id, None)
    if val is None:
        val = jso.get("-" + item_id, default)
    return val


def html_change(s, old, jso, item_id, repfmt, default, valdef = None, delta = 0):
    """
    Korvataan s:ssä oleva sana old jsonin avaimesta item_id löytyvällä
    tekstillä kun se muokataan repfmt formaatilla.  Mikäli avainta
    ei löydy (edes - alkuisena), käytetään korvauksena default-jonoa.
    Jos default == None ei tehdä mitään
    :param s: jono josta korvataan
    :param old: teksti jota etsitään
    :param jso: dict josta etsitään avainta (kokeillaan myös - alkuun)
    :param item_id: avain jonka kohdalta korvaava teksti otetaan
    :param repfmt: muotoiluohje miten korvaava teksti muotoillaan
    :param default: oletus jos avainta ei löydy, None => ei korvata mitään
    :param valdef: mitä arvoa käytetään tekstille jos ei löydy
    :param delta: minkä verran arvoa muutetaan kun se laitetaan
    :return: korvattu jono
    """
    try:
        val = get_by_id(jso, item_id, valdef)
        n = default
        if val is not None:
            if delta:
                val = int(val) + delta
            n = repfmt.format(val)
        if n is None:
            return s
        return s.replace(old, n)
    except ValueError:
        return s


class Geogebra(Language):
    global GEOGEBRA_DEFAULT_SRC_HTML
    global GEOGEBRA_PARAMETERS_INIT
    global GEOGEBRA_TOOL_HTML

    def get_default_before_open(self):
        return '<div class="defBeforeOpen"><p>Open GeoGebra</p></div>'

    def runner_name(self):
        return "geogebra-runner"

    def js_files(self):
        return ["/cs/js/build/geogebra.js"]

    def can_give_task(self):
        return True

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.txt" % (self.basename, self.filename)
        self.fileext = "txt"
        self.readpoints_default = 'Score: (.*)'
        self.delete_tmp = False

    def modify_usercode(self, s):
        if not s.startswith("{"):
            return s
        s = s.replace("&quot;", '"')
        js = json.loads(s)
        res = ''
        for key in js:
            res += js[key] + "\n"
        return res

    def run(self, result, sourcelines, points_rule):
        self.save(result)
        return 0, "GeoGebra saved", "", ""

    def save(self, result):
        data = dict(self.query.jso["input"])
        if 'type' in data:
            del data['type']
        result["save"] = data
        return 0, "GeoGebra saved", "", ""

    def deny_attributes(self):
        return {"srchtml":"",
                "filename": "",
                "prehtml": "",
                "posthtml": "",
                "data":"",
                "javascript": "",
                "commands": "",
                "objxml": ""
        }

    def state_copy(self):
        return ["message"]

    def iframehtml(self, result, sourcelines, points_rule):
        ma = self.query.jso['markup']
        jsformat = "{}"
        jsdef = None
        postdef = None
        if get_by_id(ma, 'tool', False):
            jsformat = GEOGEBRA_PARAMETERS_INIT
            jsdef = ""
            postdef = GEOGEBRA_TOOL_HTML

        srchtml = get_by_id(ma, 'srchtml', GEOGEBRA_DEFAULT_SRC_HTML)
        srchtml = html_change(srchtml, "//GEOMATERIALID", ma, "material_id", 'material_id: "{}",', None)
        srchtml = html_change(srchtml, "//GEOFILENAME", ma, "filename", 'filename: "{}",', None)
        srchtml = html_change(srchtml, "//GEOWIDTH", ma, "width", 'width: {},', "", "800", -10)
        srchtml = html_change(srchtml, "//GEOHEIGHT", ma, "height", 'height: {},', "", "450", -20)  # prevent scrollbar
        srchtml = html_change(srchtml, "//GEOJAVASCRIPT", ma, "javascript", jsformat, None, jsdef)
        srchtml = html_change(srchtml, "GEOPREHTML", ma, "prehtml", '{}', "")
        srchtml = html_change(srchtml, "GEOPOSTHTML", ma, "posthtml", '{}', "", postdef)

        data = get_by_id(ma, "data", "")
        state = self.query.jso.get("state", {})
        if state is None:
            state = {}
        udata = state.get("data", None)
        if udata:
            data = udata
        if data:
            if data[0] == '<': # is XML?
                if not udata:  # send XML in data part
                    state["data"] = data
                data = ''
            else:
                state["data"] = ''
        if data: # send ggb in parameters ggbBase64
            srchtml = srchtml.replace('//GGBBASE64', f'ggbBase64: "{data.strip()}",')

        commands = get_by_id(ma, "commands", None)
        if commands:
            state["commands"] = commands
        objxml = get_by_id(ma, "objxml", None)
        if objxml:
            state["objxml"] = objxml
        geostate = b64encode(json.dumps(state).encode("UTF-8")).decode().strip()
        srchtml = srchtml.replace('GEOSTATE', f"'{geostate}'")
        return srchtml

class JSframe(Language):
    def get_default_before_open(self):
        return '<div class="defBeforeOpen"><p>Open JS-frame</p></div>'

    def runner_name(self):
        return "jsframe-runner"

    def js_files(self):
        return ["/cs/js/build/jsframe.js"]

    def can_give_task(self):
        return True

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.txt" % (self.basename, self.filename)
        self.fileext = "txt"
        self.readpoints_default = 'Score: (.*)'
        self.delete_tmp = False

    def modify_usercode(self, s):
        return s

    def run(self, result, sourcelines, points_rule):
        self.save(result)
        return 0, "JSFrame - saved", "", ""

    def save(self, result):
        data = dict(self.query.jso["input"])
        if 'type' in data:
            del data['type']
        result["save"] = data
        return 0, "JSFrame saved", "", ""

    def deny_attributes(self):
        return { # "srchtml":"",
                "filename": "",
                "prehtml": "",
                "posthtml": "",
                "javascript": "",
                "commands": "",
        }

    def state_copy(self):
        return ["c"]

    def iframehtml(self, result, sourcelines, points_rule):
        ma = self.query.jso['markup']
        srchtml = get_by_id(ma, 'srchtml', '')
        data = ma.get('data', None)
        fielddata = ma.get('fielddata', None)

        state = self.query.jso.get("state", {})
        if state:
            c = state.get("c", None)
            if c is not None:
                data = c
        init_data = ""
        if data:
            init_data = "window.initData = " + json.dumps(data) + ";\n"
        if fielddata:
            init_data += "window.fieldData = " + json.dumps(fielddata) + ";\n"
        if init_data:
            init_data = "<script>" + init_data + "</script>"
            srchtml = srchtml.replace("</body>", init_data + "\n</body>")

        return srchtml

class R(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.is_optional_image = True
        self.prgpath = "/tmp/%s/r" % self.basename
        self.filepath = self.prgpath
        self.sourcefilename = "%s/%s.r" % (self.prgpath, self.filename)
        self.fileext = "r"
        self.exename = self.sourcefilename
        mkdirs(self.filepath)
        self.image_ext = "png"
        self.pure_exename = "./%s.r" % self.filename
        #  self.imgsource = "%s/Rplot001.%s" % (self.prgpath, self.image_ext)
        self.imgsource = "Rplot001.%s" % self.image_ext
        self.pure_imgdest = u"{0:s}.{1:s}".format(self.rndname, self.image_ext)
        self.imgdest = "/csgenerated/%s.%s" % (self.rndname, self.image_ext)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["Rscript", "--save", "--restore", self.pure_exename],
                                              ulimit=df(self.ulimit, "ulimit -f 80000"))
        err = re.sub("^Loading required package: .*\n", "", err, flags=re.M)
        err = re.sub("^This is vegan .*\n", "", err, flags=re.M)
        out, err = self.copy_image(result, code, out, err, points_rule)
        if self.delete_tmp:
            remove(self.sourcefilename)
            remove(self.exename)

        return code, out, err, pwddir


class FS(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.fs" % (self.basename, self.filename)
        self.fileext = "fs"

    def get_cmdline(self, sourcecode):
        return "fsharpc --out:%s %s" % (self.exename, self.sourcefilename)

    def run(self, result, sourcelines, points_rule):
        return self.runself(["mono", self.pure_exename])

    def clean_error(self, err):
        return err.replace(
            "F# Compiler for F# 4.0 (Open Source Edition)\n"
            "Freely distributed under the Apache 2.0 Open Source License\n",
            "")


class Mathcheck(Language):
    def css_files(self):
        return ["/cs/css/mathcheck.css"]

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.txt" % (self.basename, self.filename)
        self.fileext = "txt"
        self.readpoints_default = '<!--!points! (.*) -->'

    def run(self, result, sourcelines, points_rule):
        self.stdin = "%s.txt" % self.filename
        cmdline = "/cs/mathcheck/mathcheck_subhtml.out <%s" % sanitize_filename(self.sourcefilename)
        # print("mathcheck: ", self.stdin)
        # code, out, err, pwddir = self.runself(["/cs/mathcheck/mathcheck_subhtml.out"])
        self.prgpath = sanitize_filename(self.prgpath)
        # cmdline = sanitize_cmdline(cmdline)
        out = check_output(["cd " + self.prgpath + " && " + cmdline], stderr=subprocess.STDOUT,
                           shell=True).decode("utf-8")
        correct_text = get_param(self.query, 'correctText', None)
        if correct_text:
            out = out.replace("No errors found. MathCheck is convinced that there are no errors.", correct_text)
        return 0, out, "", ""


class Upload(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        cmds = get_param(query, "cmds", "")
        if not cmds:
            self.filename = None
            return
        self.sourcefilename = "/tmp/%s/%s.txt" % (self.basename, self.filename)
        fn = self.query.jso["input"]["uploadedFile"]
        dn = os.path.dirname(fn)
        ldn = "/tmp/" + self.basename + dn
        lfn = "/tmp/" + self.basename + fn
        mkdirs(ldn)
        # os.symlink(fn,"/tmp/"+self.basename+"/"+fn)
        if not os.path.isfile(lfn):
            shutil.copyfile(fn, lfn)
        self.filename = "." + fn
        self.pure_exename = "/home/agent" + fn

    pass

    def run(self, result, sourcelines, points_rule):
        out = ""
        if self.filename:
            out = "saved: " + self.filename
        return 0, out, "", ""


class Octave(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.m" % (self.basename, self.filename)
        self.exename = self.sourcefilename
        self.pure_exename = "./%s.m" % self.filename
        self.fileext = "m"
        self.imgdest = "/csgenerated/%s.png" % self.rndname
        self.imgsource = get_imgsource(query)
        self.wavsource = get_param(query, "wavsource", "")
        # wavdest = "/csgenerated/%s/%s" % (self.user_id, wavsource)
        self.wavdest = "/csgenerated/%s%s" % (self.rndname, self.wavsource)  # rnd name to avoid browser cache problems
        # wavname = "%s/%s" % (self.user_id, wavsource)
        self.wavname = "%s%s" % (self.rndname, self.wavsource)
        mkdirs("/csgenerated/%s" % self.user_id)

    def run(self, result, sourcelines, points_rule):
        # print("octave: ", self.exename)
        extra = get_param(self.query, "extra", "").format(self.pure_exename, self.userargs)
        self.dockercontainer = get_json_param(self.query.jso, "markup", "dockercontainer", f"timimages/cs3:{CS3_TAG}")
        code, out, err, pwddir = self.runself(["octave", "--no-window-system", "--no-gui", "-qf", self.pure_exename],
                                              timeout=20,
                                              ulimit=df(self.ulimit, "ulimit -t 30 -f 80000"), no_x11=True,
                                              dockercontainer=self.dockercontainer,
                                              extra=extra
                                              )
        if err:
            err = err[0:2000]

            print("err1s: ", err)
            lin = err.splitlines()
            lout = []
            i = 0
            while i < len(lin):
                if (re.match("octave: unable to open X11 DISPLAY", lin[i]) or
                        re.match("octave: disabling GUI features", lin[i]) or
                        re.match("octave: X11 DISPLAY environment variable not set", lin[i])):
                    i += 1
                elif re.match("warning: ft_", lin[i]):
                    i += 1
                    if i < len(lin) and re.match("warning: called from", lin[i]):
                        i += 1
                        while i < len(lin) and re.match(" {4}", lin[i]):
                            i += 1
                else:
                    lout.append(lin[i])
                    i += 1
            err = "\n".join(lout)
            err = err.strip()
            print("err2: ", err)
        out, err = self.copy_image(result, code, out, err, points_rule)
        if self.wavsource and self.wavdest:
            remove(self.wavdest)
            wav_ok, e = copy_file(self.filepath + "/" + self.wavsource, self.wavdest, True, self.is_optional_image)
            if e:
                err = (str(err) + "\n" + str(e) + "\n" + str(out))
            # print("WAV: ", self.is_optional_image, wav_ok, self.wavname, self.wavsource, self.wavdest)
            remove(self.wavsource)
            web = result["web"]
            if wav_ok:
                web["wav"] = "/csgenerated/" + self.wavname
        return code, out, err, pwddir


class Rust(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "rustc"

    def extension(self):
        self.check_extension([".rs"], ".rs", ".exe")

    def get_cmdline(self, sourcecode):
        return f"{self.compiler} -C debuginfo=0 -o {self.exename} {self.opt} {self.sourcefilename}"

    def run(self, result, sourcelines, points_rule):
        return self.runself([self.pure_exename])


class Pascal(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "fpc"

    def extension(self):
        self.check_extension([".pp", ".pas"], ".pas", ".exe")

    def get_cmdline(self, sourcecode):
        return self.compiler + " %s %s -o%s" % (self.opt, self.sourcefilename, self.exename)

    def run(self, result, sourcelines, points_rule):
        return self.runself([self.pure_exename])


# Copy this for new language class
class Lang(Language):
    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)

    def get_cmdline(self, sourcecode):
        cmdline = ""
        return cmdline

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself([])
        return code, out, err, pwddir


dummy_language = Language(QueryClass(), "")

languages = dict()
languages["jypeli"] = Jypeli
languages["comtest"] = CSComtest
languages["shell"] = Shell
languages["cs"] = CS
languages["java"] = Java
languages["graphics"] = Graphics
languages["jcomtest"] = JComtest
languages["junit"] = JUnit
languages["scala"] = Scala
languages["cc"] = CC
languages["c++"] = CPP
languages["ccomtest"] = CComtest
languages["py"] = PY3
languages["py2"] = PY2
languages["swift"] = Swift
languages["lua"] = Lua
languages["clisp"] = CLisp
languages["text"] = Text
languages["xml"] = XML
languages["css"] = Css
languages["jjs"] = JJS
languages["js"] = JS
languages["glowscript"] = Glowscript
languages["vpython"] = VPython
languages["sql"] = SQL
languages["psql"] = PSQL
languages["alloy"] = Alloy
languages["run"] = Run
languages["md"] = MD
languages["html"] = HTML
languages["simcir"] = SimCir
languages["sage"] = Sage
languages["r"] = R
languages["fs"] = FS
languages["mathcheck"] = Mathcheck
languages["upload"] = Upload
languages["octave"] = Octave
languages["processing"] = Processing
languages["wescheme"] = WeScheme
languages["ping"] = Ping
languages["kotlin"] = Kotlin
languages["fortran"] = Fortran
languages["rust"] = Rust
languages["pascal"] = Pascal
languages["stack"] = Stack
languages["geogebra"] = Geogebra
languages["jsframe"] = JSframe
languages["quorum"] = Quorum
