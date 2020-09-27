from subprocess import check_output
from typing import Optional

import requests
import re

from points import *
from run import *
from os.path import splitext
from pathlib import Path
from modifiers import Modifier
from traceback import print_exc
from dataclasses import dataclass, field

sys.path.insert(0, '/py')  # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon

from fileParams import *  # noqa
from file_util import File, default_filename

"""
Adding new language to csPlugin:

0. Install new compiler to cs/Dockerfile and build new Docker container from that
    - if in varibles.sh is export IS_DEVELOPMENT=true, change it to false
    - in /opt/tim directory run ./dc build csplugin
    - if in varibles.sh was export IS_DEVELOPMENT=true, change it back to true
        - in /opt/tim directory run ./dc build csplugin
    -  docker push cs3:rust
1. Add the language class starting with capital letter to this or new file
2. Add language name to 'ttype' variable
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

JAVAFX_VERSION = '14'


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
    ttype = "_language"
    def __init__(self, query: Optional[QueryClass], sourcefiles = ""):
        """
        :param self: object reference
        :param query: query to use
        :param sourcecodes: source code as a string or list of files ({path: str, content: str})
        """
        self.query = query
        self.markup = {}
        self.stdin = None
        self.query = query
        self.user_id = '--'
        if query.jso:
            self.user_id = df(query.jso.get('info'), {}).get('user_id', '--')
            self.markup = query.jso.get('markup', {})
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
        self.rootpath = get_param(query, "rootPath", None)
        if self.rootpath is not None:
            self.rootpath = os.path.normpath(self.rootpath)
            self.upath = os.path.join(self.rootpath, self.upath)
            if not os.path.normpath(self.upath).startswith(self.rootpath):
                raise Exception("Root path not a parent of path")

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
            if self.rootpath is not None:
                _, self.rootpath = self.rootpath.split("/", 1)
                self.rootpath = self.mustpath + "/" + (self.rootpath if self.rootpath else "")
            self.delete_tmp = False
            mkdirs("/tmp/user")
            # print(self.task_id, self.doc_id, self.fullpath)
        else:
            # Generate random cs and exe filenames
            self.basename = "tmp/" + self.rndname
            mkdirs("/tmp/tmp")

        self.fullpath = "/tmp/" + self.basename  # check it is sure under userpath

        if isinstance(sourcefiles, str):
            sourcefiles = [File.default(query, sourcefiles)]
        if sourcefiles[0].path is None:
            sourcefiles[0].path = default_filename(query)

        extensions, fileext, filedext = self.extensions()
        self.filenames = [file.path for file in sourcefiles]
        self.sourcefiles = sourcefiles
        for file in self.sourcefiles:
            file.fileext = fileext
            file.filedext = fileext
        self.check_extensions(extensions)

        for i in range(len(self.filenames)):
            self.sourcefiles[i].path = "/tmp/%s/%s%s" % (self.basename, self.filenames[i], self.sourcefiles[i].filedext)

        self.ifilename = get_param(query, "inputfilename", "/input.txt")
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

    # single file submission compatibility
    @property
    def filename(self):
        return self.filenames[0]

    @filename.setter
    def filename(self, val):
        self.filenames[0] = val

    @property
    def sourcefilename(self):
        return self.sourcefiles[0].path

    @sourcefilename.setter
    def sourcefilename(self, val):
        self.sourcefiles[0].path = val

    @property
    def fileext(self):
        return self.sourcefiles[0].fileext

    @fileext.setter
    def fileext(self, val):
        self.sourcefiles[0].fileext = val

    def extensions(self):
        return None, "", "" # list of the extensions or None for all, default fileext, default filedext

    def check_extensions(self, extensions):
        if extensions is not None:
            for i in range(len(self.filenames)):
                for ex in extensions:
                    if self.filenames[i].endswith(ex):
                        self.sourcefiles[i].fileext = ex.replace(".", "")
                        self.sourcefiles[i].filedext = ex
                        self.filenames[i] = self.filenames[i][:-len(ex)]
                        break

    def get_cmdline(self):
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

    def save(self, result):  # when used without run, this can change result if needed
        return

    def modify_query(self):
        """
        Modify query before send to browser.  No need to remove attributes, this is done
        next using deny_attribute
        :return:
        """
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
        return "cs-runner"

    @staticmethod
    def js_files():
        """
        :return: list of needed js-files (maybe copiled from ts-files)
        """
        return ["/cs/js/build/csModule.js"]

    @staticmethod
    def css_files():
        """
        :return: list of needed css-files (maybe copiled from scss-files)
        """
        return ["/cs/css/cs.css"]

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

    def get_review(self, usercode):
        """
        return text to show when reviewing task
        """
        return usercode

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

        mounts = self.query.jso.get('markup', {}).get('mounts',[])

        code, out, err, pwddir = run2_subdir(args,
                                    dir=self.rootpath,
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
                                    compile_commandline=self.compile_commandline,
                                    mounts=mounts
                                    )
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

    def web_data(self):
        return None

    def is_valid(self):
        return True

    @classmethod
    def all_subclasses(cls):
        subclasses = cls.__subclasses__()
        return subclasses + [i for sc in subclasses for i in sc.all_subclasses()]

    @classmethod
    def get_client_ttype(cls, ttype):
        """Returns the ttype of this class that should be given to client"""
        if isinstance(cls.ttype, list):
            return cls.ttype[0]
        return cls.ttype

    @staticmethod
    def supports_multifiles():
        """Whether the class supports multiple files as sourcecode"""
        return False # technically True but we want to default to False for now


class LanguageError(Language):
    ttype="_error"
    def __init__(self, query, error_str, sourcecode = ""):
        try:
            super().__init__(query, sourcecode)
        except Exception as e:
            print("Error:", str(e))
            print_exc()
            self.valid = False
            self.own_error = str(e)
        else:
            self.valid = True
            self.own_error = None

        self.query = query
        self.error = error_str

    def modify_query(self):
        self.query.query["error"] = [self.error]
        if self.own_error is not None:
            self.query.query["own_error"] = [self.own_error]

    def runner_name(self):
        return "cs-error"

    def is_valid(self):
        return self.valid


class All(Language):
    ttype = "all"


class CS(Language):
    ttype = ["cs", "c#", "csharp"]

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "csc"
        self.fileext = "cs"
        self.filedext = ".cs"
        self.sourcefilename = "/tmp/%s/%s.cs" % (self.basename, self.filename)
        self.exename = "/tmp/%s/%s.exe" % (self.basename, self.filename)

    def get_sourcefiles(self, main=None):
        sourcefiles = self.markup.get("sourcefiles", None)
        if not sourcefiles:
            sourcefiles = self.sourcefilename
        if main:
            sourcefiles = main + " " + sourcefiles
        return sourcefiles


    def before_save(self, s):
        mockconsole = get_param(self.query, "mockconsole", True)
        if mockconsole:
            s = s.replace('System.Console.ReadLine', 'TIMconsole.ReadLine')
            s = s.replace('Console.ReadLine', 'TIMconsole.ReadLine')
        return s

    def get_cmdline(self):
        options = ""
        if self.just_compile:
            options = "/target:library"
        cmdline = "%s /r:System.Numerics.dll /out:%s %s %s /cs/jypeli/TIMconsole.cs" % (
            self.compiler, self.exename, options, self.get_sourcefiles())
        return cmdline

    def run(self, result, sourcelines, points_rule):
        # result["tim_info"]["valid"] = False  # kokeilu että valit toimii
        # result["tim_info"]["validMsg"] = "Et voi enää saada pisteitä kun katsoit vastauksen"
        return self.runself(["mono", "-O=all", self.pure_exename])


class Jypeli(CS, Modifier):
    ttype = "jypeli"

    def __init__(self, query, sourcecode=""):
        super().__init__(query, sourcecode)
        self.imgsource = "/tmp/%s/output.bmp" % self.basename
        self.pure_bmpname = "./%s.bmp" % self.filename
        self.imgdest = "/csgenerated/%s.png" % self.rndname
        self.pure_exename = u"{0:s}.exe".format(self.filename)
        self.pure_mgdest = u"{0:s}.png".format(self.rndname)

    def get_cmdline(self):
        mainfile = "/cs/jypeli/Ohjelma.cs"
        options = ""
        if self.just_compile:
            options = "/target:library"
        sourcecode = self.sourcefiles[0].content
        if sourcecode.find(" Main(") >= 0 or self.just_compile:
            mainfile = ""
        else:
            classname = self.markup.get("classname", None)
            if not classname:
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
                      self.compiler, self.exename, options, self.get_sourcefiles(mainfile))
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


class CSComtest(CS, Modifier):  # TODO: comtests probably shouldn't be modifiers but they are used as such
    ttype = "comtest"
    nunit = None

    def __init__(self, query, sourcecode=""):
        super().__init__(query, sourcecode)
        self.testdll = u"./{0:s}Test.dll".format(self.filename)
        self.hide_compile_out = True

    def get_cmdline(self):
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
    ttype = "shell"

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
    ttype = "ping"

    def run(self, result, sourcelines, points_rule):
        return 0, "Ping", "", ""


class Java(Language):
    ttype = "java"

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

    def get_cmdline(self):
        return f"javac --module-path /javafx-sdk-{JAVAFX_VERSION}/lib --add-modules=ALL-MODULE-PATH -Xlint:all -cp {self.classpath} {self.javaname}"

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["java", "--module-path", f"/javafx-sdk-{JAVAFX_VERSION}/lib",
                                               "--add-modules=ALL-MODULE-PATH", "-cp",
                                               self.classpath, self.javaclassname],
                                              ulimit=df(self.ulimit, "ulimit -f 10000"))
        return code, out, err, pwddir


class Kotlin(Java):
    ttype = "kotlin"

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.fileext = "kt"
        self.filename = self.classname + "." + self.fileext
        self.javaname = self.filepath + "/" + self.filename
        self.sourcefilename = self.javaname
        self.jarname = self.classname + ".jar"

    def get_cmdline(self):
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


class JComtest(Java, Modifier):
    ttype = "jcomtest"

    def __init__(self, query, sourcecode=""):
        super().__init__(query, sourcecode)
        self.testcs = self.filepath + "/" + self.classname + "Test.java"
        self.testdll = self.javaclassname + "Test"
        self.hide_compile_out = True

    def get_cmdline(self):
        return "java comtest.ComTest %s && javac %s %s" % (self.sourcefilename, self.sourcefilename, self.testcs)

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["java", "org.junit.runner.JUnitCore", self.testdll], no_uargs=True)
        out, err = check_comtest(self, "jcomtest", code, out, err, result, points_rule)
        return code, out, err, pwddir


class JUnit(Java, Modifier):
    ttype = "junit"

    def __init__(self, query, sourcecode=""):
        super().__init__(query, sourcecode)

    def get_cmdline(self):
        return "javac %s" % self.javaname

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["java", "org.junit.runner.JUnitCore", self.javaclassname])
        out, err = check_comtest(self, "junit", code, out, err, result, points_rule)
        return code, out, err, pwddir


class Graphics(Java, Modifier):
    ttype = "graphics"

    def __init__(self, query, sourcecode=""):
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
        runcmd = ["java", "--module-path", f"/javafx-sdk-{JAVAFX_VERSION}/lib",
                  "--add-modules=ALL-MODULE-PATH", "sample.Runner",
                  self.javaclassname, "--captureName", "run/capture.png"]
        runcmd.extend(a)
        code, out, err, pwddir = self.runself(runcmd, cwd=self.prgpath)
        out, err = self.copy_image(result, code, out, err, points_rule)
        err = re.sub('Xlib: {2}extension "RANDR" missing on display ":1"\\.\n', "", err)
        return code, out, err, pwddir


class Scala(Language):
    ttype = "scala"

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.scala" % (self.basename, self.filename)
        self.classname = self.filename
        self.fileext = "scala"

    def get_cmdline(self):
        return "scalac %s" % self.sourcefilename

    def run(self, result, sourcelines, points_rule):
        return self.runself(["scala", self.classname], ulimit=df(self.ulimit, "ulimit -f 10000"))


class CC(Language):
    ttype = "cc"

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "gcc"
        self.source_extensions = [".c", ".cc"]

    def extensions(self):
        return [".h", ".c", ".cc"], ".c", ".exe"

    def get_cmdline(self):
        return self.compiler + " -Wall %s %s -o %s -lm" % (self.opt, self.sources(), self.exename)

    def run(self, result, sourcelines, points_rule):
        return self.runself([self.pure_exename])

    def sources(self):
        return " ".join(file.path for file in self.sourcefiles if any(file.path.endswith(ext) for ext in self.source_extensions))

    @staticmethod
    def supports_multifiles():
        return True


class CPP(CC):
    ttype = ["c++", "cpp"]

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "g++ -std=c++14"
        self.source_extensions = [".cpp", ".cc"]

    def extensions(self):
        return [".h", ".hpp", ".hh", ".cpp", ".cc"], ".cpp", ".exe"


class CComtest(Language):
    ttype = "ccomtest"

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
    ttype = "fortran"

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

    def get_cmdline(self):
        return self.compiler + " -Wall %s %s -o %s -lm" % (self.opt, self.sourcefilename, self.exename)

    def run(self, result, sourcelines, points_rule):
        return self.runself([self.pure_exename])


class PY3(Language):
    ttype = ["py", "py3", "python", "python3"]

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
    ttype = "py2"

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself(["python2", self.pure_exename])
        out, err = self.copy_image(result, code, out, err, points_rule)
        return code, out, err, pwddir


class Swift(Language):
    ttype = "swift"

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
    ttype = "lua"

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
    ttype = "clisp"

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
    ttype = "quorum"

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
    ttype = ["text", "txt"]

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
        saved_text = get_param(self.query, "savedText", "Saved")
        code, out, err, pwddir = (0, "", saved_text.format(showname), "")
        return code, out, err, pwddir


class XML(Text):
    ttype = "xml"

    def run(self, result, sourcelines, points_rule):
        convert = self.query.jso.get('markup', {}).get('convert', None)
        if not convert:
            return super(XML, self).run(result, sourcelines, points_rule)

        # language_class = languages.get(convert.lower(), Language)
        from ttype import TType
        language = TType(convert, self.query, sourcelines).get_language()
        return language.convert(sourcelines)


class Css(Text):
    ttype = "css"
    pass


class JJS(Language):
    ttype = "jjs"

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
    ttype = "js"

    def run(self, result, sourcelines, points_rule):
        return 0, "", "", ""


class Glowscript(JS):
    ttype = "glowscript"
    pass


class Processing(JS):
    ttype = "processing"
    pass


class WeScheme(JS):
    ttype = "wescheme"


class VPython(JS):
    ttype = "vpython"
    pass


class SQL(Language):
    ttype = "sql"

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
    ttype = "psql"

    def run(self, result, sourcelines, points_rule):
        return self.runself(["psql", "-h", self.dbname, "-U", "$psqluser"])


class Alloy(Language):
    ttype = "alloy"

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
    ttype = "run"

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
    ttype = "md"
    pass


class HTML(Language):
    ttype = "html"
    pass


class SimCir(Language):
    ttype = "simcir"

    @staticmethod
    def css_files():
        return ["/cs/simcir/simcir.css", "/cs/simcir/simcir-basicset.css"]


class Sage(Language):
    ttype = "sage"


def get_by_id(jso, item_id, default=None):
    val = jso.get(item_id, None)
    if val is None:
        val = jso.get("-" + item_id, default)
    return val


def html_change(s, old, jso, item_id, repfmt, default, valdef=None, delta=0):
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


"""
This is the outer default HTML code for the JSAV plugin.
Some keywords are later on substituted with user input.
"""

JSAV_DEFAULT_SRC_HTML = """
<!DOCTYPE html>
<html>
<head>
  <title>A JSAV Animation</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <link rel="stylesheet" href="/cs/jsav/css/JSAV.css" type="text/css" />
  <link rel="stylesheet" href="/cs/css/jsav-tim.css" type="text/css" />
</head>
<body>

<style>
//JSAVCSS
</style>

<div>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.js"></script>
<script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/jquery-ui.min.js"></script>
<script src="/cs/jsav/lib/jquery.transit.js"></script>
<script src="/cs/jsav/lib/raphael.js"></script>
<script src="/cs/jsav/build/JSAV.js"></script>

//JSAVHTML

<script type="text/javascript">
    var logEventHandler;
    var exercise;

    score = {correct: 0, total: 0};
    logEventHandler = function(evt) {
        // This counts the total number of questions
        if (evt.type === "jsav-question-created") {
            score.total++;
        } else if (evt.type === "jsav-question-answer") {
            if (evt.correct) { score.correct++; }
        } else if (evt.type === "jsav-exercise-init") {
            exercise = evt.exercise;
        }
    };

    function getData() {
        if (exercise) {
            return {"points": exercise.grade().correct };
        }
        else {
            return {"points": score.correct };
        }
    }

    window.JSAV_OPTIONS = {
        logEvent: logEventHandler,
        element: $(".jsavcontainer"),
        questionDialogBase: $(".questiondialog"),
    };
    //JSAVJAVASCRIPT
</script>

</div>
</body>
</html>
"""

"""
This is the inner default HTML code for the JSAV plugin.
"""

JSAV_HTML = """
<div class="jsavcontainer">
  <div class="jsavcounter"></div>
  <div class="jsavoutput jsavline"></div>
  <div class="jsavcanvas"></div>
  <div class="jsavcontrols"></div>
</div>
<div class="questiondialog"></div>
"""


class Jsav(Language):
    ttype = "jsav"

    def can_give_task(self):
        return True

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.txt" % (self.basename, self.filename)
        self.fileext = "txt"
        self.readpoints_default = 'Score: (.*)'
        self.delete_tmp = False

        self.model = False
        if self.query.jso:
            state = self.query.jso.get("state", None)
            if state:
                self.model = state.get("model", False)

    def state_copy(self):
        """
        :return: list of state attribute names from the currently selected answer to be copied to .ts client code
        """
        return ["message"]

    @staticmethod
    def js_files():
        return ["/cs/js/build/jsav.js"]

    def runner_name(self):
        return "cs-jsav-runner"

    def deny_attributes(self):
        return {"srchtml": "",
                "jsavhtml": "",
                "jsavjs": "",
                "jsavcss": ""
                }

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
        return 0, "JSAV saved", "", ""

    def save(self, result):
        data = dict(self.query.jso["input"])
        if data.get("model", False):
            self.model = True

        if self.model:
            data['model'] = "y"
            result["tim_info"]["valid"] = False
            result["tim_info"]["validMsg"] = "Et voi enää saada pisteitä kun katsoit vastauksen"

        if 'type' in data:
            del data['type']

        result["save"] = data
        return 0, "JSAV saved", "", ""

    def iframehtml(self, result, sourcelines, points_rule):
        """
        :return: the finalized HTML code for the JSAV plugin's iframe
        """
        ma = self.query.jso['markup']
        jsavhtml = JSAV_HTML
        srchtml = get_by_id(ma, 'srchtml', JSAV_DEFAULT_SRC_HTML)
        srchtml = html_change(srchtml, "//JSAVHTML", ma, "jsavhtml", '{}', "", jsavhtml)
        srchtml = html_change(srchtml, "//JSAVJAVASCRIPT", ma, "jsavjs", '{}', "")
        srchtml = html_change(srchtml, "//JSAVCSS", ma, "jsavcss", '{}', "")
        return srchtml

    def convert(self, sourcelines):
        url = "http://stack-api-server/api/xmltoyaml.php"
        data = {'xml': sourcelines}
        r = requests.post(url=url, data=json.dumps(data))
        r = r.json()
        return 0, r.get('yaml'), "", ""


class R(Language):
    ttype = "r"

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
    ttype = "fs"

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.fs" % (self.basename, self.filename)
        self.fileext = "fs"

    def get_cmdline(self):
        return "fsharpc --out:%s %s" % (self.exename, self.sourcefilename)

    def run(self, result, sourcelines, points_rule):
        return self.runself(["mono", self.pure_exename])

    def clean_error(self, err):
        return err.replace(
            "F# Compiler for F# 4.0 (Open Source Edition)\n"
            "Freely distributed under the Apache 2.0 Open Source License\n",
            "")


class Mathcheck(Language):
    ttype = "mathcheck"

    @staticmethod
    def css_files():
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
            out = out.replace("No errors found. Please notice that the check was not complete.", correct_text)
        return 0, out, "", ""


class Upload(Language):
    ttype = "upload"

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


octave_warnings_to_skip = [
    "OpenJDK 64-Bit Server VM warning: Archived non-system classes are disabled because",
    "octave: unable to open X11 DISPLAY",
    "octave: disabling GUI features",
    "octave: X11 DISPLAY environment variable not set",
]


class Octave(Language):
    ttype = "octave"

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
                if any(re.match(skipmsg, lin[i]) for skipmsg in octave_warnings_to_skip):
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
    ttype = "rust"

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "rustc"

    def extensions(self):
        return [".rs"], ".rs", ".exe"

    def get_cmdline(self):
        return f"{self.compiler} -C debuginfo=0 -o {self.exename} {self.opt} {self.sourcefilename}"

    def run(self, result, sourcelines, points_rule):
        return self.runself([self.pure_exename])


class Pascal(Language):
    ttype = "pascal"

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.compiler = "fpc"

    def extensions(self):
        return [".pp", ".pas"], ".pas", ".exe"

    def get_cmdline(self):
        return self.compiler + " %s %s -o%s" % (self.opt, self.sourcefilename, self.exename)

    def run(self, result, sourcelines, points_rule):
        return self.runself([self.pure_exename])


class Tauno(Language, Modifier):
    ttype = "tauno"

    def modify_query(self):
        self.query.set_param(True, "isTauno")


# Copy this for new language class
class Lang(Language):
    ttype = None  # replace: "name used in type attribute"

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)

    def get_cmdline(self):
        cmdline = ""
        return cmdline

    def run(self, result, sourcelines, points_rule):
        code, out, err, pwddir = self.runself([])
        return code, out, err, pwddir


dummy_language = Language(QueryClass(), "")
