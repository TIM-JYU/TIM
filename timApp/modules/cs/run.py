import itertools
import subprocess
import sys
import time
import uuid
from pathlib import PurePath, PureWindowsPath
from subprocess import PIPE, Popen
import shlex

CS3_TAG = 'rust'

sys.path.insert(0, '/py')  # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon

from fileParams import *  # noqa


def wait_file(f1):
    """Wait until the file is ready or 10 tries has been done.

    :param f1: filename to wait
    :return: sthe file status if it became ready, otherwise False

    """
    count = 0
    while count < 10:
        count += 1
        if os.path.isfile(f1):
            s1 = os.stat(f1)
            if s1.st_size > 50:
                return s1
            # print(s1.st_size, " ??????????????????????? ")
        time.sleep(0.05)
    return False


def generate_filename():
    return str(uuid.uuid4())


def run(args, cwd=None, shell=False, kill_tree=True, timeout=-1, env=None, stdin=None, uargs=None, code="utf-8"):
    """Alkuperäinen ajaminen, jossa ajo suoritetaan tavallisen prosessina.

    :param args: run arguments for the command
    :param cwd: dircetory to start
    :param shell: run shell or not
    :param kill_tree: kill to source tree after run
    :param timeout: time to run in ms
    :param env: environment variables
    :param stdin: stdin-file
    :param uargs: user arguments for run
    :type code: str
    :type kill_tree: bool

    """
    s_in = None
    if uargs and len(uargs):
        args.extend(shlex.split(uargs))
    if stdin:
        s_in = PIPE
    p = Popen(args, shell=shell, cwd=cwd, stdout=PIPE, stderr=PIPE, env=env, stdin=s_in)  # , timeout=timeout)
    try:
        if stdin:
            # print(stdin)
            file = codecs.open(stdin, 'r', "utf-8")
            lines = file.read()
            # print("Input ======")
            # print(lines)
            # print("===========")
            file.close()
            # p.stdin.write(str.encode(lines))
            # p.stdin.close()
            stdout, stderr = p.communicate(str.encode(lines), timeout=timeout)
        else:
            stdout, stderr = p.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        return -9, '', ''
    except IOError as e:
        return -2, '', ('IO Error ' + str(e))
    return p.returncode, stdout.decode(), stderr.decode()


# noinspection PyBroadException
def run2(args, cwd=None, shell=False, kill_tree=True, timeout=-1, env=None, stdin=None, uargs=None, code="utf-8",
         extra="", ulimit=None, no_x11=False, savestate="", dockercontainer=f"timimages/cs3:{CS3_TAG}", compile_commandline = ""):
    """Run that is done by opening a new docker instance to run the command.  A script rcmd.sh is needed to fullfill the
    run inside docker.

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
    :param ulimit: limits for the run
    :param savestate: to which file to save te state of shell
    :param no_x11: do not use X11
    :param dockercontainer: what container to run, container needs user with name agent
    :param compile_commandline: command line to compile code
    :return: error code, stdout text, stderr text

    """
    s_in = ""
    pwddir = ""
    if not ulimit:
        ulimit = "ulimit -f 1000 -t 10 -s 2000 "  # -v 2000 -s 100 -u 10
    if uargs:
        uargs = str(uargs)
        if len(uargs):
            args.extend(shlex.split(uargs))
    if stdin:
        s_in = " <" + stdin
    mkdirs(cwd + "/run")
    tmpname = generate_filename()
    urndname = "run/" + tmpname  # ohjaustiedostojen nimet
    stdoutf = urndname + ".in"
    stderrf = urndname + ".err"
    # print("cwd=", cwd)
    cmdf = cwd + "/" + urndname + ".sh"  # varsinaisen ajoskriptin nimi
    compf = cwd + "/run/compile.sh"
    cmnds = ' '.join(tquote(arg) for arg in args)  # otetaan args listan jonot yhteen
    source = ''
    if savestate and cmnds.endswith('.sh'): # source works only for shell scripts
        source = 'source '
    # tehdään komentojono jossa suuntaukset
    cmnds = "#!/usr/bin/env bash\n" + ulimit + "\n" + extra + source + cmnds + \
            " 1>" + "~/" + stdoutf + " 2>" + "~/" + stderrf + s_in + "\n"
    compile_cmnds = None
    if compile_commandline:
        compile_cmnds = "#!/usr/bin/env bash\n" + compile_commandline + \
                " 1>" + "~/" + stdoutf + " 2>" + "~/" + stderrf + "\n"
        codecs.open(compf, "w", "utf-8").write(compile_cmnds)  # kirjoitetaan kääntämisskripti
        os.chmod(compf, 0o777)
    else:
        try:
            os.remove(compf)
        except:
            pass

    # cmnds = "#!/usr/bin/env bash\n" + ulimit + "\n" + extra + cmnds + " 1>" + "~/" +
    # stdoutf + " 2>" + "~/" + stderrf + s_in + "\n"
    # print("============")
    # print(cwd)
    # print(stdoutf)
    # print(stderrf)
    # print(cmdf)
    # print(cmnds)
    # print("============")
    codecs.open(cmdf, "w", "utf-8").write(cmnds)  # kirjoitetaan komentotiedosto
    mkdirs("/tmp/run")  # varmistetaan run-hakemisto
    udir = cwd.replace("/tmp/", "")  # koska mountattu eri tavalla, poistetaan tmp alusta
    # print(udir,"\nWait for run")
    compose_proj = os.environ['COMPOSE_PROJECT_NAME']

    # Convert possible Windows path to Linux style, e.g. C:/Users/... -> /C/Users/...
    root_dir = PureWindowsPath(os.environ['TIM_ROOT'])
    if root_dir.drive:
        drive_letter = root_dir.drive[0]
        root_dir = PurePath('/') / drive_letter / root_dir.relative_to(root_dir.anchor)

    path_mappings = [["-v", f"{root_dir.as_posix()}/timApp/modules/cs/{p}:/cs/{p}:ro"] for p in
                     ["rcmd.sh", "cpp", "java", "jypeli", "doxygen", "mathcheck", "fs", "data", "simcir", "MIRToolbox"]]

    dargs = ["docker", "run", "--name", tmpname, "--rm=true",
             *itertools.chain.from_iterable(path_mappings),
             "-v", f"/tmp/{compose_proj}_uhome/{udir}/:/home/agent/",
             "-w", "/home/agent", dockercontainer, "/cs/rcmd.sh", urndname + ".sh", str(no_x11), str(savestate)]
    # dargs = ["docker", "exec", "kana",
    #         "/cs/rcmd.sh", urndname + ".sh", str(no_x11), str(savestate)]
    # print(dargs)
    p = Popen(dargs, shell=shell, cwd="/cs", stdout=PIPE, stderr=PIPE, env=env)  # , timeout=timeout)
    errcode = 0
    errtxt = ""
    try:
        stdout, stderr = p.communicate(timeout=timeout)
        # print("stdout: ", stdout[:100])
        # print("stderr: ", stderr)
        # print("Run2 done!")
        try:
            pwddir = codecs.open(cwd + '/pwd.txt', 'r', "utf-8").read()  # .encode("utf-8")
        except:
            pwddir = ""
        # print("pwddir=", pwddir)
        err = stderr.decode()

        if stderr and err.find("Compile error") < 0:
            remove(cwd + "/" + stdoutf)
            remove(cwd + "/" + stderrf)
            err = stderr.decode()
            if "File size limit" in err:
                err = "File size limit exceeded"
                pass
            if "Killed" in err:
                err = "Timeout. Too long loop?"
                pass
            # errcode = -3
            # errtxt = "Run error: " + str(err) + "\n"
            return -3, '', ("Run error: " + str(err)), pwddir
        try:
            try:
                stdout = codecs.open(cwd + "/" + stdoutf, 'r', code).read()  # luetaan stdin ja err
            except UnicodeDecodeError:
                stdout = codecs.open(cwd + "/" + stdoutf, 'r', "iso-8859-15").read()  # luetaan stdin ja err
        except:
            stdout = ""

        try:
            try:
                stderr = err + codecs.open(cwd + "/" + stderrf, 'r', "utf-8").read()
            except UnicodeDecodeError:
                try:
                    stderr = err + codecs.open(cwd + "/" + stderrf, 'r', "utf-8").read()
                except UnicodeDecodeError:
                    stderr = err + codecs.open(cwd + "/" + stderrf, 'r', "iso-8859-15").read()
        except:
            stderr = err

        remove(cwd + "/" + stdoutf)
        remove(cwd + "/" + stderrf)
        remove(cwd + '/pwd.txt')
        # print(stdout)
        # print("stderr", stderr)
    except subprocess.TimeoutExpired:
        # p.kill()
        remove(cwd + "/" + stdoutf)
        remove(cwd + "/" + stderrf)
        remove(cwd + '/pwd.txt')
        os.system("docker rm -f " + tmpname)
        return -9, '', '', pwddir
    except IOError as e:
        remove(cwd + "/" + stdoutf)
        remove(cwd + "/" + stderrf)
        remove(cwd + '/pwd.txt')
        return -2, '', ("IO Error" + str(e)), pwddir
    return errcode, stdout, errtxt + stderr, pwddir

def run2_subdir(args, dir=None, cwd=None, *kargs, **kwargs):
    """run2 but inside subdirectory cwd with dir as root.
    
    :param args: command and arguments
    :param dir: root directory to be included in run
    :param cwd: working directory relative to dir
    :param *kargs: other run2 arguments
    :param **kwargs: other run2 arguments
    """
    if dir is None:
        return run2(args, cwd=cwd, *kargs, **kwargs)
    
    if cwd and cwd[0] == "/":
        cwd = os.path.normpath(cwd.replace(dir, "/home/agent/"))
    if isinstance(args, str):
        args = ["bash", "-c", "set -e; cd " + cwd + "; " + args]
    else:
        argstring = " ".join([shlex.quote(arg) for arg in args])
        args = ["bash", "-c", "set -e; cd " + cwd + "; " + argstring]
    return run2(args, cwd=dir, *kargs, **kwargs)


def copy_file(f1, f2, remove_f1=False, is_optional=False):
    """Copy file.  This function is done, because basic copy2 seems to fail in some cases or to be more specific, the f1
    may not be ready before starting copy. First if the file is not optional, it is waited to appear.  After appering it
    should be more than 43 bytes long (seems the not ready file is many times 43 bytes long)

    :param f1: file name to copyt
    :param f2:
    :param remove_f1:
    :param is_optional:
    :return:

    """
    try:
        # print(f1, f2)
        count = 0
        while count < 10:
            count += 1
            if not os.path.isfile(f1) and is_optional:
                return False, ""
            s1 = wait_file(f1)
            if not s1:
                print("No file:", f1)
                return False, "No file: " + f1
            shutil.copy2(f1, f2)
            # os.system("cp " + f1 + " " + f2)
            s2 = os.stat(f2)
            # print(s1.st_size, " ?? ", s2.st_size)
            if s1.st_size == s2.st_size:
                if remove_f1:
                    remove(f1)
                return True, ""
            # print(s1.st_size, " != ", s2.st_size)
        print("Copy error!!!")
        return False, "Copy error!!!"
    except OSError as e:
        # err = err + "\n" + str(e) + "\n" + out
        print(e)
        return False, e


def get_imgsource(query):
    result = get_param(query, "imgsource", "")
    if result:
        return result
    result = get_param(query, "bmpname", "")  # backwards compability
