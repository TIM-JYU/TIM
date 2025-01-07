import codecs
import itertools
import os
import shlex
import shutil
import subprocess
import time
import uuid
from pathlib import PurePath, PureWindowsPath
from subprocess import PIPE, Popen

from file_util import write_safe, is_safe_path, rm_safe
from tim_common.fileParams import mkdirs, tquote, get_param

CS3_IMAGE = os.environ.get("CSPLUGIN_IMAGE", "")
CS3_TAG = CS3_IMAGE.split(":")[-1]


def wait_file(f1, tries=10):
    """Wait until the file is ready or enough tries has been done.

    :param f1: filename to wait
    :param tries: number of tries
    :return: the file status if it became ready, otherwise False

    """
    count = 0
    while count < tries:
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


def run(
    args,
    cwd=None,
    shell=False,
    kill_tree=True,
    timeout=-1,
    env=None,
    stdin=None,
    uargs=None,
    code="utf-8",
):
    """Alkuperäinen ajaminen, jossa ajo suoritetaan tavallisen prosessina.

    :param args: run arguments for the command
    :param cwd: directory to start
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
    p = Popen(
        args, shell=shell, cwd=cwd, stdout=PIPE, stderr=PIPE, env=env, stdin=s_in
    )  # , timeout=timeout)
    try:
        if stdin:
            # print(stdin)
            file = codecs.open(stdin, "r", "utf-8")
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
        return -9, "", ""
    except OSError as e:
        return -2, "", ("IO Error " + str(e))
    return p.returncode, stdout.decode(), stderr.decode()


def get_user_mappings(root_dir, mounts):
    """
    Return list of needed docker volume mappings to map under user directory
    At the moment this is "static", so every new mapping need to be added
    to know_user_mappings by admin
    :param root_dir: what is the root directory to map
    :param mounts: list of keys to mount
    :return: docker volume mount list
    """
    user_mappings = []
    # TODO: add mapping command to this list
    know_user_mappings = {
        "ohj1Content": [
            "-v",
            f"{root_dir.as_posix()}/timApp/static/ohj1/Content:/home/agent/Content:ro",
        ],
    }
    for mnt_name in mounts:
        mapping = know_user_mappings.get(mnt_name)
        if mapping:
            user_mappings.append(mapping)
    return user_mappings


class RunCleaner:
    def __init__(self, p: Popen, container: str, files: list[str]):
        self.p = p
        self.files = files
        self.container = container

    def __enter__(self):
        pass

    def __exit__(self, type, value, traceback):
        if self.p.returncode is None:
            self.p.kill()
            subprocess.run(["docker", "kill", self.container])

        for file in self.files:
            rm_safe(file)


# noinspection PyBroadException
def run2(
    args,
    cwd=None,
    shell=False,
    kill_tree=True,
    timeout=-1,
    env=None,
    stdin=None,
    uargs=None,
    code="utf-8",
    extra="",
    ulimit=None,
    no_x11=False,
    savestate="",
    dockercontainer=CS3_IMAGE,
    compile_commandline="",
    mounts=[],
    extra_mappings=None,
    escape_pipe=False,
    save_run_cmd=None,
    save_test_run_cmd=None,
):
    """Run that is done by opening a new docker instance to run the command.  A script rcmd.sh is needed to fulfill the
    run inside docker.

    :param args: run arguments for the command
    :param cwd: in which directory the command should start
    :param shell: maybe not needed any more???
    :param kill_tree: maybe not needed anymore
    :param timeout: how long the run is allowed to run
    :param env: environment variables for run
    :param stdin: what file to use for stdin
    :param uargs: user arguments for the run
    :param code: which coding schema to use ("utf-8" is default)
    :param extra: extra command used for the run
    :param ulimit: limits for the run
    :param savestate: to which file to save te state of shell
    :param no_x11: do not use X11
    :param dockercontainer: what container to run, container needs user with name agent
    :param compile_commandline: command line to compile code
    :param escape_pipe: If True, pipe character | is escaped into '|'
    :param mounts: User-defined mounts
    :param extra_mappings: Extra non-user csplugin folder mappings
    :param escape_pipe: TODO: what
    :param save_run_cmd: filename to save run_cmd
    :param save_test_run_cmd: filename to save test_run_cmd
    :return: error code, stdout text, stderr text

    """
    s_in = ""
    pwddir = ""
    if not ulimit:
        ulimit = "ulimit -f 1000 -t 20 -s 2000 -n 65536"
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
    cmnds = " ".join(tquote(arg) for arg in args)
    if not escape_pipe:
        cmnds = cmnds.replace("'|'", "|")
    source = ""
    if savestate and cmnds.endswith(".sh"):  # source works only for shell scripts
        source = "source "
    run_cmd = "{ " + extra + source + cmnds + "; }"
    if save_run_cmd:
        save_run_cmd_file = cwd + "/" + save_run_cmd
        write_safe(save_run_cmd_file, run_cmd)  # kirjoitetaan ajotiedosto
        os.chmod(save_run_cmd_file, 0o775)
    if save_test_run_cmd:
        save_test_run_cmd_file = cwd + "/" + save_test_run_cmd
        write_safe(save_test_run_cmd_file, run_cmd)  # kirjoitetaan ajotiedosto
        os.chmod(save_test_run_cmd_file, 0o775)
    compile_cmnds = None
    # tehdään komentojono jossa suuntaukset
    if compile_commandline:
        cmnds = (
            "#!/usr/bin/env bash\n"
            + ulimit
            + "\n"
            + run_cmd
            + " 1>>"
            + "~/"
            + stdoutf
            + " 2>>"
            + "~/"
            + stderrf
            + s_in
            + "\n"
        )
        compile_cmnds = (
            "#!/usr/bin/env bash\n("
            + compile_commandline
            + ") 1>"
            + "~/"
            + stdoutf
            + " 2>"
            + "~/"
            + stderrf
            + "\n"
        )
        write_safe(compf, compile_cmnds)
        os.chmod(compf, 0o777)
    else:
        cmnds = (
            "#!/usr/bin/env bash\n"
            + ulimit
            + "\n{ "
            + extra
            + source
            + cmnds
            + "; } 1>"
            + "~/"
            + stdoutf
            + " 2>"
            + "~/"
            + stderrf
            + s_in
            + "\n"
        )
        rm_safe(compf)
    # cmnds = "#!/usr/bin/env bash\n" + ulimit + "\n" + extra + cmnds + " 1>" + "~/" +
    # stdoutf + " 2>" + "~/" + stderrf + s_in + "\n"
    # print("============")
    # print(cwd)
    # print(stdoutf)
    # print(stderrf)
    # print(cmdf)
    # print(cmnds)
    # print("============")
    write_safe(cmdf, cmnds)  # kirjoitetaan komentotiedosto
    mkdirs("/tmp/run")  # varmistetaan run-hakemisto
    udir = cwd.replace(
        "/tmp/", ""
    )  # koska mountattu eri tavalla, poistetaan tmp alusta
    # print(udir,"\nWait for run")
    compose_proj = os.environ["COMPOSE_PROJECT_NAME"]
    # Convert possible Windows path to Linux style, e.g. C:/Users/... -> /C/Users/...
    root_dir = PureWindowsPath(os.environ["TIM_ROOT"])
    if root_dir.drive:
        drive_letter = root_dir.drive[0]
        root_dir = PurePath("/") / drive_letter / root_dir.relative_to(root_dir.anchor)

    extra_mappings = extra_mappings or []

    root_dir_path = root_dir.as_posix()

    def resolve_mapping(p: str) -> list[str]:
        orig = (
            p if p.startswith("/cs_data/") else f"{root_dir_path}/timApp/modules/cs/{p}"
        )
        return ["-v", f"{orig}:/cs/{p}:ro"]

    path_mappings = [
        resolve_mapping(p)
        for p in [
            "rcmd.sh",
            "cpp",
            "java",
            "dotnet",
            "doxygen",
            "mathcheck",
            "fs",
            "data",
            *extra_mappings,
        ]
    ]

    user_mappings = get_user_mappings(root_dir, mounts)

    network_args = (
        []
        if CS3_TAG.startswith("base-")
        else ["--network", f"{compose_proj}_csplugin_db"]
    )

    dargs = [
        "docker",
        "run",
        "--name",
        tmpname,
        "--rm=true",
        "--tmpfs",
        "/cs",
        *itertools.chain.from_iterable(path_mappings),
        *itertools.chain.from_iterable(user_mappings),
        "-v",
        f"/tmp/{compose_proj}_uhome/{udir}/:/home/agent/",
        "-v",
        f"{compose_proj}_csplugin_data:/cs_data:ro",
        *network_args,
        "-w",
        "/home/agent",
        dockercontainer,
        "/cs/rcmd.sh",
        urndname + ".sh",
        str(no_x11),
        str(savestate),
    ]
    # dargs = ["docker", "exec", "kana",
    #         "/cs/rcmd.sh", urndname + ".sh", str(no_x11), str(savestate)]
    # print(" ".join(dargs))
    p = Popen(
        dargs, shell=shell, cwd="/cs", stdout=PIPE, stderr=PIPE, env=env
    )  # , timeout=timeout)
    errcode = 0
    errtxt = ""

    with RunCleaner(
        p, tmpname, [cwd + "/" + stdoutf, cwd + "/" + stderrf, cwd + "/pwd.txt"]
    ):
        try:
            stdout, stderr = p.communicate(timeout=timeout)
            # print("stdout: ", stdout[:100])
            # print("stderr: ", stderr)
            # print("Run2 done!")
            try:
                pwddir = codecs.open(
                    cwd + "/pwd.txt", "r", "utf-8"
                ).read()  # .encode("utf-8")
            except:
                pwddir = ""
            # print("pwddir=", pwddir)
            err = stderr.decode()

            if stderr and err.find("Compile error") < 0:
                err = stderr.decode()
                if "File size limit" in err:
                    err = "File size limit exceeded"
                if "Killed" in err:
                    err = "Timeout. Too long loop?"
                # errcode = -3
                # errtxt = "Run error: " + str(err) + "\n"
                return -3, "", ("Run error: " + str(err)), pwddir
            try:
                try:
                    stdout = codecs.open(
                        cwd + "/" + stdoutf, "r", code
                    ).read()  # luetaan stdin ja err
                except UnicodeDecodeError:
                    stdout = codecs.open(
                        cwd + "/" + stdoutf, "r", "iso-8859-15"
                    ).read()  # luetaan stdin ja err
            except:
                stdout = ""

            try:
                try:
                    stderr = err + codecs.open(cwd + "/" + stderrf, "r", "utf-8").read()
                except UnicodeDecodeError:
                    try:
                        stderr = (
                            err + codecs.open(cwd + "/" + stderrf, "r", "utf-8").read()
                        )
                    except UnicodeDecodeError:
                        stderr = (
                            err
                            + codecs.open(
                                cwd + "/" + stderrf, "r", "iso-8859-15"
                            ).read()
                        )
            except:
                stderr = err

            # print(stdout)
            # print("stderr", stderr)
        except subprocess.TimeoutExpired:
            return -9, "", "", pwddir
        except OSError as e:
            return -2, "", ("IO Error" + str(e)), pwddir
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
    may not be ready before starting copy. First if the file is not optional, it is waited to appear.
    After appearing, it should be more than 43 bytes long (seems the not ready file is many times 43 bytes long)

    :param f1: file name to copy
    :param f2:
    :param remove_f1:
    :param is_optional:
    :return:

    """
    if not is_safe_path(f2):
        return False, f"Cannot write to {f2}"

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
                    rm_safe(f1)
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
    result = get_param(query, "bmpname", "")  # backwards compatibility
