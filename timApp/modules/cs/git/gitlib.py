from typing import Optional, Union, Dict, Any
from pathlib import Path
import os.path
from shutil import rmtree
from git.util import *
import re
import urllib
from urllib.request import urlopen, Request
from dataclasses import dataclass, field
import subprocess
from datetime import datetime, timedelta
from threading import Lock
from shutil import copy2
from file_util import File, copy_files_glob, rm
import json

git_libs = {}
git_lib_urls = {}

@dataclass
class APIResponse:
    status: int
    data: Any
    reason: Optional[str]


class CacheItem:
    def __init__(self):
        self.lock = Lock()
        self.time = datetime.min


@dataclass
class LibResponse:
    ok: bool
    msg: str = field(default="")
    data: Any = field(default=None)


class GitLib:
    id: str = "_gitlib"
    url: Optional[str] = None

    _cache = {}

    def __init__(self, settings: Settings, info: RemoteInfo):
        self.settings = settings
        self.domain = info.host
        self.api_path = settings.apiProtocol + "://" + self.domain + "/"
        self.remote = info
        self.remote.path = self.sanitize_repo_path(info.path) if info.path is not None else ""

    def get_headers(self):
        raise NotImplementedError("get_headers not implemented")

    def call_api(self, path, data = None, method = None):
        request = Request(self.api_path + path, json.dumps(data).encode('utf-8'), self.get_headers(), method=method)
        try:
            response = urlopen(request)
        except urllib.error.HTTPError as e:
            try:
                data = e.file.read().decode('utf-8')
            except:
                data = None
            return APIResponse(e.code, data, e.reason)
        except urllib.error.URLError as e:
            return APIResponse(e.reason.errno, None, e.reason.strerror)

        try:
            data=json.loads(response.read().decode('utf-8'))
        except:
            data=None

        return APIResponse(response.status, data, None)

    def post(self, path, data = None):
        return self.call_api(path, data, method = 'POST')
    def get(self, path, data = None):
        return self.call_api(path, data, method = 'GET')
    def delete(self, path, data = None):
        return self.call_api(path, data, method = 'DELETE')
    def put(self, path, data = None):
        return self.call_api(path, data, method = 'PUT')
    def patch(self, path, data = None):
        return self.call_api(path, data, method = 'PATCH')

    @property
    def cache_path(self) -> str:
        return f"/tmp/git_files/{self.remote.host.lower()}/{self.remote.path}/{self.remote.branch}"

    def check_cache(self, force_update = False):
        """Updates cache if needed"""
        path = self.cache_path
        cached: CacheItem = GitLib._cache.setdefault(path, CacheItem())
        with cached.lock:
            if not (Path(path) / ".git").exists():
                self.clone(path)
            elif force_update or datetime.now() - cached.time > timedelta(seconds=self.settings.cache):
                self.checkout()
            else:
                return
            GitLib._cache[path].time = datetime.now()

    def get_files(self, sub_path: str, glob: str = "", force_update = False):
        self.check_cache(force_update)

        cpath = self.cache_path

        basepath = Path(cpath) / sub_path
        baseglob = basepath / glob
        if baseglob.is_dir():
            matches = baseglob.glob("**/*")
            file_paths = [file for file in matches if file.is_file()]
        elif baseglob.exists():
            file_paths = [baseglob]
        else:
            matches = list(basepath.glob(glob))
            file_paths = [file for file in matches if file.is_file()]

        files = []
        for path in file_paths:
            file = File(str(path.relative_to(basepath)))

            try:
                file.bcontent = path.read_bytes()
            except Exception as e:
                raise Exception(f"Failed to read git file {path}: {e}")

            try:
                file.content = file.bcontent.decode(encoding="utf-8")
            except Exception as e:
                file.content = None
            files.append(file)

        return files

    def copy_files(self, sub_path: str, destination: str, glob: str = ""):
        self.check_cache()

        basepath = Path(self.cache_path) / sub_path
        dpath = Path(destination)
        if basepath.is_file() and dpath.exists():
            rm(basepath)
        elif basepath.is_dir() and dpath.exists() and not dpath.is_dir():
            rm(dpath)

        copy_files_glob(glob, str(basepath), destination)

    def sanitize_repo_path(self, repo: str):
        repo = re.sub(r'[^A-Za-z0-9\.-_]', "-", repo)
        repo = re.sub(r'-+', "-", repo)
        return repo[:-1] if repo.endswith('-') else repo

    def clone(self, path: Union[str, Path]):
        clone(path, self.remote.host, self.remote.path, self.remote.protocol, self.remote.user, self.remote.name, self.remote.branch)

    def checkout(self, sub_path = ".", path: str = None, do_fetch=True, force=True):
        if path is None:
            path = self.cache_path
        checkout(path, sub_path, do_fetch, self.remote.name, self.remote.branch, force)

    def pull_or_clone(self, path: str):
        try:
            lpath = get_repo_root(Path(path))
        except:
            lpath = None

        if lpath is None or os.path.abspath(path) != lpath:
            self.clone(path)
        else:
            self.checkout(path)

    @classmethod
    def all_subclasses(cls):
        subclasses = cls.__subclasses__()
        return subclasses + [i for sc in subclasses for i in sc.all_subclasses()]

    def check_credentials(self, credentials) -> LibResponse:
        raise NotImplementedError("check_credentials is not implemented")

    def library_specific(self, credentials, options):
        pass


def get_lib_class(host: str, library: Optional[str] = None) -> type(GitLib):
    if library is not None:
        cls = git_libs.get(library, None)
    else:
        cls = git_lib_urls.get(host.lower(), None)
    return cls


def get_lib(info: RemoteInfo, settings: Settings) -> GitLib:
    cls = get_lib_class(info.host, settings.library)

    if cls is None:
        raise Exception(f"Couldn't get a git library for {info.host}")

    return cls(settings, info)


def get_repo_root(opath: Path) -> str:
    path = opath
    while not path.is_dir():
        if path == path.parent:
            break
        path = path.parent

    response = subprocess.run(["git", "-C", str(path), "rev-parse", "--absolute-git-dir", "2>", "/dev/null"], stdout=subprocess.PIPE, encoding='utf-8')
    if response.returncode != 0 or len(response.stdout) == 0:
        raise NotInAGitRepo(str(opath))

    return response.stdout.strip()


def is_in_git_repo(path: Path, must_exist=False):
    if must_exist and not path.is_dir():
        return False

    try:
        get_repo_root(path)
    except NotInAGitRepo:
        return False

    return True


def get_remote_and_branch(path: str) -> (str, str):
    if not is_in_git_repo(Path(path)):
        raise NotInAGitRepo(path)

    response = subprocess.run(["git", "-C", path, "status", "-sb"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    if response.returncode != 0 or len(response.stdout) == 0:
        raise Exception(f"Git status: returncode: {response.returncode}, stdout: {response.stdout}, stderr: {response.stderr}")

    tmp: str = response.stdout.strip()
    tmp = tmp.split("\n", 1)[0]
    tmp = tmp.split(" ", 2)[1]
    _, remote = tmp.split("...")
    remote, remote_branch = remote.split("/", 1)
    return remote, remote_branch # TODO: test


def get_remote_info(path: str) -> RemoteInfo:
    remote, remote_branch = get_remote_and_branch(path)

    response = subprocess.run(["git", "-C", path, "config", "--get", f"remote.{remote}.url"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    if response.returncode != 0 or len(response.stdout) == 0:
        raise Exception(f"Git config: returncode: {response.returncode}, stdout: {response.stdout}, stderr: {response.stderr}")

    remote = RemoteInfo.parse_url(response.stdout.strip())
    remote.name = remote
    remote.branch = remote_branch
    return remote # TODO: test


def clone(path: Union[str, Path], host: str, repo: str, protocol: str = "ssh", user: str = None, remote = "origin", branch = "master"):
    if isinstance(path, str):
        path = Path(path)
    if path.exists() and not path.is_dir():
        raise NotADirectoryError(f"Git clone: path is not a directory")

    if protocol != "ssh":
        raise ValueError("Git clone: Only ssh protocol is supported")

    path.mkdir(parents=True, exist_ok=True)

    prefix = ""
    if user is not None:
        prefix = f"{user}@"

    # using ssh:// allows specifying a different port
    response = subprocess.run(["git", "clone", f"{protocol}://{prefix}{host}/{repo}", "-o", remote, "-b", branch, str(path)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    if response.returncode != 0:
        raise Exception(f"Git clone: returncode: {response.returncode}, stdout: {response.stdout}, stderr: {response.stderr}")


def checkout(path: str, sub_path = ".", do_fetch=True, remote="origin", branch="master", force=True):
    checkout_branch = f"{remote}/{branch}" if remote is not None and len(remote) > 0 else branch
    options = []
    if force:
        options.append("-f")
    if do_fetch:
        response = subprocess.run(["git", "-C", path, "fetch"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
        if response.returncode != 0:
            raise Exception(f"Git fetch: returncode: {response.returncode}, stdout: {response.stdout}, stderr: {response.stderr}")
    response = subprocess.run(["git", "-C", path, "checkout", *options, checkout_branch, "--", sub_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    if response.returncode != 0:
        raise Exception(f"Git checkout: returncode: {response.returncode}, stdout: {response.stdout}, stderr: {response.stderr}")


def pull_or_clone(path: str, rinfo: RemoteInfo):
    try:
        lpath = get_repo_root(Path(path))
    except:
        lpath = None

    if lpath is None or os.path.abspath(path) != lpath:
        clone(path, rinfo.host, rinfo.path, rinfo.protocol, rinfo.user, rinfo.name, rinfo.branch)
    else:
        checkout(path, remote=rinfo.name, branch=rinfo.branch)


def populate():
    from git.gitea import GiteaLib
    from git.gitea_aalto import GiteaAaltoLib
    global git_libs, git_lib_urls
    classes =  [GitLib] + GitLib.all_subclasses()

    def add_id(cls, id):
        if id in git_libs:
            raise Exception(f"GitLib {cls.__name__} has a duplicate id ({id}) with {git_libs[id].__name__}")
        git_libs[id] = cls

    def add_url(cls, url):
        if url in git_lib_urls:
            raise Exception(f"GitLib {cls.__name__} has a duplicate url ({url}) with {git_lib_urls[url].__name__}")
        git_lib_urls[url] = cls

    for cls in classes:
        if hasattr(cls, "id") and cls.id is not None:
            id = cls.id
            add_id(cls, id.lower())
        else:
            id = None

        if not hasattr(cls, "url") or cls.url is None or (isinstance(cls.url, list) and len(cls.url) == 0):
            url = None
        else:
            url = cls.url
            if isinstance(url, list):
                for u in url:
                    add_url(cls, u.lower())
            else:
                add_url(cls, url.lower())

        if not id and not url:
            raise Exception(f"GitLib {cls.__name__} hasn't defined id or url")

populate()