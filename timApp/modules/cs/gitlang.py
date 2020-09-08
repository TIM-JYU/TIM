import re

from languages import Language
from modifiers import Modifier
from marshmallow import EXCLUDE

from git.util import Options, RemoteInfo, Settings

from tim_common.fileParams import *
from git.gitlib import GitLib, get_lib
from file_util import listify, FileSpecification

def get_lib_and_options(query):
    options = get_json_param(query.jso, "markup", "git", None)
    options: Options = Options.load(options)
    if options is None:
        raise ValueError("No git markup attribute present with git")

    defaults = get_json_param(query.jso, "markup", "gitDefaults", {})
    settings: Settings = Settings.load(defaults)

    info = RemoteInfo.parse_url(settings.url, settings)
    lib = get_lib(info, settings)
    return lib, options


def create_repo(lib, credentials, repo_settings):
    if repo_settings is None:
        return None

    if repo_settings.oldName is None:
        repo_settings.oldName = repo_settings.name

    if repo_settings.owner is None:
        repo_settings.owner = repo_settings.oldOwner
    if repo_settings.oldOwner is None:
        repo_settings.oldOwner = repo_settings.owner

    if not lib.repository_exists(repo_settings):
        if repo_settings.fork:
            response = lib.fork(repo_settings)
            if not response.ok:
                return "Failed to fork repository"
        else:
            response = lib.create_repository(repo_settings)
            if not response.ok:
                return "Failed to create repository"

    response = lib.library_specific(credentials, repo_settings)
    if not response.ok:
        return response.msg

    return None


class GitReg(Language):
    """For creating a git user at an external service"""
    ttype = "gitreg"

    def __init__(self, query, code = None):
        super().__init__(query, code)

        self.lib, self.options = get_lib_and_options(query)

        userid =  query.jso.get("user_id", None) != "Anonymous" if query.jso else None
        if userid is None:
            userid = get_json_param(query.jso, "info", "user_id", "Anonymous")
        self.is_logged_in = userid != "Anonymous"
        self.is_registered = False
        self.askFields = self.options.askFields
        self.repo = self.options.repo

        if not self.is_logged_in:
            return

        fields = self.options.fields
        self.credentials = {field: data["value"] for field, data in fields.items()}

        response = self.lib.is_registered(self.credentials)
        if response.ok:
            self.post_register()
        else:
            response = self.lib.check_credentials(self.credentials)
            if not response.ok:
                for field, value in response.data.items():
                    if field in self.credentials and value != "ok":
                        on_error = fields[field].get("onError", "ask")
                        if on_error == "ask":
                            self.credentials.pop(field)
                            self.askFields.append(field)
                        elif on_error == "none":
                            self.credentials[field] = None
                        else:
                            raise Exception(f"Invalid {field} ({self.credentials[field]}) in credentials")

    def run(self, result, sourcelines, points_rule):
        result["nosave"] = True

        if not self.is_logged_in:
            raise Exception("You must be logged in")

        err = ""

        credentials = get_json_param(self.query.jso, "input", "gitRegFields", {})
        credentials.update(self.credentials)

        response = self.lib.check_credentials(credentials)
        if not response.ok:
            err = response.msg
        else:
            response = self.lib.create_user(credentials)
            if response.ok:
                self.post_register()
            else:
                raise Exception("Failed to create user: " + response.msg)

        return 0, "", err, ""

    def post_register(self):
        self.is_registered = True

        response = self.lib.get_user(self.credentials)
        if not response.ok:
            raise Exception(f"Couldn't get git user: {response.msg}")

        self.credentials = response.data

        if self.options.repo is not None:
            error = create_repo(self.lib, self.credentials, self.options.repo)
            if error is not None:
                raise Exception(error)

    def modify_query(self):
        self.query.set_param(self.askFields, "markup", "git", "askFields")
        self.query.set_param(self.is_registered, "gitRegistered")

    def web_data(self):
        return self.is_registered

    def runner_name(self):
        return "cs-git-reg-runner"

class GitCheck(Modifier):
    """For checking git availability and initializing repo"""
    ttype = "gitcheck"

    def __init__(self, query):
        super().__init__(query)

        if get_param(query, "gitcheckDone", False):
            self.remove = None
            self.success = True
            return

        self.remove = None
        self.success = False

        self.files = get_json_param(query.jso, "markup", "files", None)
        if self.files is None:
            raise ValueError("No files markup present with git modifier")

        self.files = listify(self.files)
        files = [file for file in self.files if file.get("source", "editor").startswith("git:")]

        if len(files) == 0:
            raise ValueError("No git files in files markup with git modifier")

        self.lib, self.options = get_lib_and_options(query)
        credentials = {field: data["value"] for field, data in self.options.fields.items()}

        response = self.lib.get_user(credentials)
        if not response.ok:
            if self.options.onError in ["raise", "create"]:
                raise Exception(f"Couldn't get git user: {response.msg}")
            elif self.options.onError in ["remove", "removeall"]:
                self.remove = self.options.onError
        elif self.options.repo and not self.lib.repository_exists(self.options.repo):
            if self.options.onError == "raise":
                raise Exception(f"Repository {self.options.repo} doesn't exist")
            elif self.options.onError == "create":
                create_repo(self.lib, response.data, self.options.repo)
            elif self.options.onError in ["remove", "removeall"]:
                self.remove = self.options.onError

        self.success = self.remove == None

    def modify_query(self):
        if self.files is None:
            return

        if self.remove in ["remove", "removeall"]:
            if self.remove == "remove":
                matcher = re.compile(rf'git:([^;@]*@)?{re.escape(self.lib.remote.host)}')
                files = [file for file in self.files if matcher.match(file.get("source", "editor")) is None]
            else:
                files = [file for file in self.files if not file.get("source", "editor").startswith("git:")]

            self.query.set_param(files, "markup", "files")
            self.query.set_param(True, "gitcheckDone")
