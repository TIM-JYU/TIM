import secrets
import string
from dataclasses import dataclass
from dataclasses import field
from typing import Optional, Any

from loadable import Loadable
from marshmallow import fields

from tim_common.marshmallow_dataclass import dataclass as mdataclass, NewType


class NotInAGitRepo(Exception):
    def __init__(self, path):
        self.message = f"{path} is not in a git repo"
        super().__init__(self.message)


mAny = NewType("Any", Any, field=fields.Raw)


@mdataclass
class Settings(Loadable):
    url: str | None = field(default=None)
    user: str | None = field(default=None)
    branch: str | None = field(default="master")
    library: str | None = field(default=None)
    apiProtocol: str = field(default="https")
    cache: int = field(default=86400)  # time in seconds; default 24 hours
    librarySpecific: mAny | None = field(default=None)


@dataclass
class RepoSettings:
    name: str
    owner: str | None = field(default=None)
    fork: bool = field(default=False)
    oldName: str | None = field(default=None)
    oldOwner: str | None = field(default=None)
    librarySpecific: mAny | None = field(default=None)


@mdataclass
class Options(Loadable):
    onError: str = field(default="raise")
    repo: RepoSettings | None = field(default=None)
    library: str | None = field(default=None)
    fields: dict[str, Any] = field(default_factory=lambda: {})
    askFields: list[str] | None = field(default=None)


@dataclass
class RemoteInfo:
    host: str
    path: str = field(default="")
    protocol: str = field(default="ssh")
    name: str = field(default="origin")
    branch: str = field(default="master")
    user: str | None = field(default=None)

    @staticmethod
    def parse_url(url: str, settings: Settings = None):
        def inner(url: str):
            user = None
            protocol = None
            prefix = ""
            if url.startswith("https://"):
                protocol = "https"
                prefix = "https://"
            elif url.startswith("http://"):
                protocol = "http"
                prefix = "http://"
            elif url.startswith("ssh://"):
                protocol = "ssh"
                prefix = "ssh://"

            if protocol is not None:
                url = url[len(prefix) :]
                parts = url.split("/", maxsplit=1)
            else:
                protocol = "ssh"
                parts = url.split(":", maxsplit=1)

            host = parts[0]
            repo = parts[1] if len(parts) == 2 else ""

            if protocol == "ssh" and "@" in host:
                user, host = host.split("@", 1)

            return [protocol, host, repo, user]

        main_vars = inner(url)

        if settings.url is not None:
            setting_vars = inner(settings.url)
        else:
            setting_vars = [None] * 4

        if settings.user:
            setting_vars[3] = settings.user

        protocol, host, repo, user = (
            main if main is not None else setting
            for main, setting in zip(main_vars, setting_vars)
        )

        if host is None:
            raise ValueError(
                f"Git host couldn't be determined from {url} or {settings.urlPrefix}"
            )

        out = RemoteInfo(host, repo, protocol)
        out.user = user
        return out


def generate_password():
    alphabet = string.ascii_letters + string.digits
    return "".join(secrets.choice(alphabet) for i in range(20))
