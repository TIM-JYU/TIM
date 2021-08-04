from typing import TypedDict

import httpagentparser
from werkzeug.user_agent import UserAgent


class UserAgentDevice(TypedDict, total=False):
    name: str
    version: str


class UserAgentResult(TypedDict, total=False):
    os: UserAgentDevice
    dist: UserAgentDevice
    flavor: UserAgentDevice
    browser: UserAgentDevice
    platform: UserAgentDevice
    bot: bool


class SimpleUserAgent(UserAgent):
    def __init__(self, string: str):
        super().__init__(string)
        res: UserAgentResult = httpagentparser.detect(string)

        os_list = []
        os_ver_list = []

        for t in ['flavor', 'dist', 'os']:
            if t in res:
                os_list.append(res[t]['name'])  # type: ignore
                os_ver_list.append(res[t].get('version'))  # type: ignore

        os = "_".join(os_list).lower() if os_list else None
        os_ver = next((v for v in os_ver_list if v), None)
        if os and os_ver:
            os += f"_{os_ver}".lower()

        self.platform = os

        if 'browser' in res:
            self.browser = res['browser'].get('name')
            self.version = res['browser'].get('version')

        if self.browser:
            self.browser = self.browser.lower()
        if self.version:
            self.version = self.version.lower()

        self.language = None
