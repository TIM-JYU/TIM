from dataclasses import dataclass
from typing import Any
from urllib.parse import urlparse

from requests import Session, Response

from timApp.tim_app import app
from timApp.util.flask.requesthelper import RouteException


@dataclass
class CASException(Exception):
    msg: str
    pass


@dataclass(slots=True)
class CASServiceInfo:
    base_url: str
    auth_path: str


class CASSession(Session):
    def __init__(
        self,
        base_url: str,
        service_map: dict[str, CASServiceInfo],
        username: str,
        password: str,
        *args: Any,
        **kwargs: Any,
    ) -> None:
        super().__init__(*args, **kwargs)
        self.username = username
        self.password = password
        self.base_url = base_url
        self.service_map = service_map
        self._tgt = None

    def _refresh_ticket_granting_token(self) -> None:
        # https://apereo.github.io/cas/6.5.x/protocol/REST-Protocol-Request-TicketGrantingTicket.html
        tickets_url = f"{self.base_url}/cas/v1/tickets"
        resp = self.post(
            tickets_url, data={"username": self.username, "password": self.password}
        )
        match resp.status_code:
            case 401:
                raise CASException("Invalid username or password")
            case 400 | 415:
                raise CASException("Invalid request")
            case _ if resp.status_code != 201:
                raise CASException(f"Unknown error, got code {resp.status_code}")

        self._tgt = resp.headers["location"]

    def _get_service_ticket(self, service_info: CASServiceInfo) -> str:
        # https://apereo.github.io/cas/6.5.x/protocol/REST-Protocol-Request-ServiceTicket.html
        # with addition that 404 implies expired TGT

        if self._tgt is None:
            self._refresh_ticket_granting_token()
        max_tries = 4
        for _ in range(max_tries):
            resp = self.post(
                self._tgt,
                data={"service": f"{service_info.base_url}/{service_info.auth_path}"},
            )
            if resp.status_code == 404:
                self._refresh_ticket_granting_token()
                continue
            if resp.status_code != 200:
                raise CASException(
                    f"Unknown error requesting service ticket for {service_info.base_url}/{service_info.auth_path}, "
                    f"got code {resp.status_code}"
                )
            return resp.text

        raise CASException(f"Could not get service ticket after {max_tries}")

    def _logout(self) -> None:
        # https://apereo.github.io/cas/6.5.x/protocol/REST-Protocol-Logout.html
        if self._tgt is None:
            return
        self.delete(self._tgt)
        self._tgt = None

    def close(self) -> None:
        self._logout()
        super().close()

    def request(self, method: str, url: str, *args: Any, **kwargs: Any) -> Response:
        url_path = urlparse(url).path.strip("/")
        service, _ = url_path.split("/", 1)
        service_info = self.service_map.get(service)
        if service_info is None:
            raise CASException(
                f"Could not find info for service {service} (parsed from '{url}'). "
                f"All usable services should be specified in service_map argument via constructor"
            )
        service_ticket = self._get_service_ticket(service_info)
        params = kwargs.get("params", {})
        params["ticket"] = service_ticket
        return super().request(
            method, f"{service_info.base_url}/{url_path}", *args, **kwargs
        )


def get_studyinfo_service_url(service_name: str) -> str:
    return f"{app.config['STUDYINFO_BASE_URL']}/{service_name}"


STUDYINFO_SERVICES = {
    "valintalaskentakoostepalvelu": CASServiceInfo(
        base_url=get_studyinfo_service_url("valintalaskentakoostepalvelu"),
        auth_path="j_spring_cas_security_check",
    ),
}


def studyinfo_session() -> CASSession:
    username = app.config["STUDYINFO_USERNAME"]
    password = app.config["STUDYINFO_PASSWORD"]
    if username is None:
        raise RouteException(
            "StudentInfo credentials are not configured on this TIM instance, cannot use StudentInfo"
        )

    return CASSession(
        base_url=app.config["STUDYINFO_BASE_URL"],
        service_map=STUDYINFO_SERVICES,
        username=username,
        password=password,
    )
