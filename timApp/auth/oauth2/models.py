import time
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Optional

from authlib.oauth2.rfc6749 import (
    ClientMixin,
    scope_to_list,
    list_to_scope,
    TokenMixin,
    AuthorizationCodeMixin,
)
from sqlalchemy import ForeignKey, String
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db

if TYPE_CHECKING:
    from timApp.user.user import User


class Scope(Enum):
    user_courses = "user_courses"
    user_tasks = "user_tasks"
    profile = "profile"


@dataclass
class OAuth2Client(ClientMixin):
    """
    An application that is allowed to authenticate as a TIM user and use OAUTH-protected REST API.
    """

    client_id: str
    """Unique identifier for the client."""

    client_name: str | None = None
    """User-friendly client name"""

    client_secret: str = ""
    """Client secret that is used to allow OAUTH2 authentication."""

    redirect_urls: list[str] = field(default_factory=list)
    """List of valid URLs that TIM is allowed to redirect the user to upon successful authentication."""

    allowed_scopes: list[Scope] = field(default_factory=list)
    """Resource scopes that the client can ask for. Scopes are used to limit what REST API can be used."""

    token_endpoint_auth_method: str = "client_secret_post"
    """How the client authenticates itself with TIM. Allowed values:
        *  "none": The client is a public client as defined in OAuth 2.0,
            and does not have a client secret.

        *  "client_secret_post": The client uses the HTTP POST parameters
            as defined in OAuth 2.0

        *  "client_secret_basic": The client uses HTTP Basic as defined in
            OAuth 2.0
    """

    response_types: list[str] = field(default_factory=list)
    """What response types the client can handle.
    In other words, tells TIM how to send user's OAUTH2 token to the client.
    Allowed values: "code" and "token".
    """

    grant_types: list[str] = field(default_factory=list)
    """What grant types the client can handle. Default values:
        * authorization_code
        * implicit
        * client_credentials
        * password

    More custom grant types are allowed.
    """

    @property
    def name(self) -> str:
        return self.client_name or self.client_id

    def get_client_id(self) -> str:
        return self.client_id

    def get_default_redirect_uri(self) -> str | None:
        if self.redirect_urls:
            return self.redirect_urls[0]
        return None

    def get_allowed_scope(self, scope: str) -> str | None:
        if not scope:
            return ""
        allowed = {s.name for s in self.allowed_scopes}
        scopes = scope_to_list(scope)
        return list_to_scope([s for s in scopes if s in allowed])

    def check_redirect_uri(self, redirect_uri: str) -> bool:
        return redirect_uri in self.redirect_urls

    def check_client_secret(self, client_secret: str) -> bool:
        return self.client_secret == client_secret

    def check_endpoint_auth_method(self, method: str, endpoint: str) -> bool:
        if endpoint == "token":
            return self.token_endpoint_auth_method == method
        return True

    def check_response_type(self, response_type: str) -> bool:
        return response_type in self.response_types

    def check_grant_type(self, grant_type: str) -> bool:
        return grant_type in self.grant_types


class OAuth2Token(db.Model, TokenMixin):
    __tablename__ = "oauth2_token"

    id: Mapped[int] = mapped_column(primary_key=True)
    user_id: Mapped[Optional[int]] = mapped_column(ForeignKey("useraccount.id"))
    user: Mapped[Optional["User"]] = relationship()

    client_id: Mapped[Optional[str]] = mapped_column(String(48))
    token_type: Mapped[Optional[str]] = mapped_column(String(40))
    access_token: Mapped[str] = mapped_column(String(255), unique=True)
    refresh_token: Mapped[Optional[str]] = mapped_column(String(255), index=True)
    scope: Mapped[Optional[str]] = mapped_column(default="")
    issued_at: Mapped[int] = mapped_column(default=lambda: int(time.time()))
    access_token_revoked_at: Mapped[int] = mapped_column(default=0)
    refresh_token_revoked_at: Mapped[int] = mapped_column(default=0)
    expires_in: Mapped[int] = mapped_column(default=0)

    def check_client(self, client: ClientMixin) -> bool:
        return self.client_id == client.get_client_id()

    def get_scope(self) -> str | None:
        return self.scope

    def get_expires_in(self) -> int:
        return self.expires_in

    def is_revoked(self) -> bool:
        return bool(self.access_token_revoked_at) or bool(self.refresh_token_revoked_at)

    def is_expired(self) -> bool:
        if not self.expires_in:
            return False

        expires_at = self.issued_at + self.expires_in
        return expires_at < time.time()


class OAuth2AuthorizationCode(db.Model, AuthorizationCodeMixin):
    __tablename__ = "oauth2_auth_code"

    id: Mapped[int] = mapped_column(primary_key=True)
    user_id: Mapped[Optional[int]] = mapped_column(ForeignKey("useraccount.id"))
    user: Mapped["User"] = relationship()

    code: Mapped[str] = mapped_column(String(120), unique=True)
    client_id: Mapped[Optional[str]] = mapped_column(String(48))
    redirect_uri: Mapped[Optional[str]] = mapped_column(default="")
    response_type: Mapped[Optional[str]] = mapped_column(default="")
    scope: Mapped[Optional[str]] = mapped_column(default="")
    nonce: Mapped[Optional[str]]
    auth_time: Mapped[int] = mapped_column(default=lambda: int(time.time()))

    code_challenge: Mapped[Optional[str]]
    code_challenge_method: Mapped[Optional[str]] = mapped_column(String(48))

    def is_expired(self) -> bool:
        return self.auth_time + 300 < time.time()

    def get_redirect_uri(self) -> str | None:
        return self.redirect_uri

    def get_scope(self) -> str | None:
        return self.scope

    def get_auth_time(self) -> int:
        return self.auth_time

    def get_nonce(self) -> str | None:
        return self.nonce
