import time

from authlib.integrations.flask_oauth2 import AuthorizationServer, ResourceProtector
from authlib.integrations.sqla_oauth2 import (
    create_save_token_func,
    create_bearer_token_validator,
)
from authlib.oauth2 import OAuth2Request
from authlib.oauth2.rfc6749 import grants
from authlib.oauth2.rfc7636 import CodeChallenge
from flask import Flask
from sqlalchemy import select, delete

from timApp.auth.oauth2.models import OAuth2Client, OAuth2Token, OAuth2AuthorizationCode
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from tim_common.marshmallow_dataclass import class_schema

ALLOWED_CLIENTS: dict[str, OAuth2Client] = {}


class RefreshTokenGrant(grants.RefreshTokenGrant):
    TOKEN_ENDPOINT_AUTH_METHODS = [
        "client_secret_basic",
        "client_secret_post",
    ]
    INCLUDE_NEW_REFRESH_TOKEN = True

    def authenticate_refresh_token(self, refresh_token: str) -> OAuth2Token | None:
        token: OAuth2Token | None = (
            run_sql(select(OAuth2Token).filter_by(refresh_token=refresh_token).limit(1))
            .scalars()
            .first()
        )
        if token and not token.is_revoked() and not token.is_expired():
            return token
        return None

    def authenticate_user(self, credential: OAuth2Token) -> User:
        return db.session.get(User, credential.user_id)

    def revoke_old_credential(self, credential: OAuth2Token) -> None:
        credential.refresh_token_revoked_at = int(time.time())
        db.session.add(credential)
        db.session.commit()


class AuthorizationCodeGrant(grants.AuthorizationCodeGrant):
    TOKEN_ENDPOINT_AUTH_METHODS = ["client_secret_basic", "client_secret_post", "none"]

    def save_authorization_code(
            self, code: str, request: OAuth2Request
    ) -> OAuth2AuthorizationCode:
        code_challenge = request.data.get("code_challenge")
        code_challenge_method = request.data.get("code_challenge_method")
        auth_code = OAuth2AuthorizationCode(
            code=code,
            client_id=request.client.client_id,
            redirect_uri=request.redirect_uri,
            scope=request.scope,
            user_id=request.user.id,
            code_challenge=code_challenge,
            code_challenge_method=code_challenge_method,
        )
        db.session.add(auth_code)
        db.session.commit()
        return auth_code

    def query_authorization_code(
            self, code: str, client: OAuth2Client
    ) -> OAuth2AuthorizationCode | None:
        auth_code = (
            run_sql(
                select(OAuth2AuthorizationCode).filter_by(
                    code=code, client_id=client.client_id
                )
            )
            .scalars()
            .first()
        )
        if auth_code and not auth_code.is_expired():
            return auth_code
        return None

    def delete_authorization_code(
            self, authorization_code: OAuth2AuthorizationCode
    ) -> None:
        db.session.delete(authorization_code)
        db.session.commit()

    def authenticate_user(self, authorization_code: OAuth2AuthorizationCode) -> User:
        return db.session.get(User, authorization_code.user_id)


def query_client(client_id: str) -> OAuth2Client:
    if client_id not in ALLOWED_CLIENTS:
        raise Exception(f"OAuth2 client {client_id} is not in allowed list")
    return ALLOWED_CLIENTS[client_id]


save_token = create_save_token_func(db.session, OAuth2Token)
auth_server = AuthorizationServer(query_client=query_client, save_token=save_token)

require_oauth = ResourceProtector()
"""Special decorator to request for permission scopes"""


def delete_expired_oauth2_tokens() -> None:
    now_time = int(time.time())
    run_sql(
        delete(OAuth2Token).where(
            (OAuth2Token.expires_in + OAuth2Token.issued_at < now_time)
            | (OAuth2Token.access_token_revoked_at < now_time)
            | (OAuth2Token.refresh_token_revoked_at < now_time)
        )
    )
    db.session.commit()


def init_oauth(app: Flask) -> None:
    global ALLOWED_CLIENTS
    clients = app.config.get("OAUTH2_CLIENTS", [])
    schema = class_schema(OAuth2Client)()
    clients_obj: list[OAuth2Client] = [schema.load(c) for c in clients]
    ALLOWED_CLIENTS = {c.client_id: c for c in clients_obj}

    auth_server.init_app(app)
    auth_server.register_grant(AuthorizationCodeGrant, [CodeChallenge(required=True)])
    auth_server.register_grant(RefreshTokenGrant)

    # TODO: Do we need to support revocation?

    from timApp.auth.oauth2.routes import oauth

    app.register_blueprint(oauth)

    bearer_cls = create_bearer_token_validator(db.session, OAuth2Token)
    require_oauth.register_token_validator(bearer_cls())
