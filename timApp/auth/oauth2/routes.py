from authlib.integrations.flask_oauth2 import current_token
from authlib.oauth2 import OAuth2Error
from authlib.oauth2.rfc6749 import BaseGrant, scope_to_list
from flask import Response, render_template

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.oauth2.models import (
    OAuth2Client,
    Scope,
)
from timApp.auth.oauth2.oauth2 import (
    auth_server,
    require_oauth,
)
from timApp.auth.sessioninfo import get_current_user_object, logged_in
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.util.flask.responsehelper import json_response, safe_redirect
from timApp.util.flask.typedblueprint import TypedBlueprint

oauth = TypedBlueprint("oauth", __name__, url_prefix="/oauth")


@oauth.get("authorize")
def request_authorization() -> str | Response:
    user = get_current_user_object() if logged_in() else None
    try:
        grant: BaseGrant = auth_server.get_consent_grant(end_user=user)
    except OAuth2Error:
        return safe_redirect("/")
    client: OAuth2Client = grant.client
    scope = client.get_allowed_scope(grant.request.scope)
    return render_template(
        "oauth_authorize.jinja2", client=client, scopes=scope_to_list(scope)
    )


@oauth.post("authorize")
def authorize(confirm: bool) -> Response:
    verify_logged_in()
    return auth_server.create_authorization_response(
        grant_user=get_current_user_object() if confirm else None
    )


@oauth.post("token")
@csrf.exempt
def issue_token() -> Response:
    return auth_server.create_token_response()


@oauth.get("profile")
@require_oauth(Scope.profile.name)
def get_user_profile() -> Response:
    user: User = current_token.user
    # Implement profile output here to make sure not too much information is given out for this scope
    return json_response(
        {
            "id": user.id,
            "emails": [
                {  # First email is considered primary
                    "email": user.email,
                    "verified": True,
                }
            ],
            "last_name": user.last_name,
            "given_name": user.given_name,
            "real_name": user.real_name,
            "username": user.name,
        }
    )


@oauth.post("introspect")
@require_oauth()
@csrf.exempt
def introspect_token() -> Response:
    token = current_token
    return json_response(
        {
            "active": True,
            "client_id": token.client_id,
            "token_type": token.token_type,
            "username": token.user.name,
            "scope": token.get_scope(),
            "aud": token.client_id,
            "exp": token.expires_in,
            "iat": token.issued_at,
        }
    )
