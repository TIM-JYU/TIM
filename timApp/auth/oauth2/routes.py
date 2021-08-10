from authlib.integrations.flask_oauth2 import current_token
from authlib.oauth2.rfc6749 import BaseGrant
from flask import Response, render_template

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.oauth2.models import OAuth2Client, describe_scope, Scope
from timApp.auth.oauth2.oauth2 import auth_server, require_oauth
from timApp.auth.sessioninfo import get_current_user_object, logged_in
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

oauth = TypedBlueprint('oauth', __name__, url_prefix='/oauth')


@oauth.get('authorize')
def request_authorization() -> str:
    user = get_current_user_object() if logged_in() else None
    grant: BaseGrant = auth_server.get_consent_grant(end_user=user)
    client: OAuth2Client = grant.client
    scope = client.get_allowed_scope(grant.request.scope)
    scopes = describe_scope(scope)
    return render_template("oauth_authorize.jinja2", grant=grant, user=user, client=client, scopes=scopes)


@oauth.post('authorize')
def authorize(confirm: bool) -> Response:
    verify_logged_in()
    return auth_server.create_authorization_response(grant_user=get_current_user_object() if confirm else None)


@oauth.post('token')
@csrf.exempt
def issue_token() -> Response:
    return auth_server.create_token_response()


@oauth.get('profile')
@require_oauth(Scope.profile.name)
def get_user_profile() -> Response:
    user: User = current_token.user
    # Implement profile output here to make sure not too much information is given out for this scope
    return json_response({
        "id": user.id,
        "emails": [{  # First email is considered primary
            "email": user.email,
            "verified": True,
        }],
        "last_name": user.last_name,
        "given_name": user.given_name,
        "real_name": user.real_name,
        "username": user.name
    })
