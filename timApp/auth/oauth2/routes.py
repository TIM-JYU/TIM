from authlib.oauth2.rfc6749 import BaseGrant
from authlib.oauth2.rfc6749 import BaseGrant
from flask import Response, render_template

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.oauth2.models import OAuth2Client, describe_scope
from timApp.auth.oauth2.oauth2 import auth_server
from timApp.auth.sessioninfo import get_current_user_object, logged_in
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
def issue_token() -> Response:
    return auth_server.create_token_response()
