import datetime
from authlib.integrations.flask_oauth2 import current_token
from authlib.oauth2 import OAuth2Error
from authlib.oauth2.rfc6749 import BaseGrant, scope_to_list
from flask import Response, render_template

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.oauth2.models import OAuth2Client, Scope
from timApp.auth.oauth2.oauth2 import auth_server, require_oauth
from timApp.auth.sessioninfo import get_current_user_object, logged_in
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.util.flask.responsehelper import json_response, safe_redirect
from timApp.util.flask.typedblueprint import TypedBlueprint
from authlib.oauth2.rfc7662 import IntrospectionEndpoint

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


@oauth.get("tokenValidity")
@require_oauth(Scope.profile.value)
def token_is_valid() -> Response:
    """
    Check if the token is valid
    :return: JSON response with the token status and validity time (hours:minutes:seconds)
    """

    # Convert seconds to days: hours:minutes:seconds
    convert = str(datetime.timedelta(seconds=current_token.expires_in))

    return json_response({"validityTime": convert})


# class IdeIntrospectionEndpoint(IntrospectionEndpoint):
#     def query_token(self, token, token_type_hint):
#         if token_type_hint == "access_token":
#             tok = Token.query.filter_by(access_token=token).first()
#         elif token_type_hint == "refresh_token":
#             tok = Token.query.filter_by(refresh_token=token).first()
#         else:
#             # without token_type_hint
#             tok = Token.query.filter_by(access_token=token).first()
#             if not tok:
#                 tok = Token.query.filter_by(refresh_token=token).first()
#         return tok
#
#     def introspect_token(self, token):
#         return {
#             "active": True,
#             "client_id": token.client_id,
#             "token_type": token.token_type,
#             "scope": token.get_scope(),
#             "aud": token.client_id,
#             "exp": token.expires_at,
#             "iat": token.issued_at,
#         }
#
#     def check_permission(self, token, client, request):
#         # for example, we only allow internal client to access introspection endpoint
#         return client.client_type == "internal"
#
#
# # register it to authorization server
# auth_server.register_endpoint(IdeIntrospectionEndpoint)
#
#
# @oauth.post("introspect")
# def introspect_token():
#     return auth_server.create_endpoint_response(IdeIntrospectionEndpoint.ENDPOINT_NAME)
