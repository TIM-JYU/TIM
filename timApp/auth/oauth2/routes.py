from flask import render_template_string, Response

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.oauth2.oauth2 import auth_server
from timApp.auth.sessioninfo import get_current_user_object, logged_in
from timApp.util.flask.typedblueprint import TypedBlueprint

oauth = TypedBlueprint('oauth', __name__, url_prefix='/oauth')


@oauth.get('authorize')
def request_authorization() -> str:
    if not logged_in():
        return render_template_string("""<p>Not logged in!</p>""")
    grant = auth_server.get_consent_grant(end_user=get_current_user_object())
    return render_template_string("""<p>Got grant: {{grant.__dict__}}</p>""", grant=grant)


@oauth.post('authorize')
def authorize(confirm: bool) -> Response:
    verify_logged_in()
    return auth_server.create_authorization_response(grant_user=get_current_user_object() if confirm else None)


@oauth.post('token')
def issue_token() -> Response:
    return auth_server.create_token_response()
