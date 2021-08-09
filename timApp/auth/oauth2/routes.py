from flask import render_template_string

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.oauth2.oauth2 import auth_server
from timApp.auth.sessioninfo import get_current_user_object
from timApp.util.flask.typedblueprint import TypedBlueprint

oauth = TypedBlueprint('oauth', __name__, url_prefix='/oauth')


@oauth.get('authorize')
def request_authorization():
    return render_template_string("""
<p>Test!</p>
""")


@oauth.post('authorize')
def authorize(confirm: bool):
    verify_logged_in()
    return auth_server.create_authorization_response(grant_user=get_current_user_object() if confirm else None)


@oauth.post('token')
def issue_token():
    return auth_server.create_token_response()
