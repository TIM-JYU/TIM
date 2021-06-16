import secrets
from dataclasses import field

from flask import Response

from timApp.auth.accesshelper import verify_logged_in
from timApp.messaging.messagelist.listoptions import Channel
from timApp.notification.send_email import send_email
from timApp.tim_app import app
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import is_valid_email

verification = TypedBlueprint('verification', __name__, url_prefix='/verification')


@verification.route("/addnewcontact", methods=['POST'])
def add_contact_info(contact_info: str, contact_info_type: Channel = field(metadata={'by_value': True})) \
        -> Response:
    """Add a new contact information for a TIM user.

    :param contact_info_type: The channel user wishes to add a new contact information.
    :param contact_info: The contact information.
    :return: OK response.
    """
    verify_logged_in()
    # TODO: Check for duplicate contact information.
    # TODO: Generate verification information for db.
    # Random string for verification URL.
    verification_string = secrets.token_urlsafe(32)
    verification_url = f"{app.config['TIM_HOST']}/verification/contact/{verification_string}"
    # TODO: Send verification link to the contact information.
    if contact_info_type is Channel.EMAIL:
        if not is_valid_email(contact_info):
            # If the email contact information is considered to not be a valid email address, then return an
            # exception to the user.
            raise RouteException("01")
        send_email(contact_info, "New TIM contact information verification link", f"""Hey, 

someone requested to add a new email ({contact_info}) to their TIM account. If this person was you, then you may 
click the link at the end of this message and your contact information will be added to your user profile in TIM. 

If this was not you, then please disregard this message, someone most likely accidentally wrote the wrong contact 
information for themselves. If, however, this is among a multitude of requests that are not from you, then please 
forward this message to {app.config['HELP_EMAIL']} and TIM support will be deal with it.

Verification link (click only if you requested this action):
{verification_url}

""")
    return ok_response()
