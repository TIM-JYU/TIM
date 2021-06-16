import secrets
from dataclasses import field
from enum import Enum

from flask import Response

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user
from timApp.messaging.messagelist.listoptions import Channel
from timApp.notification.send_email import send_email
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.usercontact import UserContact
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import is_valid_email, get_current_time

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
    # TODO: Check for duplicate contact information?

    # Random string for verification URL.
    verification_string = secrets.token_urlsafe(32)
    verification_url = f"{app.config['TIM_HOST']}/verification/contact/{verification_string}"
    uc = None
    # Add appropriate contact info.
    if contact_info_type is Channel.EMAIL:
        if not is_valid_email(contact_info):
            # If the email contact information is considered to not be a valid email address, then return an
            # exception to the user.
            raise RouteException("01")
        # TODO: Generate verification information for db.
        # TODO: Generate contact info in db, with verified = False.
        u_id = get_current_user().id
        uc = UserContact(user_id=u_id, contact=contact_info, channel=Channel.EMAIL, verified=False, primary=False)
        db.session.add(uc)
    ver = Verification(verification_type=VerificationType.CONTACT_OWNERSHIP, verification_pending=get_current_time(),
                       verification_token=verification_string, contact=uc)
    db.session.add(ver)
    send_email(contact_info, "New TIM contact information verification link", f"""Hey, 

someone requested to add a new email ({contact_info}) to their TIM account. If this person was you, then you may 
click the link at the end of this message and your contact information will be added to your user profile in TIM. 

If this was not you, then please disregard this message, someone most likely accidentally wrote the wrong contact 
information for themselves. If, however, this is among a multitude of requests that are not from you, then please 
forward this message to {app.config['HELP_EMAIL']} and TIM support will be deal with it.

Verification link (click only if you requested this action):
{verification_url}

""")
    db.session.commit()
    return ok_response()


class VerificationType(Enum):
    """Type of verification, used to direct the proper verification action afterwards."""
    LIST_JOIN = 1
    """A user has been invited to a message list."""
    CONTACT_OWNERSHIP = 2
    """A user has added a new contact information for themselves, and it's verified they are in possession of said 
    contact information. """


class Verification(db.Model):
    """For various pending verifications, such as message list joining and contact information ownership
    verification."""

    __tablename__ = "verifications"

    id = db.Column(db.Integer, primary_key=True)

    contact_id = db.Column(db.Integer, db.ForeignKey("user_contacts.id"), primary_key=True)

    verification_type = db.Column(db.Enum(VerificationType), nullable=False)
    """The type of verification, see VerificationType class for details."""

    verification_pending = db.Column(db.DateTime(timezone=True))
    """When a verification has been added to db, pending sending to a user."""

    verification_token = db.Column(db.Text, nullable=False)
    """Generated verification link. This is given to the user and once they click on it, they are verified (in 
    whatever it was that needed verification)."""

    verified_at = db.Column(db.DateTime(timezone=True))
    """When the user used the link to verify."""

    contact = db.relationship("UserContact", back_populates="verification", lazy="select", uselist=False)
    """Relationship to UserContact, to allow connecting without db flushing first."""

