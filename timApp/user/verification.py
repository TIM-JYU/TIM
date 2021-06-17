import secrets
from dataclasses import field
from enum import Enum

from flask import Response
from sqlalchemy.orm.exc import NoResultFound, MultipleResultsFound

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
from timApp.util.logger import log_warning, log_error
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
    u_id = get_current_user().id
    # Check for duplicate contact information.
    existing_contact_info = UserContact.query.filter_by(user_id=u_id, contact=contact_info,
                                                        channel=contact_info_type).first()

    if existing_contact_info and existing_contact_info.verified:
        # If the contact info already exists and is verified by the user, then inform them about it.
        raise RouteException("02")

    # Random string for verification URL.
    verification_string = secrets.token_urlsafe(32)
    verification_url = f"{app.config['TIM_HOST']}/verification/contact/{verification_string}"
    uc = None
    # Add appropriate contact info.
    if contact_info_type is Channel.EMAIL:
        if not is_valid_email(contact_info):
            raise RouteException("01")
        if not existing_contact_info:
            # Generate new contact info in db, with verified = False to wait for the user to verify this.
            uc = UserContact(user_id=u_id, contact=contact_info, channel=Channel.EMAIL, verified=False, primary=False)
            db.session.add(uc)
    # Generate verification information for db.
    ver = Verification(verification_type=VerificationType.CONTACT_OWNERSHIP, verification_pending=get_current_time(),
                       verification_token=verification_string, contact=uc)
    db.session.add(ver)
    send_verification_messsage(contact_info, verification_url, contact_info_type)
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


def send_verification_messsage(contact_info: str, verification_url: str, channel: Channel) -> None:
    """Send verification messages to appropriate message channels.

    :param contact_info: The contact information to receive the verification message.
    :param verification_url: The URL in TIM to give to the owner of the contact information, so they can verify their
    ownership of said contact information.
    :param channel: The medium where the message is sent.
    """
    if channel is Channel.EMAIL:
        send_email(contact_info, "TIM-yhteystiedon vahvistuslinkki / New TIM contact information verification link",
                   f"""In English below.

    Joku on pyytänyt liittämään tämän sähköpostiosoitteen TIM-tiliinsä. Jos tämä on tapahtunut sinun aloitteestasi, 
    niin voit painaa myöhemmin tässä viestissä olevaa linkkiä ja tämä yhteystieto lisätään profiiliisi TIMissä. 

    Jos tämä tapahtuma ei ole sinun aloitteestasi, voit jättää tämän viestin huomiotta. Todennäköisesti TIM-käyttäjä on 
    vahingossa yrittänyt lisätä itselleen väärän yhteystiedon. Jos kuitenkin tämä on osa usean samanlaisen viestin 
    sarjaa, niin ole hyvä ja edelleenlähetä tämä viesti osoitteeseen {app.config['HELP_EMAIL']} ja TIMin tuki hoitaa 
    asian.

    Vahvistuslinkki (paina ainostaan jos pyysit tätä toimintoa):
    {verification_url}


    In English:

    Someone requested to add a new email ({contact_info}) to their TIM account. If this person was you, then you may 
    click the link at the end of this message and your contact information will be added to your user profile in TIM. 

    If this was not you, then please disregard this message, someone most likely accidentally wrote the wrong contact 
    information for themselves. If, however, this is among a multitude of requests that are not from you, then please 
    forward this message to {app.config['HELP_EMAIL']} and TIM support will be deal with it.

    Verification link (click only if you requested this action):
    {verification_url}
    """)
    else:
        # Technically we should not be here, as it would mean somehow the contact info is meant for a channel that
        # TIM does not yet handle. Log this anomaly for later investigation (or remind us to implement a handler for
        # it).
        log_warning(f"Tried to send verification message to an unhandled message channel '{channel}' in "
                    f"verification.py.")
        # The user needs some kind of feedback to know that they are not getting their verification message as they
        # might expect.
        raise RouteException("03")


@verification.route("/contact/<verification_token>", methods=['POST'])
def contact_info_verification(verification_token: str) -> Response:
    """Verify user's additional contact information.

    :param verification_token: Generated string token
    :return: OK response.
    """
    try:
        v = Verification.query.filter_by(verification_token=verification_token).one()
        # Verify the contact information the token corresponds with.
        contact_info = v.contact
        contact_info.verified = True
        v.verified_at = get_current_time()
    except NoResultFound:
        # We are most likely here if someone copy-pasted their verification link badly to the browser.
        raise RouteException("Verification could not be done. Make sure you used the correct link.")
    except MultipleResultsFound:
        # If we are here, we have found multiple same tokens in the db. Something is most likely wrong with token
        # generation.
        log_error(f"Multiple verification tokens found in db (token: {verification_token}).")
    db.session.commit()
    return ok_response()
