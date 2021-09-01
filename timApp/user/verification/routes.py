from dataclasses import field
from typing import Union

from flask import Response, request, render_template
from sqlalchemy.orm.exc import NoResultFound, MultipleResultsFound  # type: ignore

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_object
from timApp.messaging.messagelist.listinfo import Channel
from timApp.messaging.messagelist.messagelist_utils import sync_new_contact_info
from timApp.notification.send_email import send_email
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.usercontact import UserContact
from timApp.user.verification.verification import Verification, VerificationType, request_verification, \
    ContactAddVerification
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_error, log_warning
from timApp.util.utils import is_valid_email, get_current_time

verification = TypedBlueprint('verification', __name__, url_prefix='/verification')


@verification.post("/addnewcontact")
def add_contact_info(contact_info: str, contact_info_type: Channel = field(metadata={'by_value': True})) \
        -> Response:
    """Add a new contact information for a TIM user.

    :param contact_info_type: The channel user wishes to add a new contact information.
    :param contact_info: The contact information.
    :return: OK response.
    """
    verify_logged_in()
    user = get_current_user_object()
    # Check for duplicate contact information.
    existing_contact_info = db.session.query(UserContact.verified).filter(
        (UserContact.user == user) & (UserContact.channel == contact_info_type) & (
                UserContact.contact == contact_info)).first()

    if existing_contact_info:
        # If the contact info already exists and is verified by the user, then inform them about it.
        raise RouteException("02")

    # Add appropriate contact info.
    if contact_info_type is Channel.EMAIL:
        if not is_valid_email(contact_info):
            raise RouteException("01")

    uc = UserContact(user=user, contact=contact_info, channel=Channel.EMAIL, verified=False, primary=False)
    db.session.add(uc)

    request_verification(user, ContactAddVerification(contact=uc), "Verify test", """
This is a verification message.

URL: {{ verify_url }}

""")

    db.session.commit()
    return ok_response()


@verification.get("/<verify_type>/<verify_token>")
def show_verify_page(verify_type: str, verify_token: str):
    verify_type_parsed = VerificationType.parse(verify_type)
    error = None
    if not verify_type_parsed:
        error = "Invalid verification type"
    verification_obj = Verification.query.get((verify_token, verify_type_parsed))
    if not verification_obj:
        error = "No verification found for the given data"
    return render_template("user_action_verification.jinja2",
                           error=error,
                           verify_type=verify_type,
                           verify_info=verification_obj)


@verification.route("/old/contact/<verification_token>", methods=["GET", "POST"])
def tmp_contact_info_verification(verification_token: str) -> Union[Response, str]:
    """Verify user's additional contact information.

    :param verification_token: Generated string token to identify user's not-yet-verified contact information.
    :return: For successes:
      - If the HTTP method is GET, return template 'contact-info-verification.jinja2' with necessary substituted
       variables.
      - If the HTTP method is POST and contact verification token was found in the db, return OK response.
    For failures:
      - Return error code '01' if the verification token is not found in the db.
      - Return error code '02' if during a GET call the user's contact information has already been verified.
      - Return error code '03' if the user tries to verify the same contact info in multiple POST calls.
    """
    template = 'user_action_verification.jinja2'
    try:
        v = Verification.query.filter_by(verification_token=verification_token).one()
        if v.verification_type == VerificationType.CONTACT_OWNERSHIP:
            user_contact = v.contact
            if request.method == 'GET':
                if v.verified_at:
                    # The contact info is already verified.
                    return render_template(template, verification_token=verification_token, error=True, error_code="02",
                                           title="Contact information verification", type=v.verification_type,
                                           channel=user_contact.channel, contact_info=user_contact.contact)
                else:
                    # The contact info is not yet verified.
                    return render_template(template, verification_token=verification_token, error=False,
                                           error_code=None, title="Contact information verification",
                                           type=v.verification_type, channel=user_contact.channel,
                                           contact_info=user_contact.contact)
            else:
                if v.verified_at:
                    # The contact info is already verified. We should only be here if the user has gone outside of
                    # the beaten path to make a POST call.
                    raise RouteException("03")
                else:
                    # THe contact info is not yet verified.
                    # If the method is POST, the user has verified their contact info.
                    user_contact.verified = True
                    v.verified_at = get_current_time()
                    # Sync the now verified contact info to relevant message lists.
                    sync_new_contact_info(user_contact)

                    db.session.commit()
                    return ok_response()
    except NoResultFound:
        # We are most likely here if someone copy-pasted their verification link badly to the browser.
        # Assume this comes from a GET request.
        return render_template(template, verification_token=verification_token, error=True,
                               error_code="01", title="Verification error", type=None)
    except MultipleResultsFound:
        # If we are here, we have found multiple same tokens in the db. Something is most likely wrong with token
        # generation.
        log_error(f"Multiple verification tokens found in db (with token: '{verification_token}').")
        return render_template(template, verification_token=verification_token, error=True,
                               error_code="Invalid verification link.", title="Verification error", type=None)
    return ok_response()


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
    forward this message to {app.config['HELP_EMAIL']} and TIM support will deal with it.

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
