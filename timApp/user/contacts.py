from dataclasses import field

from flask import Response
from sqlalchemy import func, select
from sqlalchemy.orm import load_only

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_object
from timApp.messaging.messagelist.listinfo import Channel
from timApp.timdb.sqa import db, run_sql
from timApp.user.usercontact import UserContact, ContactOrigin, PrimaryContact
from timApp.user.verification.verification import (
    resend_verification,
    ContactAddVerification,
    request_verification,
    SetPrimaryContactVerification,
)
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import is_valid_email

contacts = TypedBlueprint("contacts", __name__, url_prefix="/contacts")


@contacts.post("/add")
def add_contact(
    contact_info: str,
    contact_info_type: Channel = field(metadata={"by_value": True}),
    resend_if_exists: bool = False,
) -> Response:
    """Add a new contact information for current user.

    :param contact_info_type: The channel user wishes to add a new contact information.
    :param contact_info: The contact information.
    :param resend_if_exists: If True and verification already exists, resend the verification message.
    :return: OK response.
    """
    verify_logged_in()
    user = get_current_user_object()
    # Check for duplicate contact information.
    existing_contact_info = user.get_contact(
        contact_info_type,
        contact_info,
        [load_only(UserContact.id, UserContact.verified)],
    )

    if existing_contact_info:
        # If the contact info already exists and is verified by the user, resend verification
        if existing_contact_info.verified:
            raise RouteException("The contact is already added")
        if not resend_if_exists:
            raise RouteException(
                "The contact is already added but is pending verification"
            )
        verification = (
            run_sql(
                select(ContactAddVerification)
                .filter_by(contact=existing_contact_info, reacted_at=None)
                .limit(1)
            )
            .scalars()
            .first()
        )
        if verification:
            resend_verification(verification, existing_contact_info.contact)
        else:
            # Verification was cleaned after it wasn't used for a while, create a new one
            request_verification(
                ContactAddVerification(user=user, contact=existing_contact_info),
                existing_contact_info.contact,
            )
            db.session.commit()
        return json_response({"requireVerification": False})

    # Add appropriate contact info.
    require_verification = False
    if contact_info_type == Channel.EMAIL:
        require_verification = True
        if not is_valid_email(contact_info):
            raise RouteException("Email format is invalid")

        uc = UserContact(
            user=user,
            contact=contact_info,
            channel=Channel.EMAIL,
            verified=False,
            contact_origin=ContactOrigin.Custom,
        )
        db.session.add(uc)
        request_verification(ContactAddVerification(user=user, contact=uc), uc.contact)

    db.session.commit()
    return json_response({"requireVerification": require_verification})


@contacts.post("/remove")
def remove_contact(
    contact_info: str, contact_info_type: Channel = field(metadata={"by_value": True})
) -> Response:
    """Remove a contact information from current user.

    :param contact_info_type: The channel user wishes to add a new contact information.
    :param contact_info: The contact information.
    :return: OK response.
    """
    verify_logged_in()
    user = get_current_user_object()

    if contact_info_type == Channel.EMAIL and user.email == contact_info:
        raise RouteException("Cannot remove primary email")

    contact = user.get_contact(
        contact_info_type,
        contact_info,
        [load_only(UserContact.id, UserContact.channel)],
    )

    if not contact:
        raise RouteException("The contact does not exist for the user")

    if contact.primary:
        raise RouteException(
            "Cannot remove a primary contact. Set a new primary contact before removing this one."
        )

    if contact.contact_origin != ContactOrigin.Custom:
        raise RouteException("Cannot remove managed contacts")

    verified_emails_count = db.session.scalar(
        select(func.count(UserContact.id)).filter_by(
            user_id=user.id,
            channel=Channel.EMAIL,
            verified=True,
            contact_origin=ContactOrigin.Custom,
        )
    )

    if contact.verified and verified_emails_count == 1:
        raise RouteException(
            "Removing the email will leave the user with no verified user-added emails. "
            "Add at least one verified email before removing this one."
        )

    db.session.delete(contact)
    db.session.commit()
    return ok_response()


@contacts.post("/primary")
def set_primary(
    contact: str, channel: Channel = field(metadata={"by_value": True})
) -> Response:
    """Set the primary contact.

    :param contact: Primary contact value.
    :param channel: Primary contact channel.
    :return: OK response.
    """
    verify_logged_in()
    user = get_current_user_object()

    existing_contact = user.get_contact(
        channel,
        contact,
        [load_only(UserContact.id, UserContact.contact, UserContact.primary)],
    )

    if not existing_contact:
        raise RouteException("The contact does not exist for the user")

    if existing_contact.primary:
        json_response({"verify": False})

    primary_contact_exists = (
        run_sql(
            select(UserContact.id)
            .filter_by(channel=channel, contact=contact, primary=PrimaryContact.true)
            .limit(1)
        )
        .scalars()
        .first()
        is not None
    )

    if primary_contact_exists:
        raise RouteException(
            "Another user already has this contact set to primary, please choose another contact"
        )

    existing_verification = (
        run_sql(
            select(SetPrimaryContactVerification).filter_by(
                contact=existing_contact,
                reacted_at=None,
            )
        )
        .scalars()
        .first()
    )
    if existing_verification:
        resend_verification(existing_verification)
        return json_response({"verify": True})

    current_primary = user.primary_email_contact
    need_verify = False

    if not current_primary:
        user.email = existing_contact.contact
    else:
        request_verification(
            SetPrimaryContactVerification(user=user, contact=existing_contact)
        )
        need_verify = True

    db.session.commit()
    return json_response({"verify": need_verify})
