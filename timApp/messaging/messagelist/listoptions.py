from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, Optional


class ArchiveType(Enum):
    """Different supported archive types."""
    # If you change this, make sure the mapping for Mailman's archive policies is also updated at
    # mailman_archive_policy_correlate. TIM's and Mailman's archive policies aren't a one-to-one match.

    # No archiving at all for list. Equals to Mailman's archive policy of 'none'.
    NONE = 0
    # Secret archive. Only for owner(and moderators?). No direct correlation with Mailman's archive policies.
    SECRET = 1
    # For group and it's members' eyes only. Equal for Mailman's archive policy of 'private'.
    GROUPONLY = 2
    # Logged in TIM users can see the list.
    UNLISTED = 3  # TODO: Maybe refactor the name to be more descriptive?
    # Completely public archive. Equals to Mailman's archive policy of 'public'.
    PUBLIC = 4


class ReplyToListChanges(Enum):
    """Options for email list's own address to be added to the Reply-To header for emails that are sent through the
    list. See reply_to_munging for mapping to Mailman's options"""
    # Don't meddle with the Reply-To header.
    NOCHANGES = 0
    # Add an explicit Reply-To header to messages.
    ADDLIST = 1


@dataclass
class ListOptions:
    """All options regarding message lists."""
    name: str
    """The name of the message list. A mandatory value when list options are concidered."""

    # VIESTIM: Enums need this to help marshmallow decipher JSON values in from client side properly.
    archive: ArchiveType = field(metadata={'by_value': True})
    """The type of archive policy this list uses."""

    default_reply_type: Optional[ReplyToListChanges] = field(metadata={'by_value': True})
    """The default reply type of the list."""

    notify_owners_on_list_change: Optional[bool] = None
    """A flag that determines if owners of the message list are notified of certain changes regarding the list, 
    e.g. a new user joins the list. """

    only_text: Optional[bool] = None
    """If only pure text is allowed on a list."""

    list_description: Optional[str] = None
    """A short description of the list and it's purpose."""

    list_info: Optional[str] = None
    """Additional information about the list."""

    email_admin_url: Optional[str] = None
    """If the message list has an email list associated with it, this is the link to Mailman's advanced list 
    controls. """

    tim_users_can_join: Optional[bool] = None
    """Flag used to determine if TIM users can directly join this list."""

    members_can_unsubscribe: Optional[bool] = None
    """Flag used to determine if the TIM members of this list can leave the list on their own."""

    default_send_right: Optional[bool] = None
    """The list's default send right for (new) members."""

    default_delivery_right: Optional[bool] = None
    """The list's default delivery right for (new) members."""

    list_subject_prefix: Optional[str] = None
    """Messages routed by a message list will have this subject prefix added to them."""

    domain: Optional[str] = None
    """The domain of the message list, if it has email list associated with it."""


reply_to_munging: Dict[ReplyToListChanges, str] = {
    ReplyToListChanges.NOCHANGES: "no_munging",
    ReplyToListChanges.ADDLIST: "explicit_header"
}

# A mapping of TIM's archive policies to Mailman's archive policies. Mailman's archive policies are listed here:
# https://gitlab.com/mailman/mailman/-/blob/master/src/mailman/interfaces/archiver.py
mailman_archive_policy_correlate: Dict[ArchiveType, str] = {
    ArchiveType.NONE: "never",
    # Secret archive type doesn't exist in Mailman. Because Mailman's private archive policy is open for list
    # member's, we turn Mailman's archiving off and rely solely on TIM's archiving.
    ArchiveType.SECRET: "never",
    ArchiveType.GROUPONLY: "private",
    # Unlisted archive type doesn't exist in Mailman, but closest is setting policy as private and provide necessary
    # archive links from TIM.
    ArchiveType.UNLISTED: "private",
    ArchiveType.PUBLIC: "public"
}
