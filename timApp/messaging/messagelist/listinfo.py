from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum


class Channel(Enum):
    """The message channels TIM uses and provides for users or message lists."""

    TIM_MESSAGE = "tim_message"
    EMAIL_LIST = "email_list"
    EMAIL = "email"


@dataclass
class Distribution:
    """A class to wrap information about the message channels used by a message list or TIM users."""

    tim_message: bool
    email_list: bool


class ArchiveType(Enum):
    """Different supported archive types."""

    # If you change this, make sure the mapping for Mailman's archive policies is also updated at
    # mailman_archive_policy_correlate. TIM's and Mailman's archive policies aren't a one-to-one match.

    # No archiving at all for list. Equals to Mailman's archive policy of 'none'.
    NONE = 0
    # Secret archive. Only for owner(and moderators?). No direct correlation with Mailman's archive policies.
    SECRET = 1
    # For group and its members' eyes only. Equal for Mailman's archive policy of 'private'.
    GROUPONLY = 2
    # Logged in TIM users can see the list.
    UNLISTED = 3
    # Completely public archive, people don't have to be logged in to see the archive. Equals to Mailman's archive
    # policy of 'public'.
    PUBLIC = 4


class MessageVerificationType(Enum):
    """
    Options for verifying messages in mailing lists.
    """

    NONE = "none"
    """
    No verification is done.
    
    .. note:: With no verification, there is no assurance that messages will be delivered to the users.
              The message might get stuck in a spam filter or get rejected by the mail server. 
    """

    FORWARD = "forward"
    """
    Message list operates in a forward mode. All options that modify message title or contents are ignored.
    
    .. note:: In this mode, messages will be delivered based on authentication of the sender.
              If the sender's message does not include needed verification information (e.g. DKIM signature),
              the message is still rejected.
    """

    MUNGE_FROM = "munge_from"
    """
    Message list munges (i.e. modifies) the From header of the message to that of the list and signs the messages.
    This is the default mode for message lists.
    
    .. note:: In this mode, messages will be delivered using the list's signature.
              With this option, messages are unlikely to be rejected by the mail server.
              However, the sender will be changed to the list's address.
    """


class ReplyToListChanges(Enum):
    """Options for email list's own address to be added to the Reply-To header for emails that are sent through the
    list. See reply_to_munging for mapping to Mailman's options."""

    # Don't meddle with the Reply-To header.
    NOCHANGES = "no_munging"
    # Change the Reply-To header so that the message/email list is preferred(/forced) to be included in the replies.
    ADDLIST = "point_to_list"


@dataclass
class ListInfo:
    """All options regarding message lists."""

    name: str
    """The name of the message list. A mandatory value when list options are considered."""

    archive: ArchiveType = field(metadata={"by_value": True})
    """The type of archive policy this list uses."""

    default_reply_type: ReplyToListChanges | None = field(
        metadata={"by_value": True}, default=None
    )
    """The default reply type of the list."""

    verification_type: MessageVerificationType | None = field(
        metadata={"by_value": True}, default=None
    )
    """The message verification type of the list."""

    notify_owners_on_list_change: bool | None = None
    """A flag that determines if owners of the message list are notified of certain changes regarding the list, 
    e.g. a new user joins the list. """

    only_text: bool | None = None
    """If only pure text is allowed on a list."""

    list_description: str | None = None
    """A short description of the list and its purpose."""

    list_info: str | None = None
    """Additional information about the list."""

    email_admin_url: str | None = None
    """If the message list has an email list associated with it, this is the link to Mailman's advanced list 
    controls. """

    tim_users_can_join: bool | None = None
    """Flag used to determine if TIM users can directly join this list."""

    members_can_unsubscribe: bool | None = None
    """Flag used to determine if the TIM members of this list can leave the list on their own."""

    default_send_right: bool | None = None
    """The list's default send right for (new) members."""

    default_delivery_right: bool | None = None
    """The list's default delivery right for (new) members."""

    list_subject_prefix: str | None = None
    """Messages routed by a message list will have this subject prefix added to them."""

    domain: str | None = None
    """The domain of the message list, if it has email list associated with it."""

    non_member_message_pass: bool | None = None
    """A flag that controls if messages from non members are automatically passed to the list."""

    allow_attachments: bool | None = None
    """A flag controlling if attachments are allowed on the list."""

    distribution: Distribution | None = None
    """All the message channels the list is using."""

    removed: datetime | None = None
    """If set, shows the date the message list is set to not be in use. If a user has access to the admin document 
    even if this is set, it means that the message list is frozen, but not completely deleted. """


@dataclass
class MemberInfo:
    """Wrapper for information about a member on a message list."""

    name: str
    username: str
    sendRight: bool
    deliveryRight: bool
    email: str
    removed: datetime | None = None


@dataclass
class GroupAndMembers:
    """Helper class for querying user group and its members."""

    groupName: str
    members: list[MemberInfo]


# A mapping of TIM's archive policies to Mailman's archive policies. Mailman's archive policies are listed here:
# https://gitlab.com/mailman/mailman/-/blob/master/src/mailman/interfaces/archiver.py
mailman_archive_policy_correlate: dict[ArchiveType, str] = {
    ArchiveType.NONE: "never",
    # Secret archive type doesn't exist in Mailman. Because Mailman's private archive policy is open for list
    # member's, we turn Mailman's archiving off and rely solely on TIM's archiving.
    ArchiveType.SECRET: "never",
    ArchiveType.GROUPONLY: "private",
    # Unlisted archive type doesn't exist in Mailman, but closest is setting policy as private and provide necessary
    # archive links from TIM.
    ArchiveType.UNLISTED: "private",
    ArchiveType.PUBLIC: "public",
}
