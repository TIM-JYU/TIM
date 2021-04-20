from dataclasses import dataclass, field
from enum import Enum
from typing import List, Dict


class ArchiveType(Enum):
    """Different supported archive types."""
    # If you change this, make sure the mapping for Mailman's archive policies is also updated at
    # mailman_archive_policy_correlate. TIM's and Mailman's archive policies aren't a one-to-one match.

    # No archiving at all for list. Equals to Mailman's archive policy of 'none'.
    NONE = 0
    # Secret archive. Only for owner/moderators. No direct correlation with Mailman's archive policies.
    SECRET = 1
    # For group and it's members' eyes only. Equal for Mailman's archive policy of 'private'.
    GROUPONLY = 2
    # Anyone with a link can access archive. No direct correlation with Mailman's archive policies.
    UNLISTED = 3
    # Completely public (and advertised) archive. Equals to Mailman's archive policy of 'public'.
    PUBLIC = 4


@dataclass
class ListOptions:
    """All options regarding message lists."""
    listname: str
    domain: str
    # Enums need this to help marshmallow decipher JSON values in from client side properly.
    archive: ArchiveType = field(metadata={'by_value': True})
    # VIESTIM: Is this needed?
    # emails: List[str]
    listDescription: str
    listInfo: str


# A list of tuples mapping TIM's archive policies to Mailman's archive policies. Mailman's archive policies are
# listed here: https://gitlab.com/mailman/mailman/-/blob/master/src/mailman/interfaces/archiver.py
mailman_archive_policy_correlate: Dict[ArchiveType, str] = {
    ArchiveType.NONE: "none",
    # Secret archive type doesn't exist in Mailman. Because Mailman's private archive policy is open for list
    # member's, we turn Mailman's archiving off and rely solely on TIM's archiving.
    ArchiveType.SECRET: "none",
    ArchiveType.GROUPONLY: "private",
    # Unlisted archive type doesn't exist in Mailman, but closest is setting policy as private and provide necessary
    # archive links from TIM.
    ArchiveType.UNLISTED: "private",
    ArchiveType.PUBLIC: "public"
}
