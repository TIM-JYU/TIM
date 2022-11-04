import functools
from dataclasses import dataclass
from enum import Enum
from typing import Any, Optional, TypeVar

REFEDS_PREFIX = "https://refeds.org/assurance"
REFEDS_LOCAL_ENTRPRISE_ASSURANCE = f"{REFEDS_PREFIX}/IAP/local-enterprise"


@functools.total_ordering
class RefedsIapLevel(Enum):
    """Valid Identity Assurance Proofing levels for REFEDS Assurance Framework ver 1.0 based on
    https://wiki.refeds.org/display/ASS/REFEDS+Assurance+Framework+ver+1.0
    """

    low = f"{REFEDS_PREFIX}/IAP/low"
    medium = f"{REFEDS_PREFIX}/IAP/medium"
    high = f"{REFEDS_PREFIX}/IAP/high"

    @staticmethod
    @functools.cache
    def as_list() -> list["RefedsIapLevel"]:
        return list(RefedsIapLevel)

    def __lt__(self, other: Any) -> bool:
        if not isinstance(other, RefedsIapLevel):
            return NotImplemented
        return RefedsIapLevel.as_list().index(self) < RefedsIapLevel.as_list().index(
            other
        )

    @staticmethod
    def from_string(s: str) -> Optional["RefedsIapLevel"]:
        try:
            return RefedsIapLevel(s)
        except ValueError:
            return None


@dataclass(frozen=True)
class IdentityAssuranceProofing:
    """Represents user's Identity Assurance Proofing (IAP) level.
    IAP describes how the user's identity is assured (e.g. email, government ID, etc.)

    Attributes:
        highest_refeds_level: Highest IAP level according to REFEDS Assurance Framework
        local_enterprise: If True, user's identity proofing is good enough to access Home Organisations'
                          administrative systems.
    """

    highest_refeds_level: RefedsIapLevel
    local_enterprise: bool


_T = TypeVar("_T")
