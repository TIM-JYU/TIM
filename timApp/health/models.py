from enum import Enum
from tim_common.marshmallow_dataclass import dataclass


class CheckStatus(str, Enum):
    """
    Enumeration of possible health check statuses.
    """
    OK = "ok"
    ERROR = "error"
    DEGRADED = "degraded"
    SKIPPED = "skipped"

    def __bool__(self) -> bool:
        return bool(self in [CheckStatus.OK, CheckStatus.SKIPPED])


@dataclass
class HealthStatus:
    """
    Represents the overall health status and individual check results.
    """
    status: CheckStatus
    checks: dict[str, CheckStatus]
