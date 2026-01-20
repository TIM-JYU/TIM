from enum import Enum
from tim_common.marshmallow_dataclass import dataclass


class CheckStatus(str, Enum):
    """
    Enumeration of possible health check statuses.
    """
    # Bad practice: Use lowercase property names, see issue #3934
    ok = "ok"
    error = "error"
    degraded = "degraded"
    skipped = "skipped"

    def __bool__(self) -> bool:
        return bool(self in [CheckStatus.ok, CheckStatus.skipped])


@dataclass
class HealthStatus:
    """
    Represents the overall health status and individual check results.
    """
    status: CheckStatus
    checks: dict[str, CheckStatus]
