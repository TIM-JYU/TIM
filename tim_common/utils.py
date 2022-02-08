from typing import Any, Optional, Mapping, Dict

import marshmallow
from isodate import Duration, duration_isoformat, parse_duration
from marshmallow import ValidationError
from marshmallow.fields import Boolean
from marshmallow.utils import _Missing

Missing = _Missing

_BoolField = Boolean()


def parse_bool(value: Any, default: bool = False) -> bool:
    try:
        return _BoolField.deserialize(value)
    except ValidationError:
        return default


class DurationField(marshmallow.fields.Field):
    def _serialize(
        self, value: Duration, attr: str | None, obj: Any, **kwargs: dict[str, Any]
    ) -> str:
        return duration_isoformat(value)

    def _deserialize(
        self,
        value: Any,
        attr: str | None,
        data: Mapping[str, Any] | None,
        **kwargs: dict[str, Any]
    ) -> Duration:
        try:
            return parse_duration(value)
        except:
            raise self.make_error("invalid")


class DurationSchema(marshmallow.Schema):
    TYPE_MAPPING = {Duration: DurationField}
