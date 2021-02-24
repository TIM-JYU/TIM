from typing import Any, Optional, Mapping, Dict

import marshmallow
from isodate import Duration, duration_isoformat, parse_duration
from marshmallow.utils import _Missing

Missing = _Missing


class DurationField(marshmallow.fields.Field):
    def _serialize(self, value: Duration, attr: Optional[str], obj: Any, **kwargs: Dict[str, Any]) -> str:
        return duration_isoformat(value)

    def _deserialize(self, value: Any, attr: Optional[str], data: Optional[Mapping[str, Any]], **kwargs: Dict[str, Any]) -> Duration:
        try:
            return parse_duration(value)
        except:
            raise self.make_error('invalid')


class DurationSchema(marshmallow.Schema):
    TYPE_MAPPING = {Duration: DurationField}
