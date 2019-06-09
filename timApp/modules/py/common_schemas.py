"""The state model defined in this module is used by both textfield and numericfield."""
from typing import Union

import attr
from marshmallow import Schema, fields, validates, ValidationError, post_load


@attr.s(auto_attribs=True)
class TextfieldStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    c: Union[str, float, None]


ACCEPTED_TYPES = (int, float, str, type(None))


class TextfieldStateSchema(Schema):
    c = fields.Raw(required=True, allow_none=True)

    @validates('c')
    def validate_content(self, c):
        if not isinstance(c, ACCEPTED_TYPES):
            raise ValidationError(f'State should be str, int, float or None but got {type(c)} with value {c}')

    @post_load
    def make_obj(self, data):
        return TextfieldStateModel(**data)
