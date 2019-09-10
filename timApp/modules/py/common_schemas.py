"""The state model defined in this module is used by both textfield and numericfield."""
from typing import Union

import attr
from marshmallow import Schema, fields, validates, ValidationError, post_load
from marshmallow.utils import _Missing


@attr.s(auto_attribs=True)
class TextfieldStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    c: Union[str, float, None]
    styles: Union[dict, _Missing] = None


ACCEPTED_TYPES = (int, float, str, type(None))


class TextfieldStateSchema(Schema):
    c = fields.Raw(required=True, allow_none=True)
    # TODO: Strict dictionary for style keys
    styles = fields.Dict(keys=fields.Str(), values=fields.Str())

    @validates('c')
    def validate_content(self, c):
        if not isinstance(c, ACCEPTED_TYPES):
            raise ValidationError(f'State should be str, int, float or None but got {type(c)} with value {c}')

    @post_load
    def make_obj(self, data, **kwargs):
        return TextfieldStateModel(**data)
