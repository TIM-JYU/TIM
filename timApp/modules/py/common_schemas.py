"""The state model defined in this module is used by both textfield and numericfield."""
from typing import Union, Dict

from dataclasses import dataclass
from marshmallow.utils import _Missing, missing

from marshmallow_dataclass import class_schema


@dataclass
class TextfieldStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    c: Union[str, float, int, None]
    styles: Union[Dict[str, str], _Missing] = missing


TextfieldStateSchema = class_schema(TextfieldStateModel)
