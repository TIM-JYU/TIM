"""The state model defined in this module is used by both textfield and numericfield."""
from dataclasses import dataclass
from typing import Union, Dict

from marshmallow.utils import _Missing, missing


@dataclass
class TextfieldStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    c: Union[str, int, float, None]
    styles: Union[Dict[str, str], _Missing] = missing
