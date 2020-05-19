import typing
from copy import copy
from dataclasses import dataclass, field, fields, is_dataclass
from datetime import datetime, timezone
from typing import Union, List

import marshmallow
from marshmallow import missing, pre_load

from marshmallow_dataclass import NewType
from utils import Missing


@dataclass
class PointsRule:
    class Meta:
        unknown = 'EXCLUDE'  # Plugins may have custom rules - TIM can ignore them.
    maxPoints: Union[str, int, float, None, Missing] = missing
    allowUserMin: Union[int, float, None, Missing] = missing
    allowUserMax: Union[int, float, None, Missing] = missing
    multiplier: Union[int, float, None, Missing] = missing


class PluginDateTimeField(marshmallow.fields.Field):

    def _serialize(self, value: typing.Any, attr: str, obj: typing.Any, **kwargs):
        raise NotImplementedError

    def _deserialize(self, value: typing.Any, attr: typing.Optional[str],
                     data: typing.Optional[typing.Mapping[str, typing.Any]], **kwargs):
        d = None
        if isinstance(value, datetime):
            d = value
        elif isinstance(value, str):
            try:
                d = datetime.strptime(value, "%Y-%m-%d %H:%M:%S")
            except ValueError:
                raise self.make_error('validator_failed')
        if d:
            if d.tzinfo is None:
                d = d.replace(tzinfo=timezone.utc)
            return d
        raise self.make_error('validator_failed')


PluginDateTime = NewType('PluginDateTime', datetime, field=PluginDateTimeField)


class HiddenFieldsMixin:
    @pre_load
    def process_minus(self, data, **_):
        if isinstance(data, dict):
            data = copy(data)  # Don't modify the original.
            hidden_keys = {k[1:] for k in data.keys() if isinstance(k, str) and k.startswith('-')}
            for k in hidden_keys:
                data[k] = data.pop(f'-{k}')
            data['hidden_keys'] = hidden_keys
        return data


@dataclass
class KnownMarkupFields(HiddenFieldsMixin):
    """Represents the plugin markup fields that are known and used by TIM."""
    anonymous: Union[bool, None, Missing] = missing
    answerLimit: Union[int, None, Missing] = missing
    automd: Union[bool, None, Missing] = missing
    cache: Union[bool, None, Missing] = missing
    deadline: Union[PluginDateTime, datetime, None, Missing] = missing
    fields: Union[List[str], None, Missing] = missing
    header: Union[str, None, Missing] = missing
    headerText: Union[str, None, Missing] = missing
    lazy: Union[bool, Missing] = missing
    maxHeight: Union[str, None, Missing] = field(metadata={'data_key': 'max-height'}, default=missing)
    minHeight: Union[str, None, Missing] = field(metadata={'data_key': 'min-height'}, default=missing)
    pointsRule: Union[PointsRule, None, Missing] = missing
    pointsText: Union[str, None, Missing] = missing
    showPoints: Union[bool, None, Missing] = missing
    starttime: Union[PluginDateTime, datetime, None, Missing] = missing
    stem: Union[str, None, Missing] = missing
    triesText: Union[str, None, Missing] = missing
    useCurrentUser: Union[bool, None, Missing] = missing
    texafterprint: Union[str, None, Missing] = missing
    texbeforeprint: Union[str, None, Missing] = missing
    texprint: Union[str, None, Missing] = missing

    def show_points(self):
        if self.showPoints is not missing:
            return self.showPoints
        return True

    def tries_text(self):
        if self.triesText:
            return self.triesText
        return 'Tries left:'

    def points_text(self):
        if self.pointsText:
            return self.pointsText
        return 'Points:'


def asdict_skip_missing(obj):
    result = []
    for f in fields(obj):
        v = getattr(obj, f.name)
        if v is missing:
            continue
        value = asdict_skip_missing(v) if is_dataclass(v) else v
        result.append((f.name, value))
    return dict(result)


def list_not_missing_fields(inst):
    return list(((k, v) for k, v in asdict_skip_missing(inst).items()))


@dataclass
class UndoInfo:
    button: Union[str, None, Missing] = missing
    title: Union[str, None, Missing] = missing
    confirmation: Union[str, None, Missing] = missing


@dataclass
class GenericMarkupModel(KnownMarkupFields):
    """Specifies which fields the editor can use in the plugin markup.
    This base class defines some fields that are applicable to all plugins.

    The difference to KnownMarkupFields is that this class should define fields that are not used by TIM.

    TODO: Some fields here should be moved to KnownMarkupFields.
    """

    hidden_keys: Union[List[str], Missing] = missing
    """Meta field that keeps track which markup fields were hidden (that is, prefixed with "-").
    Hidden keys are never sent to browser.
    """

    button: Union[str, None, Missing] = missing
    buttonText: Union[str, None, Missing] = missing
    disableUnchanged: Union[bool, Missing, None] = missing
    footer: Union[str, Missing] = missing
    forceBrowser: Union[bool, Missing, None] = missing
    globalField: Union[bool, Missing, None] = missing
    hideBrowser: Union[bool, Missing, None] = missing
    lang: Union[str, None, Missing] = missing
    resetText: Union[str, Missing, None] = missing
    showInView: Union[bool, Missing] = missing
    connectionErrorMessage: Union[str, Missing] = missing
    undo: Union[UndoInfo, Missing, None] = missing

    def get_visible_data(self):
        return {k: v for k, v in list_not_missing_fields(self) if k not in self.hidden_keys and k != 'hidden_keys'}
