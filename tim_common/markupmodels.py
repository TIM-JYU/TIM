from copy import copy
from dataclasses import dataclass, field, fields, is_dataclass
from datetime import datetime, timezone
from typing import Union, List, Dict, Any, Optional, Mapping, NewType

import marshmallow
from marshmallow import missing, pre_load

from tim_common.utils import Missing


@dataclass
class PointsRule:
    class Meta:
        unknown = 'EXCLUDE'  # Plugins may have custom rules - TIM can ignore them.
    maxPoints: Union[str, int, float, None, Missing] = missing
    allowUserMin: Union[int, float, None, Missing] = missing
    allowUserMax: Union[int, float, None, Missing] = missing
    multiplier: Union[int, float, None, Missing] = missing
    penalties: Union[dict[str, float], None, Missing] = missing


class PluginDateTimeField(marshmallow.fields.Field):

    def _serialize(self, value: Any, attr: str, obj: Any, **kwargs: dict[str, Any]) -> Any:
        raise NotImplementedError

    def _deserialize(self, value: Any, attr: Optional[str],
                     data: Optional[Mapping[str, Any]], **kwargs: dict[str, Any]) -> datetime:
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


PluginDateTime = NewType('PluginDateTime', datetime)
PluginDateTime._marshmallow_field = PluginDateTimeField  # type: ignore


class HiddenFieldsMixin:
    @pre_load
    def process_minus(self, data: Any, **_: dict[str, Any]) -> Any:
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
    buttonNewTask: Union[str, None, Missing] = missing
    cache: Union[bool, None, Missing] = missing
    deadline: Union[PluginDateTime, datetime, None, Missing] = missing
    fields: Union[list[str], None, Missing] = missing
    header: Union[str, None, Missing] = missing
    headerText: Union[str, None, Missing] = missing
    hideBrowser: Union[bool, Missing, None] = missing
    initNewAnswer: Union[str, None, Missing] = missing
    lazy: Union[bool, Missing] = missing
    maxHeight: Union[str, None, Missing] = field(metadata={'data_key': 'max-height'}, default=missing)
    minHeight: Union[str, None, Missing] = field(metadata={'data_key': 'min-height'}, default=missing)
    pointsRule: Union[PointsRule, None, Missing] = missing
    pointsText: Union[str, None, Missing] = missing
    postprogram: Union[str, Missing] = missing
    postoutput: Union[str, Missing] = missing
    showPoints: Union[bool, None, Missing] = missing
    starttime: Union[PluginDateTime, datetime, None, Missing] = missing
    showInView: Union[bool, Missing] = missing
    stem: Union[str, None, Missing] = missing
    triesText: Union[str, None, Missing] = missing
    useCurrentUser: Union[bool, None, Missing] = missing
    texafterprint: Union[str, None, Missing] = missing
    texbeforeprint: Union[str, None, Missing] = missing
    texprint: Union[str, None, Missing] = missing

    def show_points(self) -> bool:
        if isinstance(self.showPoints, bool):
            return self.showPoints
        return True

    def tries_text(self) -> str:
        if isinstance(self.triesText, str):
            return self.triesText
        return 'Tries left:'

    def points_text(self) -> str:
        if isinstance(self.pointsText, str):
            return self.pointsText
        return 'Points:'


def asdict_skip_missing(obj: Any) -> dict[str, Any]:
    result = []
    for f in fields(obj):
        v = getattr(obj, f.name)
        if v is missing:
            continue
        value = asdict_skip_missing(v) if is_dataclass(v) else v
        result.append((f.name, value))
    return dict(result)


def list_not_missing_fields(inst: Any) -> list:
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

    hidden_keys: Union[list[str], Missing] = missing
    """Meta field that keeps track which markup fields were hidden (that is, prefixed with "-").
    Hidden keys are never sent to browser.
    """

    button: Union[str, None, Missing] = missing
    buttonText: Union[str, None, Missing] = missing
    disableUnchanged: Union[bool, Missing, None] = missing
    footer: Union[str, Missing] = missing
    forceBrowser: Union[bool, Missing, None] = missing
    globalField: Union[bool, Missing, None] = missing
    readonly: Union[bool, Missing, None] = missing
    lang: Union[str, None, Missing] = missing
    resetText: Union[str, Missing, None] = missing
    connectionErrorMessage: Union[str, Missing] = missing
    undo: Union[UndoInfo, Missing, None] = missing

    def get_visible_data(self) -> dict:
        assert isinstance(self.hidden_keys, list)
        return {k: v for k, v in list_not_missing_fields(self) if k not in self.hidden_keys and k != 'hidden_keys'}
