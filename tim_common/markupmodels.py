from copy import copy
from dataclasses import dataclass, field, fields, is_dataclass
from datetime import datetime, timezone
from typing import Any, Mapping, NewType, Literal

import dateutil.parser
import marshmallow
from marshmallow import missing, pre_load, EXCLUDE

from tim_common.utils import Missing


@dataclass
class PointsRule:
    class Meta:
        unknown = EXCLUDE  # Plugins may have custom rules - TIM can ignore them.

    maxPoints: str | int | float | None | Missing = missing
    allowUserMin: int | float | None | Missing = missing
    allowUserMax: int | float | None | Missing = missing
    multiplier: int | float | None | Missing = missing
    penalties: dict[str, float] | None | Missing = missing


class PluginDateTimeField(marshmallow.fields.Field):
    def _serialize(
        self, value: Any, attr: str | None, obj: Any, **kwargs: dict[str, Any]
    ) -> Any:
        raise NotImplementedError

    def _deserialize(
        self,
        value: Any,
        attr: str | None,
        data: Mapping[str, Any] | None,
        **kwargs: dict[str, Any],
    ) -> datetime:
        d = None
        if isinstance(value, datetime):
            d = value
        elif isinstance(value, str):
            try:
                d = dateutil.parser.isoparse(value)
            except ValueError:
                # TODO: Remove once all dates use proper ISO format
                try:
                    d = datetime.strptime(value, "%Y-%m-%d %H:%M:%S")
                except ValueError:
                    raise self.make_error("validator_failed")
        if d:
            if d.tzinfo is None:
                d = d.replace(tzinfo=timezone.utc)
            return d
        raise self.make_error("validator_failed")


PluginDateTime = NewType("PluginDateTime", datetime)
PluginDateTime._marshmallow_field = PluginDateTimeField  # type: ignore


class HiddenFieldsMixin:
    """
    Helper mixin to handle hidden fields in the plugin markup.

    Some fields can be forced to be hidden from the browser by prefixing them with "-".
    This mixin converts hidden fields to normal fields so that they can be parsed by marshmallow.
    """

    @pre_load
    def process_minus(self, data: Any, **_: dict[str, Any]) -> Any:
        if isinstance(data, dict):
            data = copy(data)  # Don't modify the original.
            hidden_keys = {
                k[1:] for k in data.keys() if isinstance(k, str) and k.startswith("-")
            }
            for k in hidden_keys:
                data[k] = data.pop(f"-{k}")
            data["hidden_keys"] = hidden_keys
        return data


@dataclass
class AccessField:
    field: str
    limit: int
    error: str | None | Missing = missing


@dataclass
class ModelAnswerInfo:
    answer: str | None | Missing = missing
    count: int | None | Missing = 1
    disabled: bool | Literal["unless_review"] | None | Missing = missing
    endDate: PluginDateTime | datetime | None | Missing = missing
    groups: list[str] | None | Missing = missing
    hideText: str | None | Missing = missing
    linkText: str | None | Missing = missing
    linkTextCount: int | None | Missing = missing
    linkTextBeforeCount: str | None | Missing = missing
    lock: bool = True
    lockedAnswerMessage: str | None | Missing = missing
    lockConfirmation: str | None | Missing = missing
    lockedLinkText: str | None | Missing = missing
    minPoints: float | None | Missing = missing
    hidePoints: bool | None | Missing = missing
    revealDate: PluginDateTime | datetime | None | Missing = missing


@dataclass
class PreviousTaskInfo:
    taskid: str
    requireLock: bool | None | Missing = False
    count: int | None | Missing = missing
    hide: bool | None | Missing = missing
    hideText: str | None | Missing = missing
    unlockText: str | None | Missing = missing
    unlockError: str | None | Missing = missing


@dataclass
class KnownMarkupFields(HiddenFieldsMixin):
    """Represents the plugin markup fields that are known and used by TIM."""

    accessDuration: int | None | Missing = missing
    accessEndText: str | None | Missing = missing
    accessField: AccessField | None | Missing = missing
    anonymous: bool | None | Missing = missing
    answerLimit: int | None | Missing = missing
    automd: bool | None | Missing = missing
    buttonNewTask: str | None | Missing = missing
    cache: bool | None | Missing = missing
    deadline: PluginDateTime | datetime | None | Missing = missing
    fields: list[str] | None | Missing = missing
    floatHeader: str | None | Missing = missing
    floatSize: tuple[int, int] | None | Missing = missing
    header: str | None | Missing = missing
    headerText: str | None | Missing = missing
    hideBrowser: bool | Missing | None = missing
    initNewAnswer: str | None | Missing = missing
    lazy: bool | Missing = missing
    maxHeight: str | None | Missing = field(
        metadata={"data_key": "max-height"}, default=missing
    )
    minHeight: str | None | Missing = field(
        metadata={"data_key": "min-height"}, default=missing
    )
    modelAnswer: ModelAnswerInfo | None | Missing = missing
    pointsRule: PointsRule | None | Missing = missing
    pointsText: str | None | Missing = missing
    postprogram: str | Missing = missing
    postoutput: str | Missing = missing
    previousTask: PreviousTaskInfo | None | Missing = missing
    saveTeacher: bool | None | Missing = missing
    showPoints: bool | None | Missing = missing
    starttime: PluginDateTime | datetime | None | Missing = missing
    showInView: bool | Missing = missing
    stem: str | None | Missing = missing
    triesText: str | None | Missing = missing
    useCurrentUser: bool | None | Missing = missing
    texafterprint: str | None | Missing = missing
    texbeforeprint: str | None | Missing = missing
    texprint: str | None | Missing = missing
    readonly: bool | Missing | None = missing
    saveSingleAnswer: bool | Missing | None = missing
    eagerlyLoadState: bool | Missing | None = missing

    def show_points(self) -> bool:
        if isinstance(self.showPoints, bool):
            return self.showPoints
        return True

    def tries_text(self) -> str | None:
        if isinstance(self.triesText, str):
            return self.triesText
        return None

    def points_text(self) -> str | None:
        if isinstance(self.pointsText, str):
            return self.pointsText
        return None


def asdict_skip_missing(obj: Any) -> dict[str, Any]:
    result = []
    for f in fields(obj):
        v = getattr(obj, f.name)
        if v is missing:
            continue
        if f.metadata.get("missing", False):
            continue
        value = asdict_skip_missing(v) if is_dataclass(v) else v
        result.append((f.name, value))
    return dict(result)


def list_not_missing_fields(inst: Any) -> list:
    return list(((k, v) for k, v in asdict_skip_missing(inst).items()))


@dataclass
class UndoInfo:
    button: str | None | Missing = missing
    title: str | None | Missing = missing
    confirmation: str | None | Missing = missing
    confirmationTitle: str | None | Missing = missing


@dataclass
class AnswerBrowserInfo:
    pointsStep: float | None | Missing = missing
    validOnlyText: str | None | Missing = missing
    showValidOnly: bool | None | Missing = missing
    showReview: bool | None | Missing = missing
    showInitialAskNew: bool | None | Missing = missing
    autosave: bool | None | Missing = missing
    limitPoints: bool | None | Missing = missing


@dataclass
class GenericMarkupModel(KnownMarkupFields):
    """Specifies which fields the editor can use in the plugin markup.
    This base class defines some fields that are applicable to all plugins.

    The difference to KnownMarkupFields is that this class should define fields that are not used by TIM.

    TODO: Some fields here should be moved to KnownMarkupFields.
    """

    hidden_keys: list[str] | Missing = missing
    """Meta field that keeps track which markup fields were hidden (that is, prefixed with "-").
    Hidden keys are never sent to browser.
    """

    button: str | None | Missing = missing
    buttonText: str | None | Missing = missing
    allowUnsavedLeave: bool | Missing | None = missing
    disableUnchanged: bool | Missing | None = missing
    footer: str | Missing = missing
    forceBrowser: bool | Missing | None = missing
    globalField: bool | Missing | None = missing
    lang: str | None | Missing = missing
    resetText: str | Missing | None = missing
    connectionErrorMessage: str | Missing = missing
    undo: UndoInfo | Missing | None = missing
    answerBrowser: AnswerBrowserInfo | Missing | None = missing
    spellcheck: bool | None | Missing = missing
    warningFilter: str | Missing | None = missing

    def get_visible_data(self) -> dict:
        assert isinstance(self.hidden_keys, list)
        return {
            k: v
            for k, v in list_not_missing_fields(self)
            if k not in self.hidden_keys and k != "hidden_keys"
        }
