from dataclasses import dataclass, field
from typing import Any

import requests
from marshmallow import missing, validates_schema, ValidationError

from timApp.answer.answer import AnswerData
from timApp.plugin.containerLink import get_plugin
from timApp.util.get_fields import MembershipFilter
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.utils import Missing


@dataclass
class JsRunnerParams:
    code: str
    data: Any
    error_text: str = ""
    caller: str = ""


class JsRunnerError(Exception):
    pass


def jsrunner_run(params: JsRunnerParams) -> tuple[Any, str]:
    """
    Run JavaScript code in jsrunner.
    """
    runurl = get_plugin("jsrunner").host + "runScript/"
    r = requests.request(
        "post", runurl, json={"code": params.code, "data": params.data}
    )
    result = r.json()
    error = result.get("error")
    if error:
        if params.error_text and error.find("Script failed to return") >= 0:
            error += "\n" + params.error_text
        if params.caller:
            error = params.caller + "\n" + error
        raise JsRunnerError(error)
    data = result.get("result", [])
    output = result.get("output", "")
    return data, output


@dataclass
class JsRunnerMarkupModel(GenericMarkupModel):
    fields: (
        list[str] | Missing
    ) = missing  # This is actually required, but we cannot use non-default arguments here...
    autoadd: bool | Missing = missing
    autoUpdateTables: bool | Missing = True
    creditField: str | Missing = missing
    defaultPoints: float | Missing = missing
    failGrade: str | Missing = missing
    fieldhelper: bool | Missing = missing
    gradeField: str | Missing = missing
    peerReviewField: str | Missing = missing
    gradingScale: dict[Any, Any] | Missing = missing
    group: str | Missing = missing
    groups: list[str] | Missing = missing
    includeUsers: MembershipFilter | Missing = field(
        default=MembershipFilter.Current, metadata={"by_value": True}
    )
    selectIncludeUsers: bool = False
    paramFields: list[str] | Missing = missing
    postprogram: str | Missing = missing
    preprogram: str | Missing = missing
    program: str | Missing = missing
    overrideGrade: bool = False
    showInView: bool = False
    canOverwritePoints: bool = False
    canOverwriteValidity: bool = False
    confirmText: str | Missing = missing
    timeout: int | Missing = missing
    updateFields: list[str] | Missing = missing
    nextRunner: str | Missing = missing
    timeZoneDiff: int | Missing = missing
    peerReview: bool | Missing = missing
    destCourse: str | Missing = missing
    destCourseName: str | Missing = missing
    destCourseGroup: str | Missing = missing

    @validates_schema(skip_on_field_errors=True)
    def validate_schema(self, data: dict, **_: dict) -> None:
        if data.get("fields") is None:
            raise ValidationError(
                "Missing data for required field.", field_name="fields"
            )
        if data.get("group") is None and data.get("groups") is None:
            raise ValidationError("Either group or groups must be given.")


JsRunnerMarkupSchema = class_schema(JsRunnerMarkupModel)


@dataclass
class JsRunnerInputModel:
    nosave: bool | Missing = missing
    userNames: list[str] | Missing = missing
    paramComps: dict[str, str] | Missing = missing
    includeUsers: MembershipFilter | Missing = field(
        default=missing, metadata={"by_value": True}
    )


@dataclass
class RefFrom:
    docId: int
    par: str


@dataclass
class JsRunnerAnswerModel:
    input: JsRunnerInputModel
    ref_from: RefFrom | None = None
    abData: AnswerData | Missing = missing


JsRunnerAnswerSchema = class_schema(JsRunnerAnswerModel)
