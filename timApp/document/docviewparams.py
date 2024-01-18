from dataclasses import dataclass, field

from marshmallow import ValidationError

from tim_common.marshmallow_dataclass import class_schema


@dataclass(frozen=True, eq=True)
class DocCommonParams:
    """Route parameters that affect both view and print"""

    area: str | None = None


@dataclass(frozen=True, eq=True)
class DocPrintParams(DocCommonParams):
    """Print route parameters"""

    textplain: bool | None = None


@dataclass(frozen=True, eq=True)
class DocViewParams(DocCommonParams):
    """View route parameters that affect document rendering."""

    b: int | str | None = None
    e: int | str | None = None
    edit: str | None = None
    group: list[str] | None = field(default=None, metadata={"list_type": "delimited"})
    groups: list[str] | None = field(default=None, metadata={"list_type": "delimited"})
    hide_names: bool | None = None
    lazy: bool | None = None
    noanswers: bool = False
    pars_only: bool = False
    preamble: bool = False
    size: int | None = None
    valid_answers_only: bool | None = None
    as_user: str | None = None
    user: str | None = None

    def __post_init__(self) -> None:
        if self.b and self.e:
            if type(self.b) != type(self.e):
                raise ValidationError("b and e must be of same type (int or string).")
        if self.e is not None and self.size is not None:
            raise ValidationError(
                "Cannot provide e and size parameters at the same time."
            )


ViewModelSchema = class_schema(DocViewParams)()
PrintModelSchema = class_schema(DocPrintParams)()
