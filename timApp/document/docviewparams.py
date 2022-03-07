from dataclasses import dataclass

from marshmallow import ValidationError

from tim_common.marshmallow_dataclass import class_schema


@dataclass(frozen=True, eq=True)
class DocViewParams:
    """View route parameters that affect document rendering."""

    b: int | str | None = None
    e: int | str | None = None
    edit: str | None = None
    group: str | None = None
    hide_names: bool | None = None
    lazy: bool | None = None
    noanswers: bool = False
    pars_only: bool = False
    preamble: bool = False
    size: int | None = None

    def __post_init__(self) -> None:
        if self.b and self.e:
            if type(self.b) != type(self.e):
                raise ValidationError("b and e must be of same type (int or string).")
        if self.e is not None and self.size is not None:
            raise ValidationError(
                "Cannot provide e and size parameters at the same time."
            )


ViewModelSchema = class_schema(DocViewParams)()
