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

    textplain: bool | str | None = None


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
    task: str | None = None
    """In embed mode, show only the paragraph whose plugin has this task id.

    Either a plain task name (``task=t1``; the document is taken from the URL)
    or a full task id (``task=123.t1``). Equivalent to ``b=<par_id>&size=1``
    for the matching paragraph.

    Without ``embed=true``, or if ``b`` or ``e`` is given, the view is not restricted:
    ``task`` then only has its existing client-side meaning, i.e. answer links
    (``answerNumber=...&task=...&user=...``, the "only" link also with ``b`` and ``size``)
    use it to select the answer.
    """
    embed: bool = False
    """Render the document for embedding in an <iframe> on an external page.

    Usually combined with ``task`` to show a single task.
    Implies ``pars_only=true``, hides the task summary and the page margins, and
    adds a ``Content-Security-Policy: frame-ancestors`` header built from the
    ``EMBED_ALLOWED_ORIGINS`` config option.
    """

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
