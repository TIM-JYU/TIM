import json
from copy import deepcopy
from typing import Any, TYPE_CHECKING

from sqlalchemy import select
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import DbModel

if TYPE_CHECKING:
    from timApp.lecture.askedquestion import AskedQuestion


class AskedJson(DbModel):
    asked_json_id: Mapped[int] = mapped_column(primary_key=True)
    json: Mapped[str]
    hash: Mapped[str]

    asked_questions: Mapped["AskedQuestion"] = relationship(
        back_populates="asked_json", lazy="selectin"
    )

    def to_json(self, hide_points=False):
        q = normalize_question_json(json.loads(self.json))
        if hide_points:
            q.pop("points", None)
            q.pop("defaultPoints", None)
        return {
            "hash": self.hash,
            "json": q,
        }


def get_asked_json_by_hash(json_hash: str) -> AskedJson | None:
    return (
        db.session.execute(select(AskedJson).filter_by(hash=json_hash))
        .scalars()
        .first()
    )


# NOTE: Do NOT add more fields here for new qst attributes. These are ONLY for backward compatibility.
FIELD_NAME_MAP = dict(
    answerfieldtype="answerFieldType",
    expl="expl",
    headers="headers",
    matrixtype="matrixType",
    points="points",
    question="questionText",
    questiontext="questionText",
    questiontitle="questionTitle",
    questiontype="questionType",
    rows="rows",
    timelimit="timeLimit",
    title="questionTitle",
    type="questionType",
    xpl="expl",
)
KNOWN_TITLE_KEYS = {"TITLE", "title", "questionTitle"}
MANDATORY_FIELDS = {
    "answerFieldType",
    "headers",
    "questionTitle",
    "questionText",
    "questionType",
    "rows",
}
CONDITIONALLY_MANDATORY_FIELDS = {"matrix": "matrixType"}


def normalize_question_json(q: dict[str, Any]):
    """Normalizes the JSON data of a question.

    The question data format has changed a few times over the years. This function normalizes all possible formats
    to a single format that is easier to handle in other code.

    :param q: The data to normalize.
    :return: The normalized data.
    """
    normalized = {}
    json_data = find_json(q)
    if not json_data:
        return make_error_question("Missing field: questionTitle")
    process_json(
        json_data,
        normalized,
        skip_keys={"data", "DATA"},
    )
    if q is not json_data:
        # process top-level keys
        process_json(q, normalized)
    normalized.pop("json", None)
    data_field = json_data.get("data") or json_data.get("DATA")
    if data_field:
        process_json(data_field, normalized)
    missing_keys = MANDATORY_FIELDS - set(normalized.keys())
    if missing_keys:
        return make_error_question(
            f'Missing fields: {", ".join(sorted(list(missing_keys)))}'
        )
    result, err = normalize_rows(normalized["rows"])
    if not result:
        return make_error_question(err)
    if normalized.get("matrixType") == "":
        normalized.pop("matrixType")
    for qt, v in CONDITIONALLY_MANDATORY_FIELDS.items():
        if normalized["questionType"] == qt:
            if normalized.get(v) is None:
                return make_error_question(f"Missing {v} when questionType is {qt}")
    return normalized


def make_error_question(desc: str):
    return {
        "answerFieldType": "text",
        "expl": {},
        "headers": [""],
        "matrixType": "textArea",
        "questionText": f"Invalid question data: {desc}",
        "questionTitle": f"Invalid question data: {desc}",
        "questionType": "matrix",
        "rows": [""],
        "isTask": True,  # for better preview
        "invalid": True,
    }


def process_json(
    json_data: dict[str, Any],
    normalized: dict[str, str | dict | list],
    skip_keys: set[str] = None,
):
    skip_keys = skip_keys or set()
    for k, v in json_data.items():
        if k in skip_keys:
            continue
        kl = k.lower()
        mapped = FIELD_NAME_MAP.get(kl)
        if mapped:
            normalized[mapped] = deepcopy(v)
        else:
            normalized[k] = deepcopy(v)


def find_json(q):
    if not q:
        return None
    if not isinstance(q, dict):
        return None
    if KNOWN_TITLE_KEYS & set(q.keys()):
        return q
    return find_json(q.get("json") or q.get("JSON"))


def normalize_rows(rows):
    if not rows:
        return [], ""
    for r in rows:
        if isinstance(r, str):
            continue
        if not isinstance(r, dict):
            return False, "A row must be a dictionary or a string"
        cap_cols = r.get("COLUMNS")
        if cap_cols:
            r["columns"] = r.pop("COLUMNS")
    return True, ""
