import json
from copy import deepcopy
from typing import Optional, Dict, Any, Union, List, Set

from timApp.timdb.tim_models import db


class AskedJson(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'askedjson'
    asked_json_id = db.Column(db.Integer, primary_key=True)
    json = db.Column(db.Text, nullable=False)
    hash = db.Column(db.Text, nullable=False)

    asked_questions = db.relationship('AskedQuestion', back_populates='asked_json', lazy='joined')

    def to_json(self):
        q = normalize_question_json(json.loads(self.json))
        return {
            'hash': self.hash,
            'json': q,
        }


def get_asked_json_by_hash(json_hash: str) -> Optional[AskedJson]:
    return AskedJson.query.filter_by(hash=json_hash).first()


FIELD_NAME_MAP = dict(
    answerfieldtype='answerFieldType',
    expl='expl',
    headers='headers',
    matrixtype='matrixType',
    points='points',
    question='questionText',
    questiontext='questionText',
    questiontitle='questionTitle',
    questiontype='questionType',
    rows='rows',
    timelimit='timeLimit',
    title='questionTitle',
    type='questionType',
    xpl='expl',
)
KNOWN_TITLE_KEYS = {'TITLE', 'title', 'questionTitle'}


def normalize_question_json(q: Dict[str, Any], allow_top_level_keys: Set[str] = None):
    """Normalizes the JSON data of a question.

    The question data format has changed a few times over the years. This function normalizes all possible formats
    to a single format that is easier to handle in other code.
    :param allow_top_level_keys: The set of keys to leave intact in top level. Used for qst plugin.
    :param q: The data to normalize.
    :return: The normalized data.
    """
    allow_top_level_keys = allow_top_level_keys or set()
    normalized = {}
    json_data = find_json(q)
    if not json_data:
        raise Exception('Invalid question data')
    process_json(
        json_data,
        normalized,
        skip_keys={'data', 'DATA'},
        allow_keys=allow_top_level_keys if q is json_data else None
    )
    if q is not json_data:
        # process top-level keys
        process_json(q, normalized, allow_keys=allow_top_level_keys)
    data_field = json_data.get('data') or json_data.get('DATA')
    if data_field:
        process_json(data_field, normalized)
    normalize_rows(normalized['rows'])
    return normalized


def process_json(json_data: Dict[str, Any],
                 normalized: Dict[str, Union[str, Dict, List]],
                 skip_keys: Set[str] = None,
                 allow_keys: Set[str] = None):
    skip_keys = skip_keys or set()
    allow_keys = allow_keys or set()
    for k, v in json_data.items():
        if k in skip_keys:
            continue
        kl = k.lower()
        mapped = FIELD_NAME_MAP.get(kl)
        if mapped:
            normalized[mapped] = deepcopy(v)
        elif k in allow_keys:
            normalized[k] = deepcopy(v)


def find_json(q):
    if not q:
        return None
    if not isinstance(q, dict):
        return None
    if KNOWN_TITLE_KEYS & set(q.keys()):
        return q
    return find_json(q.get('json') or q.get('JSON'))


def normalize_rows(rows):
    for r in rows:
        if isinstance(r, str):
            continue
        cap_cols = r.get('COLUMNS')
        if cap_cols:
            r['columns'] = r.pop('COLUMNS')
