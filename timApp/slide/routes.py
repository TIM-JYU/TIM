import json

from sqlalchemy import select

from timApp.auth.accesshelper import get_doc_or_abort, verify_manage_access
from timApp.slide.slidestatus import SlideStatus
from timApp.timdb.sqa import db
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

slide_bp = TypedBlueprint(
    "slide",
    __name__,
    url_prefix="",
)


@slide_bp.get("/getslidestatus")
def getslidestatus(doc_id: int):
    status: SlideStatus = (
        db.session.execute(select(SlideStatus).filter_by(doc_id=doc_id).limit(1))
        .scalars()
        .first()
    )
    st = status.status if status else None
    return json_response(json.loads(st))


@slide_bp.post("/setslidestatus")
def setslidestatus(
    doc_id: int,
    indexf: int,
    indexh: int,
    indexv: int,
):
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)
    s = SlideStatus(
        doc_id=doc_id,
        status=json.dumps(dict(indexf=indexf, indexh=indexh, indexv=indexv)),
    )
    db.session.merge(s)
    db.session.commit()
    return ok_response()
