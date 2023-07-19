from dataclasses import dataclass

from flask import Blueprint, request
from flask import current_app, Response
from sqlalchemy import func, distinct, true, select
from sqlalchemy.exc import IntegrityError

from timApp.auth.accesshelper import (
    verify_read_marking_right,
    get_doc_or_abort,
    verify_teacher_access,
    verify_manage_access,
)
from timApp.auth.sessioninfo import get_session_usergroup_ids, get_session_users_objs
from timApp.document.caching import clear_doc_cache
from timApp.document.docentry import DocEntry
from timApp.document.hide_names import hide_names_in_teacher
from timApp.readmark.readings import (
    mark_read,
    mark_all_read,
    get_common_readings,
    remove_all_read_marks,
    get_read_usergroups_count,
)
from timApp.readmark.readparagraph import ReadParagraph
from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.sisu.sisu import IncorrectSettings
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import (
    get_option,
    get_consent_opt,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import json_response, ok_response, csv_response
from timApp.util.utils import seq_to_str, split_by_semicolon
from tim_common.marshmallow_dataclass import class_schema

readings = Blueprint("readings", __name__, url_prefix="")


@readings.get("/read/<int:doc_id>")
def get_read_paragraphs(doc_id):
    return get_readings_response(true(), doc_id)


@readings.get("/read/<int:doc_id>/<block_id>")
def get_read_paragraph(doc_id, block_id):
    cond = ReadParagraph.par_id.in_([block_id]) & (
        ReadParagraph.type == ReadParagraphType.click_red
    )
    return get_readings_response(cond, doc_id)


@readings.get("/read/<int:doc_id>/groupCount")
def get_read_groups_count(doc_id: int):
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)
    res = get_read_usergroups_count(d.document)
    return json_response(res)


def get_readings_response(cond, doc_id):
    d = get_doc_or_abort(doc_id)
    verify_read_marking_right(d)
    doc = d.document
    result = get_common_readings(
        get_session_usergroup_ids(),
        doc,
        filter_condition=cond,
    )
    return json_response(list(result))


@readings.put("/unread/<int:doc_id>/<par_id>")
def unread_paragraph(doc_id, par_id):
    return set_read_paragraph(doc_id, par_id, unread=True)


@dataclass
class ReadModel:
    pars: list[tuple[int, str]] | None = None


ReadModelSchema = class_schema(ReadModel)


@readings.put("/read/<int:doc_id>/<par_id>/<int:read_type>")
def set_read_paragraph(doc_id, par_id, read_type=None, unread=False):
    paragraph_type = (
        ReadParagraphType(read_type)
        if read_type is not None
        else ReadParagraphType.click_red
    )
    if current_app.config["DISABLE_AUTOMATIC_READINGS"] and paragraph_type in (
        ReadParagraphType.on_screen,
        ReadParagraphType.hover_par,
    ):
        return ok_response()
    d = get_doc_or_abort(doc_id)
    verify_read_marking_right(d)
    doc = d.document
    pardata: ReadModel = ReadModelSchema().load(request.get_json())
    parlist = pardata.pars or [[doc_id, par_id]]
    doc_map = {}
    for doc_id, par_id in parlist:
        if doc_id not in doc_map:
            d = get_doc_or_abort(doc_id)
            verify_read_marking_right(d)
            doc_map[doc_id] = d

    pars = []
    for doc_id, par_id in parlist:
        d = doc_map[doc_id].document
        try:
            pars.append(d.get_paragraph(par_id))
        except TimDbException:
            # For performance, try to load preamble only in case the paragraph is not found otherwise.
            d.insert_preamble_pars()
            try:
                pars.append(d.get_paragraph(par_id))
            except TimDbException:
                raise RouteException("Non-existent paragraph")

    docs = {}
    for group_id in get_session_usergroup_ids():
        for p in pars:
            if unread:
                rp = (
                    db.session.execute(
                        select(ReadParagraph)
                        .filter_by(
                            usergroup_id=group_id,
                            doc_id=p.get_doc_id(),
                            par_id=p.get_id(),
                            type=paragraph_type,
                        )
                        .order_by(ReadParagraph.timestamp.desc())
                        .limit(1)
                    )
                    .scalars()
                    .first()
                )
                if not rp:
                    raise RouteException("Reading not found")
                db.session.delete(rp)
            else:
                mark_read(group_id, p.doc, p, paragraph_type)
            docs[p.doc.id] = p.doc
    try:
        db.session.commit()
    except IntegrityError:
        raise RouteException("Paragraph was already marked read")
    for d in docs.values():
        for u in get_session_users_objs():
            clear_doc_cache(d, u)
    if unread:
        latest_readings = get_common_readings(
            get_session_usergroup_ids(),
            doc,
            ReadParagraph.par_id.in_([par_id]) & (ReadParagraph.type == paragraph_type),
        )
        for r in latest_readings:
            # We only need the first.
            return json_response({"status": "ok", "latest": r})
    return ok_response()


@readings.put("/read/<int:doc_id>")
def mark_document_read(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_read_marking_right(d)
    doc = d.document
    for group_id in get_session_usergroup_ids():
        mark_all_read(group_id, doc)
    db.session.commit()
    return ok_response()


@readings.post("/markAllUnread/<int:doc_id>")
def mark_all_unread(doc_id: int):
    d = get_doc_or_abort(doc_id)
    verify_manage_access(
        d,
        message="You need to have manage permission to mark document unread for everyone",
    )
    doc = d.document
    settings = doc.get_settings()
    if not settings.exam_mode():
        raise IncorrectSettings(
            "The document must have 'exam_mode' setting defined to remove read marks!"
        )
    remove_all_read_marks(doc)
    db.session.commit()
    return ok_response()


@readings.get("/read/stats/<path:doc_path>")
def get_statistics(doc_path):
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)
    if not d:
        raise NotExist()
    verify_teacher_access(d)
    sort_opt = get_option(request, "sort", "username")
    group_opt = get_option(request, "groups", None)
    block_opt = get_option(request, "blocks", None)
    result_format = get_option(request, "format", "json")
    csv_dialect = get_option(request, "csv", "excel-tab")
    consent = get_consent_opt()
    extra_condition = true()
    if group_opt:
        group_names = split_by_semicolon(group_opt)
        extra_condition = extra_condition & UserGroup.name.in_(
            select(User.name)
            .join(UserGroup, User.groups)
            .filter(UserGroup.name.in_(group_names))
        )
    if consent:
        extra_condition = extra_condition & UserGroup.id.in_(
            select(UserGroup.id)
            .select_from(User)
            .join(UserGroup, User.groups)
            .filter(User.consent == consent)
        )
    if block_opt:
        block_ids = split_by_semicolon(block_opt)
        extra_condition = extra_condition & ReadParagraph.par_id.in_(block_ids)
    automatic_types = [
        ReadParagraphType.click_par,
        ReadParagraphType.hover_par,
        ReadParagraphType.on_screen,
    ]
    cols = [
        func.count(distinct(ReadParagraph.par_id)).filter(ReadParagraph.type == t)
        for t in (ReadParagraphType.click_red, *automatic_types)
    ]
    cols.append(
        func.count(distinct(ReadParagraph.par_id)).filter(
            ReadParagraph.type.in_(automatic_types)
        )
    )
    column_names = (
        "username",
        "click_red",
        "click_par",
        "hover_par",
        "on_screen",
        "any_of_phs",
    )
    sort_col_map = dict(zip(column_names, [UserGroup.name] + cols))
    col_to_sort = sort_col_map.get(sort_opt)
    if col_to_sort is None:
        raise RouteException(
            f"Invalid sort option. Possible values are {seq_to_str(column_names)}."
        )
    stmt = (
        select(UserGroup)
        .join(ReadParagraph)
        .filter_by(doc_id=d.id)
        .filter(extra_condition)
        .group_by(UserGroup)
        .order_by(col_to_sort)
        .with_only_columns(UserGroup.name, *cols)
    )

    def maybe_hide_name_from_row(row):
        if hide_names_in_teacher(d):
            _, rest = row[0], row[1:]
            return "user", *rest
        return row

    def row_to_dict(row):
        return dict(zip(column_names, maybe_hide_name_from_row(row)))

    if result_format == "count":
        reads = list(map(row_to_dict, db.session.execute(stmt).all()))

        return Response(str(len(reads)), mimetype="text/plain")

    if result_format == "userid":
        reads = list(map(row_to_dict, db.session.execute(stmt).all()))

        result = ""
        for r in reads:
            u = r.get("username")
            result += "- " + u + "\n"

        return Response(str(result), mimetype="text/plain")

    if result_format == "csv":

        def gen_rows():
            yield column_names
            yield from (maybe_hide_name_from_row(row) for row in db.session.execute(stmt))

        return csv_response(gen_rows(), dialect=csv_dialect)
    else:
        return json_response(list(map(row_to_dict, db.session.execute(stmt).all())))
