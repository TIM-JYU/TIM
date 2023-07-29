from collections import defaultdict
from datetime import timedelta
from typing import DefaultDict

from sqlalchemy import select, func, delete
from sqlalchemy.sql import Select

from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.readmark.readparagraph import ReadParagraph
from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.timdb.sqa import db, run_sql
from timApp.util.utils import get_current_time


def get_read_expiry_condition(delta: timedelta):
    return (ReadParagraph.type == ReadParagraphType.click_red) | (
        ReadParagraph.timestamp > get_current_time() - delta
    )


def get_readings(
    usergroup_id: int, doc: Document, filter_condition=None
) -> list[ReadParagraph]:
    return (
        run_sql(get_readings_filtered_query(usergroup_id, doc, filter_condition))
        .scalars()
        .all()
    )


def has_anything_read(usergroup_ids: list[int], doc: Document) -> bool:
    # Custom query for speed
    ids = doc.get_referenced_document_ids()
    ids.add(doc.doc_id)
    query = select(ReadParagraph.id).filter(
        ReadParagraph.doc_id.in_(ids)
        & (ReadParagraph.usergroup_id.in_(usergroup_ids))
        & (ReadParagraph.type == ReadParagraphType.click_red)
    )
    # Normal query is generally faster than an "exists" subquery even if it causes extra data to be loaded
    return run_sql(query).scalars().first() is not None


def get_readings_filtered_query(
    usergroup_id: int, doc: Document, filter_condition=None
) -> Select:
    stmt = get_readings_query(usergroup_id, doc)
    if filter_condition is not None:
        stmt = stmt.filter(filter_condition)
    return stmt


def get_clicked_readings_query(doc: Document) -> Select:
    return select(ReadParagraph).filter(
        (ReadParagraph.doc_id == doc.doc_id)
        & (ReadParagraph.type == ReadParagraphType.click_red)
    )


def get_readings_query(usergroup_id: int, doc: Document) -> Select:
    """Gets the reading info for a document for a user.

    :param doc: The document for which to get the readings.
    :param usergroup_id: The id of the user group whose readings will be fetched.

    """
    ids = doc.get_referenced_document_ids()
    ids.add(doc.doc_id)
    return (
        select(ReadParagraph)
        .filter(
            ReadParagraph.doc_id.in_(ids) & (ReadParagraph.usergroup_id == usergroup_id)
        )
        .order_by(ReadParagraph.timestamp)
    )


def mark_read(
    usergroup_id: int,
    doc: Document,
    par: DocParagraph,
    read_type=ReadParagraphType.click_red,
):
    rp = ReadParagraph(
        usergroup_id=usergroup_id,
        doc_id=doc.doc_id,
        par_id=par.get_id(),
        par_hash=par.get_hash(),
        type=read_type,
    )
    db.session.add(rp)


def mark_all_read(usergroup_id: int, doc: Document):
    existing = {
        (r.par_id, r.doc_id): r
        for r in run_sql(
            get_readings_query(usergroup_id, doc).filter(
                ReadParagraph.type == ReadParagraphType.click_red
            )
        ).scalars()
    }
    for par in doc:
        e = existing.get((par.get_id(), doc.doc_id))
        if e and e.par_hash == par.get_hash():
            continue
        mark_read(usergroup_id, doc, par)


def remove_all_read_marks(doc: Document):
    # Documents can reference paragraphs from other documents, which is why
    # usually you'd use get_referenced_document_ids to get all document IDs
    # Since we're deleting read marks here, it's better to be safe and only remove marks only
    # for paragraphs defined directly in the document
    run_sql(
        delete(ReadParagraph)
        .where(
            ReadParagraph.id.in_(
                get_clicked_readings_query(doc).with_only_columns(ReadParagraph.id)
            )
        )
        .execution_options(synchronize_session=False)
    )


def get_read_usergroups_count(doc: Document):
    return db.session.scalar(
        get_clicked_readings_query(doc)
        .distinct(ReadParagraph.usergroup_id)
        .with_only_columns(func.count())
    )


def copy_readings(src_par: DocParagraph, dest_par: DocParagraph):
    if (
        src_par.doc.doc_id == dest_par.doc.doc_id
        and src_par.get_id() == dest_par.get_id()
    ):
        return

    src_par_stmt = select(ReadParagraph).filter_by(
        doc_id=src_par.doc.doc_id, par_id=src_par.get_id()
    )
    run_sql(
        delete(ReadParagraph)
        .where(
            (ReadParagraph.doc_id == dest_par.doc.doc_id)
            & (ReadParagraph.par_id == dest_par.get_id())
            & ReadParagraph.usergroup_id.in_(
                src_par_stmt.with_only_columns(ReadParagraph.usergroup_id)
            )
        )
        .execution_options(synchronize_session="fetch")
    )

    for p in run_sql(src_par_stmt).scalars():  # type: ReadParagraph
        db.session.add(
            ReadParagraph(
                usergroup_id=p.usergroup_id,
                doc_id=dest_par.doc.doc_id,
                par_id=dest_par.get_id(),
                timestamp=p.timestamp,
                par_hash=p.par_hash,
                type=p.type,
            )
        )


def get_common_readings(usergroup_ids: list[int], doc: Document, filter_condition=None):
    users: list[DefaultDict[str, DefaultDict[ReadParagraphType, ReadParagraph]]] = []
    for u in usergroup_ids:
        reading_map = defaultdict(
            lambda: defaultdict(lambda: ReadParagraph(par_hash=None))
        )
        rs = get_readings(u, doc, filter_condition)
        for r in rs:
            reading_map[r.doc_id, r.par_id][r.type] = r
        users.append(reading_map)
    common_par_ids = users[0].keys()
    for r in users[1:]:
        common_par_ids &= r.keys()
    # If the hashes are not the same for every user, it means someone has not read the latest one. We remove
    # such paragraphs.
    # TODO: How to handle different types of readings for a group?
    final_pars = [
        k
        for k in common_par_ids
        if all(
            (
                read_pars[k][ReadParagraphType.click_red].par_hash
                == users[0][k][ReadParagraphType.click_red].par_hash
            )
            for read_pars in users
        )
    ]
    for key in final_pars:
        for k, v in users[0][key].items():
            yield v
