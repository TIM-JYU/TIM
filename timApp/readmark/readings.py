from collections import defaultdict
from datetime import timedelta
from typing import List, DefaultDict

from sqlalchemy.orm import Query

from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.readmark.readparagraph import ReadParagraph
from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


def get_read_expiry_condition(delta: timedelta):
    return ((ReadParagraph.type == ReadParagraphType.click_red) |
            (ReadParagraph.timestamp > get_current_time() - delta))


def get_readings(usergroup_id: int, doc: Document, filter_condition=None) -> List[ReadParagraph]:
    return get_readings_filtered_query(usergroup_id, doc, filter_condition).all()


def has_anything_read(usergroup_ids: List[int], doc: Document) -> bool:
    # Custom query for speed
    ids = doc.get_referenced_document_ids()
    ids.add(doc.doc_id)
    query = ReadParagraph.query.filter(ReadParagraph.doc_id.in_(ids)
                                       & (ReadParagraph.usergroup_id.in_(usergroup_ids)
                                          & (ReadParagraph.type == ReadParagraphType.click_red)))
    return db.session.query(query.exists()).scalar()


def get_readings_filtered_query(usergroup_id: int, doc: Document, filter_condition=None) -> Query:
    q = get_readings_query(usergroup_id, doc)
    if filter_condition is not None:
        q = q.filter(filter_condition)
    return q


def get_readings_query(usergroup_id: int, doc: Document) -> Query:
    """Gets the reading info for a document for a user.

    :param doc: The document for which to get the readings.
    :param usergroup_id: The id of the user group whose readings will be fetched.

    """
    ids = doc.get_referenced_document_ids()
    ids.add(doc.doc_id)
    return ReadParagraph.query.filter(
        ReadParagraph.doc_id.in_(ids) &
        (ReadParagraph.usergroup_id == usergroup_id)
    ).order_by(ReadParagraph.timestamp)


def mark_read(usergroup_id: int,
              doc: Document,
              par: DocParagraph,
              read_type=ReadParagraphType.click_red):
    rp = ReadParagraph(usergroup_id=usergroup_id,
                       doc_id=doc.doc_id,
                       par_id=par.get_id(),
                       par_hash=par.get_hash(),
                       type=read_type)
    db.session.add(rp)


def mark_all_read(usergroup_id: int,
                  doc: Document):
    existing = {r.par_id: r for r in get_readings_query(usergroup_id, doc).filter(
        ReadParagraph.type == ReadParagraphType.click_red)}
    for par in doc:
        e = existing.get(par.get_id())
        if e and e.par_hash == par.get_hash():
            continue
        mark_read(usergroup_id, doc, par)


def remove_all_read_marks(doc: Document):
    ids = doc.get_referenced_document_ids()
    ids.add(doc.doc_id)
    all_doc_read = ReadParagraph.query.filter(ReadParagraph.doc_id.in_(ids)
                                              & (ReadParagraph.type == ReadParagraphType.click_red))
    all_doc_read.delete(synchronize_session=False)


def copy_readings(src_par: DocParagraph, dest_par: DocParagraph):
    if src_par.doc.doc_id == dest_par.doc.doc_id and src_par.get_id() == dest_par.get_id():
        return

    src_par_query = ReadParagraph.query.filter_by(doc_id=src_par.doc.doc_id, par_id=src_par.get_id())
    ReadParagraph.query.filter(
        (ReadParagraph.doc_id == dest_par.doc.doc_id) &
        (ReadParagraph.par_id == dest_par.get_id()) &
        ReadParagraph.usergroup_id.in_(src_par_query.with_entities(ReadParagraph.usergroup_id))).delete(
        synchronize_session='fetch')

    for p in src_par_query.all():  # type: ReadParagraph
        db.session.add(ReadParagraph(usergroup_id=p.usergroup_id,
                                     doc_id=dest_par.doc.doc_id,
                                     par_id=dest_par.get_id(),
                                     timestamp=p.timestamp,
                                     par_hash=p.par_hash,
                                     type=p.type
                                     ))


def get_common_readings(usergroup_ids: List[int], doc: Document, filter_condition=None):
    users: List[DefaultDict[str, DefaultDict[ReadParagraphType, ReadParagraph]]] = []
    for u in usergroup_ids:
        reading_map = defaultdict(lambda: defaultdict(lambda: ReadParagraph(par_hash=None)))
        rs = get_readings(u, doc, filter_condition)
        for r in rs:
            reading_map[r.par_id][r.type] = r
        users.append(reading_map)
    common_par_ids = users[0].keys()
    for r in users[1:]:
        common_par_ids &= r.keys()
    # If the hashes are not the same for every user, it means someone has not read the latest one. We remove
    # such paragraphs.
    # TODO: How to handle different types of readings for a group?
    final_pars = [par_id for par_id in common_par_ids if all(
        (read_pars[par_id][ReadParagraphType.click_red].par_hash == users[0][par_id][
            ReadParagraphType.click_red].par_hash) for read_pars in users)]
    for key in final_pars:
        for k, v in users[0][key].items():
            yield v
