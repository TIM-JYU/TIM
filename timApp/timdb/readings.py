from collections import defaultdict
from datetime import datetime, timezone, timedelta
from typing import List, DefaultDict

from sqlalchemy.orm import Query

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import Document
from timApp.timdb.readparagraphtype import ReadParagraphType
from timApp.timdb.tim_models import ReadParagraph, db


Rp = db.aliased(ReadParagraph)


# workaround for suppressing warning about None comparison
N = None


def get_read_expiry_condition(delta: timedelta):
    return ((ReadParagraph.type == ReadParagraphType.click_red) |
           (ReadParagraph.timestamp > datetime.now(tz=timezone.utc) - delta))


def get_readings(usergroup_id: int, doc: Document, filter_condition=None) -> List[ReadParagraph]:
    q = get_readings_query(usergroup_id, doc)
    if filter_condition is not None:
        q = q.filter(filter_condition)
    return q.all()


def get_readings_query(usergroup_id: int, doc: Document) -> Query:
    """Gets the reading info for a document for a user.

    :param doc: The document for which to get the readings.
    :param usergroup_id: The id of the user group whose readings will be fetched.

    """
    ids = doc.get_referenced_document_ids()
    ids.add(doc.doc_id)
    return ReadParagraph.query.outerjoin(Rp,
                                         (ReadParagraph.par_id == Rp.par_id) &
                                         (ReadParagraph.type == Rp.type) &
                                         (ReadParagraph.usergroup_id == Rp.usergroup_id) &
                                         (ReadParagraph.timestamp < Rp.timestamp)
                                         ).filter(
        (Rp.id == N) &
        ReadParagraph.doc_id.in_(ids) &
        (ReadParagraph.usergroup_id == usergroup_id)
    ).with_entities(ReadParagraph)


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
    users: List[DefaultDict[str, ReadParagraph]] = []
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
        (read_pars[par_id][ReadParagraphType.click_red].par_hash == users[0][par_id][ReadParagraphType.click_red].par_hash) for read_pars in users)]
    for key in final_pars:
        for k, v in users[0][key].items():
            yield v
