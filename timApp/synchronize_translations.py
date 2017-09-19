from difflib import SequenceMatcher

from timApp.documentmodel.docparagraph import create_reference
from timApp.documentmodel.documenteditresult import DocumentEditResult
from timApp.timdb.docinfo import DocInfo


def synchronize_translations(doc: DocInfo, edit_result: DocumentEditResult):
    """Synchronizes the translations of a document by adding missing paragraphs to the translations and deleting non-existing
    paragraphs.

    :param edit_result: The changes that were made to the document.
    :param doc: The document that was edited and whose translations need to be synchronized.
    """

    # we are only interested in the changes in the "master" document
    if not doc.is_original_translation:
        return
    # for now, it only matters if pars were added or deleted
    if not edit_result.pars_added_or_deleted:
        return
    orig = doc.document_as_current_user
    orig_ids = orig.get_par_ids()
    if orig_ids and orig.get_paragraph(orig_ids[0]).is_setting():
        orig_ids = orig_ids[1:]

    for tr in doc.translations:  # type: DocInfo
        if not tr.is_original_translation:
            tr_doc = tr.document_as_current_user
            tr_pars = tr_doc.get_paragraphs()
            tr_rps, tr_ids = [], []
            for tr_rp, tr_id in ((p.get_attr('rp'), p.get_id()) for p in tr_pars if p.get_attr('rp') is not None):
                tr_rps.append(tr_rp)
                tr_ids.append(tr_id)
            s = SequenceMatcher(None, tr_rps, orig_ids)
            opcodes = s.get_opcodes()
            for tag, i1, i2, j1, j2 in [opcode for opcode in opcodes if opcode[0] in ['delete', 'replace']]:
                for par_id in tr_ids[i1:i2]:
                    tr_doc.delete_paragraph(par_id)
            for tag, i1, i2, j1, j2 in opcodes:
                if tag == 'replace':
                    for par_id in orig_ids[j1:j2]:
                        before_i = tr_doc.find_insert_index(i2, tr_ids)
                        tr_par = create_reference(tr_doc, orig.doc_id, par_id, 'tr', add_rd=False)
                        tr_doc.insert_paragraph_obj(tr_par,
                                                    insert_before_id=tr_ids[before_i] if before_i < len(
                                                        tr_ids) else None)
                elif tag == 'insert':
                    for par_id in orig_ids[j1:j2]:
                        before_i = tr_doc.find_insert_index(i2, tr_ids)
                        tr_par = create_reference(tr_doc, orig.doc_id, par_id, 'tr', add_rd=False)
                        tr_doc.insert_paragraph_obj(tr_par,
                                                    insert_before_id=tr_ids[before_i] if before_i < len(
                                                        tr_ids) else None)
