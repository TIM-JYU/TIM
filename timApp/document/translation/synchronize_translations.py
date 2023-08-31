from difflib import SequenceMatcher

from timApp.document.document import Document
from timApp.document.docparagraph import create_reference, DocParagraph
from timApp.document.editing.documenteditresult import DocumentEditResult
from timApp.document.docinfo import DocInfo
from timApp.document.documents import (
    find_lang_matching_cite_source,
    add_explicit_area_ids,
)


def update_par_content(
    tr_doc: Document, i2: int, tr_ids: list[str], orig: Document, par_id: str
) -> None:
    """
    Insert paragraph to the Translation according to the original document.
    If the original contains citations, references are followed to the source,
    and translated source paragraph is used for translations if found.
    :param tr_doc: Current Translation that is to be updated (synchronized)
    :param i2: insert index for the new/modified paragraph
    :param tr_ids: list of reference paragraph ids
    :param orig: original document that the current Translation is based on
    :param par_id: paragraph id in the original
    :return: None
    """
    before_i = tr_doc.find_insert_index(i2, tr_ids)

    # Preserve citations if they exist. Follows cite references
    # to source, and uses translated versions according to tr_doc language.
    # TODO: Paragraph citations are now 'corrected' to always reference the corresponding
    #  language version if such translations exist for the source document. Should this
    #  behaviour be controllable to end user via a document setting? Current behaviour
    #  might not be desirable, eg. if a citation should actually be in the original
    #  source language (one _can_ avoid this by creating another document which has no translations).
    ref_par = orig.get_paragraph(par_id)
    rd = ref_par.get_attr("rd", None)
    rp = ref_par.get_attr("rp", None)
    ra = ref_par.get_attr("ra", None)

    matched_doc, rp = find_lang_matching_cite_source(tr_doc, rd, rp, ra)
    rd = matched_doc.id if matched_doc else None

    if ra:
        # Only add the area citation if it doesn't already exist,
        # or replace it with a translated area citation if it was
        # from the original but a corresponding translated one exists.
        # TODO: in order to keep the (translated) citation up-to-date,
        #  we may eventually want to replace the existing area
        #  with the one re-created here
        area_par = None
        for p in tr_doc.get_paragraphs():
            area_par = p if p.get_attr("ra") == ra else None
        if area_par and not area_par.get_attr("rd") == rd:
            # the citation points to the original, delete it
            tr_doc.delete_paragraph(area_par.id)

            tr_par = DocParagraph.create_area_reference(
                tr_doc,
                area_name=ra,
                r="tr",
                rd=matched_doc.id,
            )
        else:
            tr_par = None
    else:
        tr_par = create_reference(
            tr_doc,
            doc_id=rd if rd else orig.doc_id,
            par_id=rp if rp else par_id,
            r="tr",
            add_rd=ref_par.is_citation_par(),
        )
        add_explicit_area_ids(ref_par, tr_par)

    if tr_par:
        if orig.get_paragraph(par_id).is_setting():
            tr_par.set_attr("settings", "")
        tr_doc.insert_paragraph_obj(
            tr_par,
            insert_before_id=tr_ids[before_i] if before_i < len(tr_ids) else None,
        )


def synchronize_translations(doc: DocInfo, edit_result: DocumentEditResult):
    """Synchronizes the translations of a document by adding missing paragraphs to the translations
    and deleting non-existing paragraphs.

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

    for tr in doc.translations:  # type: DocInfo
        if not tr.is_original_translation:
            tr_doc = tr.document_as_current_user
            tr_pars = tr_doc.get_paragraphs()
            tr_rps, tr_ids = [], []
            for tr_rp, tr_id in (
                (p.get_attr("rp"), p.get_id())
                for p in tr_pars
                if p.get_attr("rp") is not None
            ):
                tr_rps.append(tr_rp)
                tr_ids.append(tr_id)
            s = SequenceMatcher(None, tr_rps, orig_ids)
            opcodes = s.get_opcodes()
            for tag, i1, i2, j1, j2 in [
                opcode for opcode in opcodes if opcode[0] in ["delete", "replace"]
            ]:
                for par_id in tr_ids[i1:i2]:
                    tr_doc.delete_paragraph(par_id)
            for tag, i1, i2, j1, j2 in opcodes:
                if tag in ["replace", "insert"]:
                    for par_id in orig_ids[j1:j2]:
                        update_par_content(tr_doc, i2, tr_ids, orig, par_id)
