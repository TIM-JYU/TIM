"""Defines the Documents class."""

from sqlalchemy import delete

from timApp.auth.auth_models import BlockAccess
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.document import Document
from timApp.document.documentparser import DocumentParser
from timApp.document.translation.translation import Translation
from timApp.document.yamlblock import YamlBlock
from timApp.item.block import Block, BlockType
from timApp.note.usernote import UserNote
from timApp.readmark.readparagraph import ReadParagraph
from timApp.timdb.sqa import run_sql
from timApp.user.usergroup import UserGroup


def apply_citation(new_doc: DocInfo, src_doc: Document):
    """Creates a citation document. Each paragraph of the citation document references the
    paragraph in the original document.

    :param new_doc: The document where the citation paragraphs will be added.
    :param src_doc: The original document to be cited.
    """

    doc = new_doc.document

    add_reference_pars(doc, src_doc, r="c")

    settings = {"source_document": src_doc.doc_id}
    orig_pars = src_doc.get_paragraphs()
    if orig_pars and orig_pars[0].is_setting():
        curr_par = doc.get_paragraphs()[0]
        yb = YamlBlock(values=settings)
        curr_par.set_markdown(yb.to_markdown())
        curr_par.save()
    else:
        doc.set_settings(settings)


def find_lang_matching_cite_source(
    tr_doc: Document, rd: str, rp: str | None = None, ra: str | None = None
) -> tuple[Document | None, str | None]:
    """
    Find document and paragraph id from cited source Translation whose language matches
    the Translation we are currently creating.
    Note some elements of the return value may be None.
    :param rd: source document id
    :param rp: source document paragraph id
    :param ra: source document paragraph area name
    :param tr_doc: Translation that is citing the source document
    :return: the matched source Translation and paragraph id as a tuple.
    """
    matched_doc = None
    par_id = None
    source_docinfo = DocEntry.find_by_id(int(rd)) if rd else None

    if source_docinfo:
        for source_tr in source_docinfo.translations:
            # Documents might be missing a lang_id, or even a DocInfo
            if (
                source_tr.lang_id
                and tr_doc.docinfo
                and source_tr.lang_id == tr_doc.docinfo.lang_id
            ):
                matched_doc = source_tr
                # Find matching paragraph hash for translated citation par
                for p in source_tr.document:
                    if (rp and p.get_attr("rp") == rp) or (
                        ra and p.get_attr("area") == ra
                    ):
                        par_id = p.id
                        break
                break
        if not matched_doc:
            matched_doc = source_docinfo.document
            par_id = rp
    return matched_doc, par_id


def add_reference_pars(
    doc: Document, original_doc: Document, r: str, translator: str | None = None
):
    for par in original_doc:
        # If the paragraph is a citation, it should remain a citation in the translation
        # instead of being converted into a 'regular' translated paragraph.
        # Additionally, we want to check for a translated version of the citation that
        # matches the language of the translation being created and use it if found.
        # If one is not found, the original citation should be used.
        citation_doc_id = par.get_attr("rd")
        citation_par_id = par.get_attr("rp")
        area_citation = par.get_attr("ra")

        if citation_doc_id:
            matched_doc, citation_par_id = find_lang_matching_cite_source(
                doc, citation_doc_id, citation_par_id
            )

            if area_citation and matched_doc:
                ref_par = par.create_area_reference(
                    doc, area_citation, r="tr", rd=matched_doc.doc_id
                )
            else:
                if not matched_doc or not citation_par_id:
                    # cited document or paragraph doesn't exist, so just use the original citation
                    matched_doc = original_doc
                    citation_par_id = par.id

                from timApp.document.docparagraph import create_reference

                ref_par = create_reference(
                    doc=doc,
                    doc_id=matched_doc.doc_id,
                    par_id=citation_par_id,
                    add_rd=True,
                    r=r,
                )
        else:
            ref_par = par.create_reference(doc, translator, r, add_rd=False)

            # For area citations to work correctly in translations,
            # we need to add explicit area/area_end tags to translated
            # area paragraphs
            if r == "tr":
                from timApp.document.docparagraph import add_explicit_area_ids

                add_explicit_area_ids(par, ref_par)
        if par.is_setting():
            ref_par.set_attr("settings", "")
        doc.add_paragraph_obj(ref_par)


def delete_document(document_id: int):
    """Deletes the specified document.

    :param document_id: The id of the document to be deleted.

    """

    for stmt in (
        delete(DocEntry).where(DocEntry.id == document_id),
        delete(BlockAccess).where(BlockAccess.block_id == document_id),
        delete(Block).where(
            (Block.type_id == BlockType.Document.value) & (Block.id == document_id)
        ),
        delete(ReadParagraph).where(ReadParagraph.doc_id == document_id),
        delete(UserNote).where(UserNote.doc_id == document_id),
        delete(Translation).where(
            (Translation.doc_id == document_id) | (Translation.src_docid == document_id)
        ),
    ):
        run_sql(stmt)

    Document.remove(document_id)


def import_document(
    content: str, path: str, owner_group: UserGroup, title: str | None = None
) -> DocInfo:
    d = DocEntry.create(path, owner_group, title=title)
    doc = d.document
    parser = DocumentParser(content)
    for block in parser.get_blocks():
        doc.add_paragraph(text=block["md"], attrs=block.get("attrs"))
    return d


def import_document_from_file(
    document_file: str, path: str, owner_group: UserGroup, title: str | None = None
) -> DocInfo:
    """Imports the specified document in the database.

    :param title: Title for the document.
    :param document_file: The file path of the document to import.
    :param path: The path for the document.
    :param owner_group: The owner group of the document.
    :returns: The created document object.

    """
    with open(document_file, encoding="utf-8") as f:
        content = f.read()  # todo: use a stream instead
    return import_document(content, path, owner_group, title=title)
