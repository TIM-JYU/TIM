"""Defines the Documents class."""

from typing import Optional

from timApp.documentmodel.document import Document
from timApp.documentmodel.documentparser import DocumentParser
from timApp.documentmodel.yamlblock import YamlBlock
from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.block import Block
from timApp.timdb.models.docentry import DocEntry, create_document_and_block
from timApp.timdb.models.translation import Translation
from timApp.timdb.tim_models import UserNotes, BlockAccess
from timApp.timdb.models.readparagraph import ReadParagraph


def create_translation(original_doc: Document,
                       owner_group_id: int) -> Document:
    doc = create_document_and_block(owner_group_id)
    add_reference_pars(doc, original_doc, 'tr')
    return doc


def create_citation(original_doc: Document,
                    owner_group_id: int,
                    path: str,
                    title: str) -> DocInfo:
    """Creates a citation document with the specified name. Each paragraph of the citation document references the
    paragraph in the original document.

    :param title: The document title.
    :param original_doc: The original document to be cited.
    :param path: The path of the document to be created.
    :param owner_group_id: The id of the owner group.
    :returns: The newly created document object.

    """

    doc_entry = DocEntry.create(path, owner_group_id, title)
    doc = doc_entry.document

    add_reference_pars(doc, original_doc, 'c')

    settings = {'source_document': original_doc.doc_id}
    orig_pars = original_doc.get_paragraphs()
    if orig_pars and orig_pars[0].is_setting():
        curr_par = doc.get_paragraphs()[0]
        yb = YamlBlock(values=settings)
        curr_par.set_markdown(yb.to_markdown())
        curr_par.save()
    else:
        doc.set_settings(settings)

    return doc_entry


def add_reference_pars(doc: Document, original_doc: Document, r: str):
    for par in original_doc:
        ref_par = par.create_reference(doc, r, add_rd=False)
        if par.is_setting():
            ref_par.set_attr('settings', '')
        doc.add_paragraph_obj(ref_par)


def delete_document(document_id: int):
    """Deletes the specified document.

    :param document_id: The id of the document to be deleted.

    """

    DocEntry.query.filter_by(id=document_id).delete()
    BlockAccess.query.filter_by(block_id=document_id).delete()
    Block.query.filter_by(type_id=blocktypes.DOCUMENT, id=document_id).delete()
    ReadParagraph.query.filter_by(doc_id=document_id).delete()
    UserNotes.query.filter_by(doc_id=document_id).delete()
    Translation.query.filter((Translation.doc_id == document_id) | (Translation.src_docid == document_id)).delete()
    Document.remove(document_id)


def import_document(content: str, path: str, owner_group_id: int, title: Optional[str] = None) -> Document:
    doc = DocEntry.create(path, owner_group_id, title=title).document
    parser = DocumentParser(content)
    for block in parser.get_blocks():
        doc.add_paragraph(text=block['md'], attrs=block.get('attrs'))
    return doc


def import_document_from_file(document_file: str,
                              path: str,
                              owner_group_id: int,
                              title: Optional[str] = None) -> Document:
    """Imports the specified document in the database.

    :param document_file: The file path of the document to import.
    :param path: The path for the document.
    :param owner_group_id: The owner group of the document.
    :returns: The created document object.

    """
    with open(document_file, 'r', encoding='utf-8') as f:
        content = f.read()  # todo: use a stream instead
    return import_document(content, path, owner_group_id, title=title)
