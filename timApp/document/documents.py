"""Defines the Documents class."""

from typing import Optional

from timApp.document.document import Document
from timApp.document.documentparser import DocumentParser
from timApp.document.yamlblock import YamlBlock
from timApp.document.docinfo import DocInfo
from timApp.item.block import Block, BlockType
from timApp.document.docentry import DocEntry, create_document_and_block
from timApp.document.translation.translation import Translation
from timApp.auth.auth_models import BlockAccess
from timApp.note.usernote import UserNote
from timApp.readmark.readparagraph import ReadParagraph
from timApp.user.usergroup import UserGroup


def create_translation(original_doc: Document,
                       owner_group) -> Document:
    doc = create_document_and_block(owner_group)
    add_reference_pars(doc, original_doc, 'tr')
    return doc


def apply_citation(new_doc: DocInfo, src_doc: Document):
    """Creates a citation document. Each paragraph of the citation document references the
    paragraph in the original document.

    :param new_doc: The document where the citation paragraphs will be added.
    :param src_doc: The original document to be cited.
    """

    doc = new_doc.document

    add_reference_pars(doc, src_doc, 'c')

    settings = {'source_document': src_doc.doc_id}
    orig_pars = src_doc.get_paragraphs()
    if orig_pars and orig_pars[0].is_setting():
        curr_par = doc.get_paragraphs()[0]
        yb = YamlBlock(values=settings)
        curr_par.set_markdown(yb.to_markdown())
        curr_par.save()
    else:
        doc.set_settings(settings)


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
    Block.query.filter_by(type_id=BlockType.Document.value, id=document_id).delete()
    ReadParagraph.query.filter_by(doc_id=document_id).delete()
    UserNote.query.filter_by(doc_id=document_id).delete()
    Translation.query.filter((Translation.doc_id == document_id) | (Translation.src_docid == document_id)).delete()
    Document.remove(document_id)


def import_document(content: str, path: str, owner_group: UserGroup, title: Optional[str] = None) -> Document:
    doc = DocEntry.create(path, owner_group, title=title).document
    parser = DocumentParser(content)
    for block in parser.get_blocks():
        doc.add_paragraph(text=block['md'], attrs=block.get('attrs'))
    return doc


def import_document_from_file(document_file: str,
                              path: str,
                              owner_group: UserGroup,
                              title: Optional[str] = None) -> Document:
    """Imports the specified document in the database.

    :param title: Title for the document.
    :param document_file: The file path of the document to import.
    :param path: The path for the document.
    :param owner_group: The owner group of the document.
    :returns: The created document object.

    """
    with open(document_file, 'r', encoding='utf-8') as f:
        content = f.read()  # todo: use a stream instead
    return import_document(content, path, owner_group, title=title)
