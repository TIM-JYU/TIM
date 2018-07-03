# -*- coding: utf-8 -*-

from typing import List, Optional, Generator, Tuple

from werkzeug.exceptions import abort

from timApp.auth.accesshelper import grant_access_to_session_users, reset_request_access_cache, get_doc_or_abort, \
    verify_edit_access
from timApp.item.tag import TagType, Tag
from timApp.timdb.dbaccess import get_timdb
from timApp.document.specialnames import FORCED_TEMPLATE_NAME, TEMPLATE_FOLDER_NAME
from timApp.auth.sessioninfo import get_current_user_object, get_current_user_group
from timApp.tim_app import app
from timApp.bookmark.bookmarks import Bookmarks
from timApp.document.docinfo import DocInfo
from timApp.document.documents import create_citation
from timApp.item.block import copy_default_rights, BlockType
from timApp.document.docentry import DocEntry, get_documents, create_document_and_block
from timApp.folder.folder import Folder
from timApp.document.translation.translation import Translation, add_tr_entry
from timApp.user.userutils import DOC_DEFAULT_RIGHT_NAME, FOLDER_DEFAULT_RIGHT_NAME
from timApp.util.utils import split_location
from timApp.item.validation import validate_item_and_create


def create_item(item_path, item_type_str, item_title, create_function, owner_group_id):
    item_path = item_path.strip('/')

    validate_item_and_create(item_path, item_type_str, owner_group_id)

    item = create_function(item_path, owner_group_id, item_title)
    timdb = get_timdb()
    grant_access_to_session_users(timdb, item.id)
    item_type = BlockType.from_str(item_type_str)
    if item_type == BlockType.Document:
        bms = Bookmarks(get_current_user_object())
        bms.add_bookmark('Last edited',
                         item.title,
                         item.url_relative,
                         move_to_top=True,
                         limit=app.config['LAST_EDITED_BOOKMARK_LIMIT']).save_bookmarks()
    copy_default_rights(item.id, item_type)
    return item


def get_templates_for_folder(folder: Folder) -> List[DocEntry]:
    current_path = folder.path
    templates = []
    u = get_current_user_object()
    while True:
        for t in get_documents(filter_folder=current_path + '/' + TEMPLATE_FOLDER_NAME,
                               search_recursively=False,
                               filter_user=u):
            if t.short_name not in (DOC_DEFAULT_RIGHT_NAME, FOLDER_DEFAULT_RIGHT_NAME):
                templates.append(t)
        if current_path == '':
            break
        current_path, short_name = split_location(current_path)

        # Templates should not be templates of templates themselves. We skip them.
        # TODO Think if this needs a while loop in case of path like templates/templates/templates
        if short_name == TEMPLATE_FOLDER_NAME:
            current_path, short_name = split_location(current_path)
    templates.sort(key=lambda d: d.short_name.lower())
    return templates


def do_create_item(item_path, item_type, item_title, copied_doc: Optional[DocInfo], template_name, use_template=True):
    item = create_item(item_path,
                       item_type,
                       item_title,
                       DocEntry.create if item_type == 'document' else Folder.create,
                       get_current_user_group())

    if isinstance(item, DocInfo):
        if copied_doc:
            for tr, new_tr in copy_document_and_enum_translations(copied_doc, item):
                copy_default_rights(new_tr.id, BlockType.Document)
        elif use_template:
            templates = get_templates_for_folder(item.parent)
            matched_templates = None
            if template_name:
                matched_templates = list(filter(lambda t: t.short_name == template_name, templates))
            if not matched_templates:
                matched_templates = list(filter(lambda t: t.short_name == FORCED_TEMPLATE_NAME, templates))
            if matched_templates:
                template = matched_templates[0]
                item.document.update(template.document.export_markdown(), item.document.export_markdown())
    reset_request_access_cache()
    return item


def copy_document_and_enum_translations(source: DocInfo, target: DocInfo) -> Generator[Tuple[DocInfo, DocInfo], None, None]:
    target.children.extend(source.children)  # required to retain rights to uploaded files

    # Copy tags except course code and subject.
    for tag in source.block.tags:
        if tag.type == TagType.Regular:
            target.block.tags.append(Tag(name=tag.name, type=tag.type, expires=tag.expires))

    target.document.update(source.document.export_markdown(),
                           target.document.export_markdown(),
                           strict_validation=False)
    for tr in source.translations:  # type: Translation
        doc_id = target.id
        new_tr = None
        if not tr.is_original_translation:
            document = create_document_and_block(get_current_user_group())
            doc_id = document.doc_id
            new_tr = add_tr_entry(doc_id, target, tr)
            document.docinfo = new_tr
            document.update(tr.document.export_markdown(), document.export_markdown(), strict_validation=False)
        elif tr.lang_id:
            add_tr_entry(doc_id, target, tr)
        if not tr.is_original_translation:
            yield tr, new_tr


def create_or_copy_item(item_path: str, item_type: str, item_title: str, cite_id: int = None, copy_id: int = None,
                        template_name: str = None, use_template: bool = True):
    if cite_id:
        return create_citation_doc(cite_id, item_path, item_title)

    d = None
    if copy_id:
        d = get_doc_or_abort(copy_id)
        verify_edit_access(d)
        d = d.src_doc
        vr = d.document.validate()
        if vr.issues:
            abort(400, f'The following errors must be fixed before copying:\n{vr}')
    item = do_create_item(item_path, item_type, item_title, d, template_name, use_template)
    return item


def create_citation_doc(doc_id, doc_path, doc_title):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)

    src_doc = d.document

    def factory(path, group, title):
        return create_citation(src_doc, group, path, title)

    item = create_item(doc_path, 'document', doc_title, factory, get_current_user_group())
    return item
