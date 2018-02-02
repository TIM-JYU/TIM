# -*- coding: utf-8 -*-

from typing import List, Optional, Generator, Tuple

from timApp.accesshelper import get_viewable_blocks_or_none_if_admin
from timApp.accesshelper import grant_access_to_session_users, reset_request_access_cache
from timApp.dbaccess import get_timdb
from timApp.documentmodel.specialnames import FORCED_TEMPLATE_NAME, TEMPLATE_FOLDER_NAME
from timApp.sessioninfo import get_current_user_object, get_current_user_group
from timApp.tim_app import app
from timApp.timdb.blocktypes import from_str, blocktypes
from timApp.timdb.bookmarks import Bookmarks
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.item import Item
from timApp.timdb.models.block import copy_default_rights
from timApp.timdb.models.docentry import DocEntry, get_documents, create_document_and_block
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.translation import Translation
from timApp.timdb.tim_models import db
from timApp.timdb.userutils import DOC_DEFAULT_RIGHT_NAME, FOLDER_DEFAULT_RIGHT_NAME
from timApp.utils import split_location
from timApp.validation import validate_item_and_create


def create_item(item_path, item_type_str, item_title, create_function, owner_group_id):
    item_path = item_path.strip('/')

    validate_item_and_create(item_path, item_type_str, owner_group_id)

    item = create_function(item_path, owner_group_id, item_title)
    timdb = get_timdb()
    grant_access_to_session_users(timdb, item.id)
    item_type = from_str(item_type_str)
    if item_type == blocktypes.DOCUMENT:
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


def do_create_item(item_path, item_type, item_title, copied_doc: Optional[DocInfo], template_name):
    item = create_item(item_path,
                       item_type,
                       item_title,
                       DocEntry.create if item_type == 'document' else Folder.create,
                       get_current_user_group())

    if isinstance(item, DocInfo):
        if copied_doc:
            for tr, new_tr in copy_document_and_enum_translations(copied_doc, item):
                copy_default_rights(new_tr.id, blocktypes.DOCUMENT)
        else:
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


def add_tr_entry(doc_id: int, item: Item, tr: Translation) -> Translation:
    new_tr = Translation(doc_id=doc_id, src_docid=item.id, lang_id=tr.lang_id)
    new_tr.title = tr.title
    db.session.add(new_tr)
    return new_tr
