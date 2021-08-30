from typing import List, Generator, Tuple, Union, Optional

from timApp.auth.accesshelper import grant_access_to_session_users, reset_request_access_cache, get_doc_or_abort, \
    verify_edit_access
from timApp.auth.sessioninfo import get_current_user_object, get_current_user_group_object
from timApp.document.docentry import DocEntry, get_documents, create_document_and_block
from timApp.document.docinfo import DocInfo
from timApp.document.documents import apply_citation
from timApp.document.specialnames import FORCED_TEMPLATE_NAME, TEMPLATE_FOLDER_NAME, PREAMBLE_FOLDER_NAME
from timApp.document.translation.translation import Translation, add_tr_entry
from timApp.folder.folder import Folder
from timApp.item.block import copy_default_rights, BlockType
from timApp.item.tag import TagType, Tag
from timApp.item.validation import validate_item_and_create_intermediate_folders, ItemValidationRule
from timApp.tim_app import app
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import DOC_DEFAULT_RIGHT_NAME, FOLDER_DEFAULT_RIGHT_NAME
from timApp.util.flask.requesthelper import RouteException
from timApp.util.utils import split_location


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


def create_document(
        item_path: str,
        item_title: str,
        validation_rule: ItemValidationRule = None,
        parent_owner: UserGroup = None,
) -> DocInfo:
    return do_create_item(item_path, BlockType.Document, item_title, validation_rule, parent_owner)


def do_create_item(
        item_path: str,
        item_type: BlockType,
        item_title: str,
        validation_rule: ItemValidationRule = None,
        parent_owner: UserGroup = None,
) -> Union[DocInfo, Folder]:
    create_function = DocEntry.create if item_type == BlockType.Document else Folder.create
    owner_group = get_current_user_group_object()

    item_path = item_path.strip('/')

    validate_item_and_create_intermediate_folders(item_path, item_type, parent_owner or owner_group, validation_rule)

    item = create_function(item_path, owner_group, item_title)
    grant_access_to_session_users(item)
    if item_type == BlockType.Document and app.config['BOOKMARKS_ENABLED']:
        bms = get_current_user_object().bookmarks
        bms.add_bookmark('Last edited',
                         item.title,
                         item.url_relative,
                         move_to_top=True,
                         limit=app.config['LAST_EDITED_BOOKMARK_LIMIT']).save_bookmarks()
    copy_default_rights(item, item_type)

    reset_request_access_cache()
    return item


def apply_template(item: DocInfo, template_name: Optional[str] = None):
    templates = get_templates_for_folder(item.parent)
    matched_templates = None
    if template_name:
        matched_templates = list(filter(lambda t: t.short_name == template_name, templates))
    if not matched_templates:
        matched_templates = list(filter(lambda t: t.short_name == FORCED_TEMPLATE_NAME, templates))
    if matched_templates:
        template = matched_templates[0]
        item.document.update(template.document.export_markdown(), item.document.export_markdown())


def copy_document_and_enum_translations(source: DocInfo, target: DocInfo, copy_uploads: bool) -> Generator[
    Tuple[DocInfo, DocInfo], None, None]:
    is_preamble = f'/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/' in source.path

    if copy_uploads:
        target.children.extend(source.children)  # required to retain rights to uploaded files

    # Copy tags except course code and subject.
    for tag in source.block.tags:
        if tag.type == TagType.Regular:
            target.block.tags.append(Tag(name=tag.name, type=tag.type, expires=tag.expires))

    target.document.update(source.document.export_markdown(),
                           target.document.export_markdown(),
                           strict_validation=False,
                           regenerate_ids=is_preamble)
    for tr in source.translations:  # type: Translation
        doc_id = target.id
        new_tr = None
        if not tr.is_original_translation:
            document = create_document_and_block(get_current_user_object().get_personal_group())
            doc_id = document.doc_id
            new_tr = add_tr_entry(doc_id, target, tr)
            document.docinfo = new_tr
            document.update(tr.document.export_markdown(),
                            document.export_markdown(),
                            strict_validation=False,
                            regenerate_ids=is_preamble)
        elif tr.lang_id:
            add_tr_entry(doc_id, target, tr)
        if not tr.is_original_translation:
            yield tr, new_tr


def create_or_copy_item(
        item_path: str,
        item_type: BlockType,
        item_title: str,
        copy_id: Optional[int] = None,
        template_name: Optional[str] = None,
        use_template: bool = True,
        copy_uploads: bool = True,
):
    d = None
    if copy_id:
        d = get_doc_or_abort(copy_id)
        verify_edit_access(d)
        d = d.src_doc
        vr = d.document.validate()
        if vr.issues:
            raise RouteException(f'The following errors must be fixed before copying:\n{vr}')
    item = do_create_item(item_path, item_type, item_title)
    if isinstance(item, DocInfo):
        if d:
            for tr, new_tr in copy_document_and_enum_translations(d, item, copy_uploads=copy_uploads):
                copy_default_rights(new_tr, BlockType.Document)
        elif use_template:
            apply_template(item, template_name)
    return item


def create_citation_doc(doc_id: int, doc_path: str, doc_title: str):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    src_doc = d.document
    item = create_document(doc_path, doc_title)
    apply_citation(item, src_doc)
    return item
