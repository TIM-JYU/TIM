"""Functions related to deleting items"""

from flask import Response
from timApp.util.flask.responsehelper import ok_response
from timApp.user.usergroup import UserGroup
from timApp.folder.folder import Folder
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo, move_document
from timApp.document.translation.translation import Translation


TRASH_FOLDER_PATH = f"roskis"


def get_trash_folder() -> Folder:
    f = Folder.find_by_path(TRASH_FOLDER_PATH)
    if not f:
        f = Folder.create(
            TRASH_FOLDER_PATH,
            owner_groups=UserGroup.get_admin_group(),
            title="Roskakori",
        )
    return f


def soft_delete_document(d: DocInfo) -> Response:
    """Performs a 'soft delete' on the specified document by moving it to the trash folder.

    :param d: The document to be deleted.
    """
    f = get_trash_folder()
    if d.path.startswith(f.path):
        # Document is already in the trash folder
        return ok_response()

    if isinstance(d, Translation):
        deleted_doc = DocEntry.create(
            f"{f.path}/tl_{d.id}_{d.src_docid}_{d.lang_id}_deleted",
            title=f"Deleted translation (src_docid: {d.src_docid}, lang_id: {d.lang_id})",
        )
        d.docentry = deleted_doc
    else:
        move_document(d, f)
    return ok_response()
