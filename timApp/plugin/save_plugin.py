from typing import Optional

from timApp.document.editing.documenteditresult import DocumentEditResult
from timApp.notification.notification import NotificationType
from timApp.notification.notify import notify_doc_watchers
from timApp.plugin.plugin import Plugin
from timApp.timdb.sqa import db


def save_plugin(p: Plugin, max_attr_width: Optional[float] = None) -> None:
    old_ver = p.par.doc.get_version()
    p.save(max_attr_width)
    new_ver = p.par.doc.get_version()
    if old_ver == new_ver:
        return
    edit_result = DocumentEditResult()
    edit_result.changed.append(p.par)
    docinfo = p.par.doc.get_docinfo()
    docinfo.update_last_modified()
    notify_doc_watchers(
        docinfo,
        p.to_paragraph(max_attr_width).get_markdown(), # TODO: for big tables this takes long time. So do it nside function if there is somebody to notify
        NotificationType.ParModified, par=p.par,
        old_version=old_ver,
    )
    db.session.commit()
