import urllib.parse
from collections import defaultdict
from dataclasses import dataclass
from threading import Thread
from typing import DefaultDict, Callable

from flask import current_app
from sqlalchemy import select
from sqlalchemy.orm import selectinload

from timApp.auth.accesshelper import (
    verify_logged_in,
    verify_view_access,
    get_item_or_abort,
)
from timApp.auth.sessioninfo import get_current_user_object, get_current_user_id
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.version import Version, ver_to_str
from timApp.item.block import Block
from timApp.notification.notification import NotificationType, Notification
from timApp.notification.pending_notification import (
    DocumentNotification,
    CommentNotification,
    PendingNotification,
    get_pending_notifications,
    GroupingKey,
    AnswerNotification,
    AnnotationNotification,
)
from timApp.notification.send_email import send_email
from timApp.tim_app import app
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import get_current_time, seq_to_str

notify = TypedBlueprint(
    "notify",
    __name__,
    url_prefix="/notify",
)


@notify.get("/<int:doc_id>")
def get_notify_settings(doc_id):
    verify_logged_in()
    i = get_item_or_abort(doc_id)
    verify_view_access(i)
    return json_response(get_current_user_object().get_notify_settings(i))


@notify.post("/<int:doc_id>")
def set_notify_settings(
    doc_id: int,
    email_doc_modify: bool,
    email_comment_add: bool,
    email_comment_modify: bool,
    email_answer_add: bool,
    email_annotation_add: bool,
    email_annotation_modify: bool,
):
    verify_logged_in()
    i = get_item_or_abort(doc_id)
    verify_view_access(i)
    get_current_user_object().set_notify_settings(
        i,
        doc_modify=email_doc_modify,
        comment_add=email_comment_add,
        comment_modify=email_comment_modify,
        answer_add=email_answer_add,
        annotation_add=email_annotation_add,
        annotation_modify=email_annotation_modify,
    )
    db.session.commit()
    return ok_response()


@notify.get("/all")
def get_user_notify_settings():
    verify_logged_in()
    nots = get_current_user_notifications()
    return json_response(nots)


def get_current_user_notifications(limit: int | None = None):
    stmt = (
        select(Notification)
        .filter_by(user_id=get_current_user_id())
        .options(selectinload(Notification.block).selectinload(Block.docentries))
        .options(selectinload(Notification.block).selectinload(Block.folder))
        .options(selectinload(Notification.block).selectinload(Block.translation))
        .order_by(Notification.block_id.desc())
    )

    if limit is not None:
        stmt = stmt.limit(limit)
    nots = run_sql(stmt).scalars().all()
    return nots


def notify_doc_watchers(
    doc: DocInfo,
    content_msg: str,
    notify_type: NotificationType,
    par: DocParagraph | None = None,
    old_version: Version = None,
    curr_user: User = None,
    **kwargs,
):
    me = curr_user if curr_user else get_current_user_object()
    new_version = doc.document.get_version()

    match notify_type:
        case NotificationType.DocModified | NotificationType.ParAdded | NotificationType.ParDeleted | NotificationType.ParModified:
            p = DocumentNotification(
                user=me,
                doc_id=doc.id,
                par_id=par.get_id() if par else None,
                text=content_msg,
                version_change=f"{ver_to_str(old_version)}/{ver_to_str(new_version)}",
                kind=notify_type,
                **kwargs,
            )
        case NotificationType.CommentAdded | NotificationType.CommentModified | NotificationType.CommentDeleted:
            p = CommentNotification(
                user=me,
                doc_id=doc.id,
                par_id=par.get_id() if par else None,
                text=content_msg,
                kind=notify_type,
                **kwargs,
            )
        case NotificationType.AnswerAdded:
            p = AnswerNotification(
                user=me,
                doc_id=doc.id,
                par_id=par.get_id() if par else None,
                text=content_msg,
                kind=notify_type,
                **kwargs,
            )
        case NotificationType.AnnotationAdded | NotificationType.AnnotationModified | NotificationType.AnnotationDeleted:
            p = AnnotationNotification(
                user=me,
                doc_id=doc.id,
                par_id=par.get_id() if par else None,
                text=content_msg,
                kind=notify_type,
                **kwargs,
            )
        case _:
            p = None

    if p is not None:
        db.session.add(p)


def get_name_string(users: list[User], show_names: bool):
    num_users = len(users)
    if show_names:
        return seq_to_str(list(u.pretty_full_name for u in users))
    else:
        return get_user_count_str(num_users)


MIXED_DOC_MODIFY = "doc_modify"
MIXED_COMMENT = "comment"
MIXED_ANNOTATION = "velp"


def get_diff_link(docentry: DocInfo, ver_before: Version, ver_after: Version):
    return f"""{current_app.config['TIM_HOST']}/diff/{docentry.id}/{ver_before[0]}/{ver_before[1]}/{ver_after[0]}/{ver_after[1]}"""


NOTIFICATION_TITLE = {
    NotificationType.DocModified: "Document modified",
    NotificationType.ParAdded: "Paragraph added",
    NotificationType.ParModified: "Paragraph modified",
    NotificationType.ParDeleted: "Paragraph deleted",
    NotificationType.CommentAdded: "Comment posted",
    NotificationType.CommentModified: "Comment modified",
    NotificationType.CommentDeleted: "Comment deleted",
    NotificationType.AnswerAdded: "Answer posted",
    NotificationType.AnnotationAdded: "Velp posted",
    NotificationType.AnnotationModified: "Velp modified",
    NotificationType.AnnotationDeleted: "Velp deleted",
}


def get_message_for(
    ps: list[PendingNotification],
    d: DocInfo,
    show_text: bool,
    show_names: bool,
    show_diff_link: bool = True,
):
    msg = ""
    num_chgs = len(ps)
    if (
        ps[0].notify_type.is_document_modification
        and num_chgs > 1
        and (show_text or show_diff_link)
    ):
        first = ps[0]
        last = ps[-1]
        assert isinstance(
            first, DocumentNotification
        ), "Expected a DocumentNotification"
        assert isinstance(last, DocumentNotification), "Expected a DocumentNotification"
        first_ver = first.version_before
        last_ver = last.version_after
        msg += f"Link to all changes: {get_diff_link(d, first_ver, last_ver)}\n\n"
        if show_text:
            msg += f"The individual changes ({num_chgs}) are listed below.\n\n"

    for p in ps:
        name_str = get_name_string([p.user], show_names=show_names)
        par = p.par_id
        t = p.notify_type
        s = NOTIFICATION_TITLE.get(t, None)
        if s is None:
            continue

        par_anchor = f"#{par}" if par else ""
        url = f"{d.url}{par_anchor}"
        if show_names:
            s += f" by {name_str}"
            d.document.insert_preamble_pars()
            if par and not p.notify_type.is_document_modification:
                try:
                    pobj = d.document.get_paragraph(par)
                except TimDbException:
                    pass
                else:
                    params_dict = {}
                    url_to_answer = p.notify_type == NotificationType.AnswerAdded
                    if pobj.is_task():
                        url_to_answer = True
                        task_id = pobj.get_attr("taskId")
                        params_dict |= {"task": task_id}
                        if isinstance(p, AnswerNotification):
                            params_dict |= {"answerNumber": p.answer_number}

                    if url_to_answer:
                        params_dict |= {
                            "user": p.user.name,
                            "valid_answers_only": "false",
                        }
                        params = urllib.parse.urlencode(params_dict)
                        url = f'{d.get_url_for_view("answers")}?{params}'
                        if "task" not in params_dict:
                            url += par_anchor

        if isinstance(p, AnswerNotification):
            s += f" to '{p.task_id}'"

        msg += f"{s}: {url}"

        if (show_text or show_diff_link) and p.notify_type.is_document_modification:
            assert isinstance(p, DocumentNotification)
            v1 = p.version_before
            v2 = p.version_after
            msg += f" (changes: {get_diff_link(d, v1, v2)} )"
        msg += "\n\n"
        if show_text or not p.notify_type.is_document_modification:
            msg += p.text + "\n\n"
    return msg.strip()


def get_edit_count_str(num_edits):
    if num_edits > 1:
        return f"{num_edits} times"
    else:
        return ""


def get_user_count_str(num_users):
    if num_users > 1:
        return f"{num_users} people"
    else:
        return "Someone"


def get_par_count_str(num_mods):
    if num_mods > 1:
        return f"{num_mods} paragraphs"
    else:
        return "a paragraph"


def get_comment_count_str(num_mods):
    if num_mods > 1:
        return f"{num_mods} comments"
    else:
        return "a comment"


def get_answer_count_str(num_mods):
    if num_mods > 1:
        return f"{num_mods} answers"
    else:
        return "an answer"


def get_annotation_count_str(num_mods):
    if num_mods > 1:
        return f"{num_mods} velps"
    else:
        return "a velp"


@dataclass(frozen=True, slots=True)
class NotificationSubject:
    subject_template: str
    num_count_modifier: Callable[[int], str]

    def message(self, user: str, num_count: int, resource_title: str) -> str:
        return self.subject_template.format_map(
            dict(
                user=user,
                num_count=self.num_count_modifier(num_count),
                resource_title=resource_title,
            )
        )


NOTIFICATION_TITLE_SUBJECT: dict[NotificationType, NotificationSubject] = {
    NotificationType.ParAdded: NotificationSubject(
        subject_template="{user} added {num_count} to the document {resource_title}",
        num_count_modifier=get_par_count_str,
    ),
    NotificationType.ParModified: NotificationSubject(
        subject_template="{user} modified {num_count} in the document {resource_title}",
        num_count_modifier=get_par_count_str,
    ),
    NotificationType.ParDeleted: NotificationSubject(
        subject_template="{user} deleted {num_count} from the document {resource_title}",
        num_count_modifier=get_par_count_str,
    ),
    NotificationType.CommentAdded: NotificationSubject(
        subject_template="{user} posted {num_count} to the document {resource_title}",
        num_count_modifier=get_comment_count_str,
    ),
    NotificationType.CommentModified: NotificationSubject(
        subject_template="{user} modified {num_count} in the document {resource_title}",
        num_count_modifier=get_comment_count_str,
    ),
    NotificationType.CommentDeleted: NotificationSubject(
        subject_template="{user} deleted {num_count} from the document {resource_title}",
        num_count_modifier=get_comment_count_str,
    ),
    NotificationType.AnswerAdded: NotificationSubject(
        subject_template="{user} posted {num_count} to the document {resource_title}",
        num_count_modifier=get_answer_count_str,
    ),
    NotificationType.AnnotationAdded: NotificationSubject(
        subject_template="{user} posted {num_count} to the document {resource_title}",
        num_count_modifier=get_annotation_count_str,
    ),
    NotificationType.AnnotationModified: NotificationSubject(
        subject_template="{user} modified {num_count} in the document {resource_title}",
        num_count_modifier=get_annotation_count_str,
    ),
    NotificationType.AnnotationDeleted: NotificationSubject(
        subject_template="{user} deleted {num_count} from the document {resource_title}",
        num_count_modifier=get_annotation_count_str,
    ),
}


def get_subject_for(ps: list[PendingNotification], d: DocInfo, show_names: bool) -> str:
    num_mods = len(ps)
    distinct_users = list({p.user for p in ps})
    type_of_all = get_type_of_notify(ps)
    name_str = get_name_string(distinct_users, show_names)

    notif_type = NOTIFICATION_TITLE_SUBJECT.get(type_of_all, None)
    if notif_type is not None:
        return notif_type.message(name_str, num_mods, d.title)
    else:
        # TODO: Allow aggregating notifications programmatically
        if (
            type_of_all == NotificationType.DocModified
            or type_of_all == MIXED_DOC_MODIFY
        ):
            return f"{name_str} edited the document {d.title} {get_edit_count_str(num_mods)}"
        if type_of_all == MIXED_COMMENT:
            return f"{name_str} posted/modified/deleted {get_comment_count_str(num_mods)} in the document {d.title}"
        if type_of_all == MIXED_ANNOTATION:
            return f"{name_str} posted/modified/deleted {get_annotation_count_str(num_mods)} in the document {d.title}"

    return f"{name_str} triggered an event in {d.title}"


def get_type_of_notify(ps) -> NotificationType | str:
    for n in NotificationType:
        if all(p.notify_type == n for p in ps):
            return n
    if all(p.notify_type.is_document_modification for p in ps):
        return MIXED_DOC_MODIFY
    elif all(not p.notify_type.is_comment_notification for p in ps):
        return MIXED_COMMENT
    elif all(not p.notify_type.is_velp_notification for p in ps):
        return MIXED_ANNOTATION
    else:
        assert (
            False
        ), "There should not be mixed comment and doc modification notification types in a batch"


@notify.get("/process")
def force_process():
    process_pending_notifications()
    return "OK"


def process_pending_notifications():
    pns = get_pending_notifications()
    grouped_pns: DefaultDict[GroupingKey, list[PendingNotification]] = defaultdict(list)
    email_threads: list[Thread] = []
    for p in pns:
        grouped_pns[p.grouping_key].append(p)
    for (doc_id, t), ps in grouped_pns.items():
        doc = DocEntry.find_by_id(doc_id)
        settings = doc.document.get_settings()
        show_only_diff = settings.send_basic_change_notifications()
        # Combine ps to a single mail (tailored for each subscriber) and send it
        match t:
            case "d":
                assert all(
                    isinstance(p, DocumentNotification) for p in ps
                ), "Expected all notifications of type DocumentNotification"
                condition = Notification.notification_type.in_(
                    (
                        NotificationType.DocModified,
                        NotificationType.ParModified,
                        NotificationType.ParAdded,
                        NotificationType.ParDeleted,
                    )
                )
            case "c":
                assert all(
                    isinstance(p, CommentNotification) for p in ps
                ), "Expected all notifications of type CommentNotification"
                condition = Notification.notification_type.in_(
                    (
                        NotificationType.CommentAdded,
                        NotificationType.CommentDeleted,
                        NotificationType.CommentModified,
                    )
                )
            case "a":
                assert all(
                    isinstance(p, AnswerNotification) for p in ps
                ), "Expected all notifications of type AnswerNotification"
                condition = Notification.notification_type.in_(
                    (NotificationType.AnswerAdded,)
                )
            case "v":
                assert all(
                    isinstance(p, AnnotationNotification) for p in ps
                ), "Expected all notifications of type AnnotationNotification"
                condition = Notification.notification_type.in_(
                    (
                        NotificationType.AnnotationAdded,
                        NotificationType.AnnotationModified,
                        NotificationType.AnnotationDeleted,
                    )
                )
            case _:
                assert False, "Unknown notification type"

        users_to_notify: set[User] = {n.user for n in doc.get_notifications(condition)}
        for user in users_to_notify:
            if (
                not user.email
                or not user.has_view_access(doc)
                or user.get_prefs().is_item_excluded_from_emails(doc)
            ):
                continue

            is_teacher: bool = user.has_teacher_access(doc) is not None
            ps_to_consider = [
                p
                for p in ps
                if p.user != user  # don't send emails about own actions
                and (
                    is_teacher or p.notify_type != NotificationType.AnswerAdded
                )  # don't send emails about answers to non-teachers
            ]

            # TODO Currently email_comment_add and email_comment_modify are basically the same option.
            # Should do additional filtering here.

            if not ps_to_consider:
                continue

            is_annotation: bool = ps_to_consider[0].notify_type in [
                NotificationType.AnnotationAdded,
                NotificationType.AnnotationModified,
                NotificationType.AnnotationDeleted,
            ]

            # Poster identity should be hidden unless the user has teacher access to the document
            subject = get_subject_for(
                ps_to_consider, doc, show_names=is_annotation or is_teacher
            )
            can_edit = (
                user.has_edit_access(doc)
                or not ps_to_consider[0].notify_type.is_document_modification
            )
            show_text = not show_only_diff and can_edit
            # If a document was modified and the user doesn't have edit access to it, we must not send the source md
            msg = get_message_for(
                ps_to_consider,
                doc,
                show_text=show_text,
                show_diff_link=can_edit,
                show_names=is_annotation or is_teacher,
            )

            is_unique_user = len({p.user for p in ps_to_consider}) == 1
            reply_to = (
                ps_to_consider[0].user.email
                if (is_annotation or is_teacher) and is_unique_user
                else None
            )
            result = send_email(
                user.email,
                subject,
                msg,
                mail_from=app.config["NOREPLY_EMAIL"],
                reply_to=reply_to,
            )
            if result:
                email_threads.append(result)
        for p in ps:
            p.processed = get_current_time()
            # To save database space, we null the text for all document notifications.
            # The document history already exists elsewhere, so we don't need it to store it.
            if isinstance(p, DocumentNotification):
                p.text = None
    for t in email_threads:
        t.join()
    db.session.commit()
