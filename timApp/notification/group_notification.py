from sqlalchemy import select

from timApp.answer.answer import Answer
from timApp.answer.answers import get_all_answer_initial_query
from timApp.document.docentry import DocEntry
from timApp.item.block import Block
from timApp.notification.send_email import send_email
from timApp.timdb.sqa import run_sql
from timApp.user.user import User
from timApp.user.usergroup import UserGroup

JOIN_MESSAGE_TASKID = "GLO_join_message"


def parse_message(message: str) -> tuple[str, str]:
    """
    Parses message by separating the Subject header from the message body.

    :param message: The message to parse
    :return: A tuple containing the subject and the message body
    """
    # First, try to read the headers
    # Then, try to read the body

    message = message.strip()
    headers = {}
    body = []
    read_line = 0

    for li, line in enumerate(message.splitlines()):
        line = line.strip()
        read_line = li
        if not line:
            break
        if ":" not in line:
            break
        key, value = line.split(":", 1)
        headers[key.strip()] = value.strip()

    for line in message.splitlines()[read_line:]:
        body.append(line)

    # TODO: Return other headers as well if needed
    return headers.get("Subject", ""), "\n".join(body).strip()


def send_group_join_message(u: User, ug: UserGroup) -> None:
    """
    Sends a group join message to the user.

    :param u: User to send the message to
    :param ug: UserGroup the user joined
    """
    admin_doc: Block | None = ug.admin_doc
    if not admin_doc:
        return
    docs: list[DocEntry] = admin_doc.docentries
    if not docs:
        return

    doc = docs[0]

    ans: Answer | None = (
        run_sql(
            select(Answer)
            .where(Answer.task_id == f"{doc.id}.{JOIN_MESSAGE_TASKID}")
            .order_by(Answer.id.desc())
            .limit(1)
        )
        .scalars()
        .first()
    )

    if not ans:
        return

    content = ans.content_as_json.get("c", None) or ans.content_as_json.get(
        "usercode", None
    )
    if not content or not isinstance(content, str):
        return

    subject, body = parse_message(content)
    if not body:
        return

    if not subject:
        subject = f"TIM: You were added to group '{ug.name}'"

    send_email(u.email, subject, body)
