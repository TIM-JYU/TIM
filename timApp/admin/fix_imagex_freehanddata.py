import json

from sqlalchemy import select

from timApp.admin.util import process_items, create_argparser, DryrunnableArguments
from timApp.answer.answer import Answer
from timApp.document.docinfo import DocInfo
from timApp.timdb.sqa import run_sql


def fix_imagex_freehanddata(doc: DocInfo, args: DryrunnableArguments) -> int:
    """Fixes invalid imagex freeHandData.
    An invalid entry in freeHandData looks like:
    ::
        {'color': '#ff0', 'lines': [[None, None]], 'w': 3}

    :param doc: The document to fix.
    :param args: The arguments.
    """
    errors = 0
    answers: list[Answer] = (
        run_sql(select(Answer).filter(Answer.task_id.startswith(f"{doc.id}.")))
        .scalars()
        .all()
    )
    for a in answers:
        data = a.content_as_json
        freehanddata = data.get("freeHandData")
        if not freehanddata:
            continue
        print(f"Processing imagex answer: {a.id} {a.task_id} {a.answered_on}")
        valid_data = []
        answer_has_errors = False
        for f in freehanddata:
            lines = f["lines"]
            if len(lines) == 1 and lines[0][0] is None:
                answer_has_errors = True
                errors += 1
                print(f"Invalid lines in answer {a.id}")
            else:
                valid_data.append(f)
        if answer_has_errors:
            data["freeHandData"] = valid_data
            a.content = json.dumps(data)
    return errors


if __name__ == "__main__":
    process_items(
        fix_imagex_freehanddata, create_argparser("Fixes invalid imagex freehanddata")
    )
