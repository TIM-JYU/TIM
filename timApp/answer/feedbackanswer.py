import json
import math
from typing import List, Optional, Dict, Tuple, Iterable

from flask import Blueprint

from timApp.user.user import Consent, User
from timApp.plugin.taskid import TaskId
from datetime import datetime
from datetime import timedelta
from timApp.answer.answer import Answer
from timApp.plugin.pluginControl import task_ids_to_strlist
from sqlalchemy import func, Numeric, Float
from sqlalchemy.orm import selectinload, defaultload


from timApp.util.flask.responsehelper import json_response

feedback = Blueprint('feedback',
                     __name__,
                     url_prefix='/feedback')


def get_all_feedback_answers(task_ids: List[TaskId],
                             sort: str,
                             age: str,
                             period_from: datetime,
                             period_to: datetime) -> List[str]:
    """Gets all answers for "Dynamic Assessment" -typed tasks
    :param task_ids:
    :param usergroup:
    :param hide_names:
    :param age:
    :param printname:
    :param sort:
    :param print_opt:
    :param period_from:
    :param period_to:
    :param consent:
    :return:
    """
    q = (Answer
         .query
         .filter((period_from <= Answer.answered_on) & (Answer.answered_on < period_to))
         .filter(Answer.task_id.in_(task_ids_to_strlist(task_ids))))
    q = q.join(User, Answer.users)
    q = q.options(defaultload(Answer.users).lazyload(User.groups))

    ''' käytetään myöhemmin jos käytetään 
    if age == "min":
        minmax = func.min(Answer.id).label('minmax')
        counts = func.count(Answer.answered_on).label('count')
    elif age == "all":
        minmax = Answer.id.label('minmax')
        counts = Answer.valid.label('count')
    else:
        minmax = func.max(Answer.id).label('minmax')
        counts = func.count(Answer.answered_on).label('count')
    '''
    #minmax = Answer.id.label('minmax')
    counts = Answer.valid.label('count')

    q = q.group_by(Answer.task_id, User.id)
    # sub = q.subquery()
    q = Answer.query.join(User, Answer.users)
    if sort == 'username':
        q = q.order_by(User.name, Answer.answered_on, Answer.task_id)
    else:
        q = q.order_by(Answer.task_id, User.name, Answer.answered_on)

    q = q.with_entities(Answer, User, counts)
    results = []

    qq: Iterable[Tuple[Answer, User, int]] = q
    cnt = 0
    #pt_dt = datetime(1, 1, 1, tzinfo='UTC')    #muistiinmenevä tehtävän tekoaiku
    pt_dt = None
    result = ""
    for answer, user, n in qq:

        points = str(answer.points)
        if points == "None":
            points = ""
        name = user.name
        n = str(int(n))
        # header = f"{name};{answer.task_id};{str(answer.answered_on)};{n};{points}"
        line = json.loads(answer.content)
        answer_str = str(line)

        if cnt > 0:
            tasksecs = (answer.answered_on - pt_dt).total_seconds()
        else:
            tasksecs = 0.0


        pt_dt = answer.answered_on

        if (cnt % 2 == 1):
            result = f"{name};"
        elif cnt % 2 == 0 and cnt > 0:
            result += ";"

        taskid = answer.task_id
        i = taskid.find(".")
        if i >= 0:
            taskid = taskid[i + 1:]
        result += f"{taskid};{answer_str};{str(math.floor(tasksecs))}"
        if (cnt % 2 == 0 and cnt > 0):
            results.append(result)
        cnt += 1

    return results


@feedback.route("/test")
def test():
    taskids = Answer.query.add_column("task_id").all()
    #taskids = ["19.Plugin1", "19.diibadaaba"]
    #id = TaskId([19],"Plugin1")
    period1 = datetime.today()
    period2 = datetime.today()
    answers = get_all_feedback_answers([], 'username', "all", period1, period2)
    return json_response({'answers' : answers})
