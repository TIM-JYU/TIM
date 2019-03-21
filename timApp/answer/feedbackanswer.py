import json
from typing import List, Tuple, Iterable
import dateutil.relativedelta

from flask import Blueprint, request

from timApp.user.user import User
from timApp.util.flask.responsehelper import csv_response
from timApp.plugin.taskid import TaskId
from datetime import datetime, timedelta, timezone
from timApp.answer.answer import Answer
from timApp.plugin.pluginControl import task_ids_to_strlist

from timApp.document.docentry import DocEntry
from timApp.util.flask.requesthelper import get_option
from timApp.util.utils import get_current_time
from timApp.auth.accesshelper import verify_logged_in, verify_teacher_access
from timApp.plugin.pluginControl import find_task_ids

feedback = Blueprint('feedback',
                     __name__,
                     url_prefix='/feedback')


def get_all_feedback_answers(task_ids: List[TaskId],
                             hide_names: bool,
                             printname: bool,
                             sort: str,
                             period_from: datetime,
                             period_to: datetime) -> List[str]:
    """

    :param task_ids: ids of tasks
    :param hide_names: names are displayed anonymously
    :param printname: full name and username or just username
    :param sort: sort by task or by
    :param period_from: don't return dates before this date
    :param period_to: don't return dates after this date
    :return: returns string in csv-form of results of adaptive feedback test
    """

    # Query from answer-table for the given time period and TaskId:s
    q = (Answer
         .query
         .filter((period_from <= Answer.answered_on) & (Answer.answered_on < period_to))
         .filter(Answer.task_id.in_(task_ids_to_strlist(task_ids))))
    # Also joins with user table to get user information
    q = q.join(User, Answer.users)
    # Not clear what this does TODO: figure out and remove if not needed
    #q = q.options(defaultload(Answer.users).lazyload(User.groups))

    counts = Answer.valid.label('count')  # Not really used TODO: check and remove

    # Sorts the answers first with user then by time and last by task_id (task_id sort not necessary?)
    if sort == 'username':
        q = q.order_by(User.name, Answer.answered_on, Answer.task_id)
    # For a report this option not necessary for now at least - depends what type reports needed in research
    else:
        q = q.order_by(Answer.task_id, User.name, Answer.answered_on)

    # q with Answer, User and count data - counts not really used... TODO: remove counts if not needed
    q = q.with_entities(Answer, User, counts)

    # makes q query an iterable qq for for-loop
    qq: Iterable[Tuple[Answer, User, int]] = q

    return compile_csv(qq, printname, hide_names) #results


def compile_csv(qq: Iterable[Tuple[Answer, User, int]], printname: bool, hide_names: bool):
    """

    :param qq: database data of answers
    :param printname: full name and username or just username
    :param hide_names: names are displayed anonymously
    :return: return string in csv-format
    """

    pt_dt = None    #Previous Datetime of previous answer
    prev_ans = None     #Previous Answer
    prev_user = None     #Previous User
    temp_bool = True    #Temporary answer combination condition, now it is every even
    exclude_first = True #Temporary setting for excluding the first

    results = []

    #First line are the headers
    if printname and not hide_names:
        results.append(['Full Name','Username', 'Result', 'Answer', 'Feedback', 'Time spent on item(sec)', 'Time spent on feedback(sec)'])
    else:
        results.append(['Username', 'Result', 'Answer', 'Feedback', 'Time spent on item(sec)', 'Time spent on feedback(sec)'])

    #Dictionary and counting for anons
    anons = {}
    cnt = 0

    #FOR STARTS FROM HERE
    for answer, user, n in qq:

        """
        points = str(answer.points)
        if points == "None":
            points = ""
        """
        """
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
        """
        if prev_user != user:
            prev_user = None
            prev_ans = None
            pt_dt = None
            temp_bool = True
        if prev_user == None: prev_user = user
        if prev_ans == None: prev_ans = answer
        if temp_bool:
            if not (prev_user in anons):
                anons[prev_user] = f"user{cnt}"
                cnt += 1
            anonuser = anons[prev_user]
            answer_content = json.loads(prev_ans.content)   # .get(prev_ans.content) #json.loads(prev_ans.content)
            feedback_content = json.loads(answer.content)   # json.loads(answer.content)

            if pt_dt != None:
                tasksecs = (prev_ans.answered_on - pt_dt).total_seconds()
            else:
                tasksecs = 0.0

            fbsecs = (answer.answered_on - prev_ans.answered_on).total_seconds()

            if (hide_names):
                shown_user = anonuser
            else:
                shown_user = prev_user.name

            if exclude_first and pt_dt:   # IF-CONDITION temporary for the sake of excluding the first sample item
                if printname and not hide_names:
                    results.append([prev_user.real_name]
                                + [shown_user]
                                + ['WRONG!!!']
                                + [answer_content]
                                + [feedback_content]
                                + [str(round(tasksecs, 1))]
                                + [str(round(fbsecs, 1))])
                else:
                    results.append([shown_user]
                                + ['WRONG!!!']
                                + [answer_content]
                                + [feedback_content]
                                + [str(round(tasksecs, 1))]
                                + [str(round(fbsecs, 1))])

        pt_dt = prev_ans.answered_on
        prev_ans = answer
        prev_user = user
        temp_bool = not temp_bool

    return results



# Route to test feedback output at feedback/test
@feedback.route("/test/<path:doc_path>")
def test(doc_path):
    verify_logged_in()
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)

    pars = d.document.get_dereferenced_paragraphs()
    task_ids, _, _ = find_task_ids(pars)

    nro = int(d.id)#int(number)
    name = get_option(request, 'name', 'both')
    hidename = False
    fullname = False
    format = get_option(request, 'format', 'excel')

    if name == 'anonymous':
        hidename = True
    elif name == 'both':
        fullname = True

    if format == 'tab':
        dialect = 'excel-tab'
    else:
        dialect = 'excel'

    period = get_option(request, 'period', 'whenever')
    period_from = datetime.min.replace(tzinfo=timezone.utc)
    period_to = get_current_time()

    if period == 'whenever':
        pass
    elif period == 'sincelast':
        pass    # TODO: last fetch
    elif period == 'day':
        period_from = period_to - timedelta(days=1)
    elif period == 'week':
        period_from = period_to - timedelta(weeks=1)
    elif period == 'month':
        period_from = period_to - dateutil.relativedelta.relativedelta(months=1)
    elif period == 'other':
        period_from_str = get_option(request, 'periodFrom', period_from.isoformat())
        period_to_str = get_option(request, 'periodTo', period_to.isoformat())
        try:
            period_from = dateutil.parser.parse(period_from_str)
        except (ValueError, OverflowError):
            pass
        try:
            period_to = dateutil.parser.parse(period_to_str)
        except (ValueError, OverflowError):
            pass

    # For choosing specific tasks for the report a list of tasks is given
    # TODO: check real call to see how task_ids from different pages can be called
    taskid1 = TaskId(nro, "sample_item")  # generating TaskIds - (pageid int, taskname string)
    taskid2 = TaskId(nro, "item1")
    taskid3 = TaskId(nro, "feedback1")
    taskid4 = TaskId(nro, "item2")
    taskid5 = TaskId(nro, "feedback2")
    taskids_test = [taskid1, taskid2, taskid3, taskid4, taskid5]



    answers = get_all_feedback_answers(task_ids, hidename, fullname,
                                        'username', period_from, period_to)

    # First returns empty, then from document nro1 and then document nro2 (change above if different)
    return csv_response(answers, dialect)
    """ return json_response({'answers0': answers0,
                          'answers1': answers1,
                          'answers2': answers2})"""





