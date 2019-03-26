import json
from typing import List, Tuple, Iterable
import dateutil.relativedelta

from flask import Blueprint, request

from timApp.auth.sessioninfo import get_current_user, get_current_user_object
from timApp.plugin.plugin import Plugin
from timApp.timdb.dbaccess import get_timdb
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.responsehelper import csv_response
from timApp.plugin.taskid import TaskId
from datetime import datetime, timedelta, timezone
from timApp.answer.answer import Answer
from timApp.plugin.pluginControl import task_ids_to_strlist

from timApp.document.docentry import DocEntry
from timApp.util.flask.requesthelper import get_option
from timApp.util.utils import get_current_time
from timApp.auth.accesshelper import verify_logged_in, verify_teacher_access, get_doc_or_abort
from timApp.plugin.pluginControl import find_task_ids
import csv

feedback = Blueprint('feedback',
                     __name__,
                     url_prefix='/feedback')


def get_all_feedback_answers(task_ids: List[TaskId],
                             hide_names: bool,
                             printname: bool,
                             valid: str,
                             period_from: datetime,
                             period_to: datetime):
    """

    :param task_ids: ids of tasks
    :param hide_names: names are displayed anonymously
    :param printname: full name and username or just username
    :param sort: sort by task or by
    :param valid: 0, 1 or all
    :param period_from: don't return dates before this date
    :param period_to: don't return dates after this date
    :return: returns string in csv-form of results of adaptive feedback test
    """

    # Query from answer-table for the given time period and TaskId:s
    q = (Answer
         .query
         .filter((period_from <= Answer.answered_on) & (Answer.answered_on < period_to))
         .filter(Answer.task_id.in_(task_ids_to_strlist(task_ids))))
    # Checks validity of answers and filters if needed.
    if valid == 'all':
        pass
    elif valid == '0':
        q = q.filter_by(valid=False)
    else:
        q = q.filter_by(valid=True)
    # Also joins with user table to get user information
    q = q.join(User, Answer.users)

    # TODO: figure out what the following does and remove if not needed
    # q = q.options(defaultload(Answer.users).lazyload(User.groups))

    # Sorts the answers first with user then by time - for a report of whole test result by one user
    q = q.order_by(User.name, Answer.answered_on)

    # q with Answer and User data
    q = q.with_entities(Answer, User)

    # makes q query an iterable qq for for-loop
    qq: Iterable[Tuple[Answer, User]] = q

    return compile_csv(qq, printname, hide_names)  # results


def compile_csv(qq: Iterable[Tuple[Answer, User]], printname: bool, hide_names: bool):
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
    for answer, user in qq:

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

        # find type of the plugin

        answer_task_id = answer.task_id
        p = Plugin.from_task_id(answer_task_id, user=get_current_user_object())
        ptype = p.type

        if ptype == 'feedback':
            if not (prev_user in anons):
                anons[prev_user] = f"user{cnt}"
                cnt += 1
            anonuser = anons[prev_user]
            correct = json.loads(answer.content)['correct']
            answer_content = json.loads(answer.content)['sentence']   # .get(prev_ans.content) #json.loads(prev_ans.content)
            feedback_content = json.loads(answer.content)['feedback']   # json.loads(answer.content)

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
                    results.append([prev_user.real_name,
                                    shown_user,correct,
                                    answer_content,
                                    feedback_content,
                                    str(round(tasksecs, 1)),
                                    str(round(fbsecs, 1))])
                else:
                    results.append([shown_user,
                                correct,
                                answer_content,
                                feedback_content,
                                str(round(tasksecs, 1)),
                                str(round(fbsecs, 1))])

        pt_dt = prev_ans.answered_on
        prev_ans = answer
        prev_user = user
        temp_bool = not temp_bool

    return results



# Route to test feedback output at feedback/test
@feedback.route("/test/<path:doc_path>")
def test(doc_path):
    verify_logged_in()
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)    #return task from URL
    verify_teacher_access(d)
    pars = d.document.get_dereferenced_paragraphs()     # TODO: add documentation
    task_ids, _, _ = find_task_ids(pars)                # 


    #filtered_task_ids = filterPlugin(task_ids, 'all')  #filteröi muut paitsi feedback
    name = get_option(request, 'name', 'both')
    hidename = False
    fullname = False
    format = get_option(request, 'format', 'excel')
    validity = get_option(request, 'valid', 'all')

    if name == 'anonymous':
        hidename = True
    elif name == 'both':
        fullname = True

    if format == 'tab':
        dialect = 'excel-tab'
    elif format == 'comma':
        dialect = 'excel'
    else:
        dialect = csv.excel
        dialect.delimiter = ';'


    period = get_option(request, 'period', 'whenever')
    period_from = datetime.min.replace(tzinfo=timezone.utc)
    period_to = get_current_time()

    # Some fetch-copypasta
    doc_ids = set()
    for tid in task_ids:
        doc_ids.add(tid.doc_id)
        d = get_doc_or_abort(tid.doc_id)

    # moar fetch-copypasta
    since_last_key = task_ids[0].doc_task
    if len(task_ids) > 1:
        since_last_key = str(next(d for d in doc_ids))
        if len(doc_ids) > 1:
            since_last_key = None


    if period == 'whenever':
        pass
    elif period == 'sincelast':
        u = get_current_user_object()
        prefs = u.get_prefs()
        last_answer_fetch = prefs.last_answer_fetch
        period_from = last_answer_fetch.get(since_last_key, datetime.min.replace(tzinfo=timezone.utc))
        last_answer_fetch[since_last_key] = get_current_time()
        prefs.last_answer_fetch = last_answer_fetch
        u.set_prefs(prefs)
        db.session.commit()
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

    #get answers for the task ids with the proper parameters
    answers = get_all_feedback_answers(task_ids, hidename, fullname, validity, period_from, period_to)


    # First returns empty, then from document nro1 and then document nro2 (change above if different)
    return csv_response(answers, dialect)
    """ return json_response({'answers0': answers0,
                          'answers1': answers1,
                          'answers2': answers2})"""



"""
def filterPlugin(task_ids: List[TaskId], plugin_name: str):
    filtered_task_ids = []
    if plugin_name == 'all':
        return task_ids
    for tid in task_ids:
        p = Plugin.from_task_id(tid.doc_task, user=get_current_user_object())
        ptype = p.type
        if ptype == plugin_name:
            filtered_task_ids.append(tid)
    return filtered_task_ids
"""
#-
# {#t1}