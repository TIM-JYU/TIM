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

from timApp.document.docentry import DocEntry, get_documents_in_folder
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
                             exp_answers: str,
                             user: List[str],
                             period_from: datetime,
                             period_to: datetime):
    """
    Get feedback answer resutls
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

    return compile_csv(qq, printname, hide_names, exp_answers, user)  # results


def compile_csv(qq: Iterable[Tuple[Answer, User]], printname: bool, hide_names: bool, exp_answers: str, s_user: [str]):
    """
    compile data into more csv friendly form
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

    """
    #First line are the headers TODO: these could be moved to test()-function
    if printname and not hide_names and use_headers:
        results.append(['Full Name','Username', 'Result', 'Item', 'Selected option', 'Feedback', 'Time spent on item(sec)', 'Time spent on feedback(sec)'])
    elif use_headers:
        results.append(['Username', 'Result', 'Item', 'Selected option', 'Feedback', 'Time spent on item(sec)', 'Time spent on feedback(sec)'])
    else:
        results.append([])
    """

    #Dictionary and counting for anons
    anons = {}
    cnt = 0

    #FOR STARTS FROM HERE
    for answer, user in qq:
        # TODO: remove documented code snippets when absolutely is known they are no longer needed
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

        if prev_user != user:   # resets if previous is different user
            prev_user = None
            prev_ans = None
            pt_dt = None
            temp_bool = True

        if prev_user is None:
            prev_user = user
        if prev_ans is None:
            prev_ans = answer

        # find type of the plugin

        answer_task_id = answer.task_id
        p = Plugin.from_task_id(answer_task_id, user=get_current_user_object())
        ptype = p.type

        # boolean to check whether to filter out current user by name
        is_correct_user_s = False
        if exp_answers == 'selected':
            is_correct_user_s = user.name == s_user[0]
        elif exp_answers == 'visible':
            if user.name in s_user:
                is_correct_user_s = True
        else:
            is_correct_user_s = True

        # if plugin is feedback then record the data into report
        if ptype == 'feedback' and is_correct_user_s:
            if not (prev_user in anons):
                anons[prev_user] = f"user{cnt}"
                cnt += 1
            anonuser = anons[prev_user]
            correct = json.loads(answer.content)['correct']

            # if we need it in right/wrong format
            if correct:
                answer_result = 'right'
            else:
                answer_result = 'wrong'

            answer_content = json.loads(answer.content)['sentence']
            feedback_content = json.loads(answer.content)['feedback']
            # sel_opt = json.loads(prev_ans.content)[] TODO: when selected option is included inb the fb-plugin add this

            if pt_dt != None:
                tasksecs = (prev_ans.answered_on - pt_dt).total_seconds()   # get time subtracted by previous
            else:
                tasksecs = 0.0

            fbsecs = (answer.answered_on - prev_ans.answered_on).total_seconds()

            if (hide_names):
                shown_user = anonuser
            else:
                shown_user = prev_user.name

            # IF-CONDITION temporary for the sake of excluding the first sample item
            if exclude_first and pt_dt:   # TODO: replace with more stable system
                if printname and not hide_names:
                    results.append([prev_user.real_name,
                                    shown_user,
                                    answer_result,
                                    answer_content,
                                    feedback_content,
                                    str(round(tasksecs, 1)),
                                    str(round(fbsecs, 1))])
                else:
                    results.append([shown_user,
                                answer_result,
                                answer_content,
                                feedback_content,
                                str(round(tasksecs, 1)),
                                str(round(fbsecs, 1))])

        pt_dt = prev_ans.answered_on
        prev_ans = answer
        prev_user = user
        temp_bool = not temp_bool       # TODO: remove

    return results



# Route to feedback report  TODO: change path name
@feedback.route("/test/<path:doc_path>")
def test(doc_path):
    verify_logged_in()
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)    #return task from URL

    all_id = get_documents_in_folder(d.location)    # finds all doc in the folder

    verify_teacher_access(d)
    pars = d.document.get_dereferenced_paragraphs()     # get dereferenced paragraphs from document
    task_ids, _, _ = find_task_ids(pars)                # find task ids from the derefenced paragraphs

    #filtered_task_ids = filterPlugin(task_ids, 'all')  #filteröi muut paitsi feedback
    name = get_option(request, 'name', 'both')
    hidename = False
    fullname = False
    format = get_option(request, 'format', 'excel')
    validity = get_option(request, 'valid', 'all')
    exp_answers = get_option(request, 'answers', 'all')
    user = get_option(request, 'user', 'none')
    scope = get_option(request, 'scope', 'task')
    list_of_users = user.split(",")

    # Booleans for how to show user names
    if name == 'anonymous':
        hidename = True
    elif name == 'both':
        fullname = True

    # dialect names for the csv-writer delimitors
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

    # Period from which to take results
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

    answers = []

    if fullname and not hidename:
        answers.append(['Full Name','Username', 'Result', 'Item', 'Selected option', 'Feedback', 'Time spent on item(sec)', 'Time spent on feedback(sec)'])
    else:
        answers.append(['Username', 'Result', 'Item', 'Selected option', 'Feedback', 'Time spent on item(sec)', 'Time spent on feedback(sec)'])

    #get answers for the task ids with the proper parameters

    if scope == 'task':
        answers += get_all_feedback_answers(task_ids, hidename, fullname, validity, exp_answers, list_of_users, period_from, period_to)
    elif scope == 'test':
        for id in all_id:
            pars2 = id.document.get_dereferenced_paragraphs()   # TODO: rename
            task_ids2,_ ,_ = find_task_ids(pars2)   # TODO: rename
            answers += get_all_feedback_answers(task_ids2, hidename, fullname, validity, exp_answers, list_of_users, period_from, period_to)
            answers.append([])

    return csv_response(answers, dialect)



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