import json
from typing import List, Tuple, Iterable
import dateutil.relativedelta

from flask import Blueprint, request

from timApp.auth.sessioninfo import get_current_user_object
from timApp.plugin.plugin import Plugin
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
from timApp.auth.accesshelper import verify_logged_in, verify_teacher_access
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
                             users: List[str],
                             period_from: datetime,
                             period_to: datetime):
    """
    Get feedback answer results.
    :param task_ids: List of task ids.
    :param hide_names: Parameter for whether to show users anonymously.
    :param printname: Parameter for whether to show full name.
    :param valid: Parameter for validity filter; "0", "1", "all".
    :param exp_answers: Parameter for filtering users.
    :param users: List of visible users with selected user always first.
    :param period_from: The minimum answering time for answers.
    :param period_to: The maximum answering time for answers.
    :return:
    """

    # Query from answer-table for the given time period and TaskId:s.
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

    # Also joins with user table to get user information.
    q = q.join(User, Answer.users)

    # Sorts the answers first with user then by time - for a report of whole test result by one user.
    q = q.order_by(User.name, Answer.answered_on)

    # "q" with Answer and User data.
    q = q.with_entities(Answer, User)

    # Makes q query an iterable qq for for-loop.
    qq: Iterable[Tuple[Answer, User]] = q

    return compile_csv(qq, printname, hide_names, exp_answers, users)


def compile_csv(qq: Iterable[Tuple[Answer, User]], printname: bool, hide_names: bool, exp_answers: str, s_user: [str]):
    """
    Compile data into more csv friendly form.
    :param qq: List of Tuples containing Answers and Users.
    :param printname: Parameter whether to show full name.
    :param hide_names: Parameter whether to show users anonymously.
    :param exp_answers: Parameter for filtering users.
    :param s_user: Visible users with first one always being the selected user.
    :return: List of task results.
    """

    pt_dt = None    # Previous Datetime of previous answer.
    prev_ans = None     # Previous Answer.
    prev_user = None     # Previous User.
    exclude_first = True  # Temporary setting for excluding the first.

    results = []

    # Dictionary and counting for anons.
    anons = {}
    cnt = 0

    for answer, user in qq:

        if prev_user != user:   # Resets if previous is different user.
            prev_user = None
            prev_ans = None
            pt_dt = None
            temp_bool = True

        if prev_user is None:
            prev_user = user
        if prev_ans is None:
            prev_ans = answer

        # Find type of the plugin.

        answer_task_id = answer.task_id
        p = Plugin.from_task_id(answer_task_id, user=get_current_user_object())
        ptype = p.type

        # Boolean to check whether to filter out current user by name.
        is_correct_user_s = False
        if exp_answers == 'selected':
            is_correct_user_s = user.name == s_user[0]
        elif exp_answers == 'visible':
            if user.name in s_user:
                is_correct_user_s = True
        else:
            is_correct_user_s = True

        # If plugin is feedback then record the data into report.
        if ptype == 'feedback' and is_correct_user_s:
            if not (prev_user in anons):
                anons[prev_user] = f"user{cnt}"
                cnt += 1
            anonuser = anons[prev_user]
            correct = json.loads(answer.content)['correct']

            # If we need it in right/wrong format.
            if correct:
                answer_result = 'right'
            else:
                answer_result = 'wrong'

            json_content = json.loads(answer.content)

            sel_opt_content = json_content['answer']
            item_content = json_content['sentence']
            feedback_content = json_content['feedback']

            if pt_dt != None:
                tasksecs = (prev_ans.answered_on - pt_dt).total_seconds()   # Get time subtracted by previous.
            else:
                tasksecs = 0.0

            fbsecs = (answer.answered_on - prev_ans.answered_on).total_seconds()

            if hide_names:
                shown_user = anonuser
            else:
                shown_user = prev_user.name

            # IF-CONDITION temporary for the sake of excluding the first sample item.
            if exclude_first and pt_dt:   # TODO: replace with more stable system
                if printname and not hide_names:
                    results.append([prev_user.real_name,
                                    shown_user,
                                    answer_result,
                                    item_content,
                                    sel_opt_content,
                                    feedback_content,
                                    str(round(tasksecs, 1)),
                                    str(round(fbsecs, 1))])
                else:
                    results.append([shown_user,
                                answer_result,
                                item_content,
                                sel_opt_content,
                                feedback_content,
                                str(round(tasksecs, 1)),
                                str(round(fbsecs, 1))])

        pt_dt = prev_ans.answered_on
        prev_ans = answer
        prev_user = user

    return results



# TODO: change path name
@feedback.route("/test/<path:doc_path>")
def test(doc_path):
    """
    Route to Feedback report.
    :param doc_path: URL for the document.
    :return: report in a CSV-form.
    """

    verify_logged_in()
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)    # Return task from URL.

    all_id = get_documents_in_folder(d.location)    # Finds all doc in the folder.

    verify_teacher_access(d)
    pars = d.document.get_dereferenced_paragraphs()     # Get dereferenced paragraphs from document.
    task_ids, _, _ = find_task_ids(pars)                # Find task ids from the derefenced paragraphs.

    name = get_option(request, 'name', 'both')
    hidename = False
    fullname = False
    format = get_option(request, 'format', 'excel')
    validity = get_option(request, 'valid', 'all')
    exp_answers = get_option(request, 'answers', 'all')
    user = get_option(request, 'user', 'none')
    scope = get_option(request, 'scope', 'task')
    list_of_users = user.split(",")

    # Booleans for how to show user names.
    if name == 'anonymous':
        hidename = True
    elif name == 'both':
        fullname = True

    # Dialect names for the csv-writer delimitors.
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

    # Creates list of document ids from task ids.
    doc_ids = set()
    for tid in task_ids:
        doc_ids.add(tid.doc_id)

    #
    since_last_key = task_ids[0].doc_task
    if len(task_ids) > 1:
        since_last_key = str(next(d for d in doc_ids))
        if len(doc_ids) > 1:
            since_last_key = None

    # Period from which to take results.
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
        answers.append(['Full Name',
                        'Username',
                        'Result',
                        'Item',
                        'Selected option',
                        'Feedback', 'Time spent on item(sec)',
                        'Time spent on feedback(sec)'])
    else:
        answers.append(['Username',
                        'Result',
                        'Item',
                        'Selected option',
                        'Feedback',
                        'Time spent on item(sec)',
                        'Time spent on feedback(sec)'])

    # Get answers for the task ids with the proper parameters.

    if scope == 'task':
        answers += get_all_feedback_answers(task_ids,
                                            hidename,
                                            fullname,
                                            validity,
                                            exp_answers,
                                            list_of_users,
                                            period_from,
                                            period_to)
    elif scope == 'test':
        for did in all_id:
            did_pars = did.document.get_dereferenced_paragraphs()
            task_dids, _ , _ = find_task_ids(did_pars)
            answers += get_all_feedback_answers(task_dids,
                                                hidename,
                                                fullname,
                                                validity,
                                                exp_answers,
                                                list_of_users,
                                                period_from,
                                                period_to)
            answers.append([])

    return csv_response(answers, dialect)
