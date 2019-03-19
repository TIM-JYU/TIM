import json
import math
import csv
import os
from io import StringIO
from typing import List, Optional, Dict, Tuple, Iterable

from flask import Blueprint

from timApp.user.user import Consent, User
from timApp.util.flask.responsehelper import csv_response
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
                             hide_names: bool,
                             printname: bool,
                             sort: str,
                             period_from: datetime,
                             period_to: datetime) -> List[str]:
    """Gets all answers for "Dynamic Assessment" -typed tasks
    :param task_ids: The ids of the tasks needed in report.
    :param usergroup:
    :param hide_names:
    :param printname:
    :param sort: The sorting needed in output, ie. 'username'.
    :param print_opt:
    :param period_from: The minimum answering time for answers.
    :param period_to: The maximum answering time for answers.
    :param consent:
    :return:
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

    #pt_dt = datetime(1, 1, 1, tzinfo='UTC')    #muistiinmenevä tehtävän tekoaiku
    pt_dt = None    #Previous Datetime of previous answer
    prev_ans = None     #Previous Answer
    prev_user = None     #Previous User
    temp_bool = True    #Temporary answer combination condition, now it is every even
    exclude_first = True #Temporary setting for excluding the first

    #Back to string format
    results = []

    #Initialize stringIO and csv-writer for it
    writerIO = StringIO()
    writer = csv.writer(writerIO, delimiter=";", lineterminator=os.linesep)

    #First line are the headers
    if (printname and not hide_names):
        writer.writerow(['Full Name','Username', 'Result', 'Answer', 'Feedback', 'Time spent on item', 'Time spent on feedback'])
        results.append(['Full Name','Username', 'Result', 'Answer', 'Feedback', 'Time spent on item', 'Time spent on feedback'])
    else:
        writer.writerow(['Username', 'Result', 'Answer', 'Feedback','Time spent on item','Time spent on feedback'])
        results.append(['Username', 'Result', 'Answer', 'Feedback', 'Time spent on item', 'Time spent on feedback'])

    #Dictionary and counting for anons
    anons = {}
    cnt = 0

    #FOR STARTS FROM HERE
    for answer, user, n in qq:  #TODO: back to the string-format

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

        if prev_user == None: prev_user = user
        if prev_ans == None: prev_ans = answer
        if temp_bool:
            if not (prev_user in anons):
                anons[prev_user] = f"user{cnt}"
                cnt += 1
            anonuser = anons[prev_user]
            answer_content = json.loads(prev_ans.content) # .get(prev_ans.content) #json.loads(prev_ans.content)
            feedback_content = json.loads(answer.content)  #json.loads(answer.content)

            if pt_dt != None:
                tasksecs = (prev_ans.answered_on - pt_dt).total_seconds()
            else:
                tasksecs = 0.0

            fbsecs = (answer.answered_on - prev_ans.answered_on).total_seconds()

            if (hide_names):
                shown_user = anonuser
            else:
                shown_user = prev_user.name

            if exclude_first and pt_dt != None:   #IF-CONDITION temporary for the sake of excluding the first sample item
                if printname and not hide_names:
                    """
                    writer.writerow([prev_user.real_name]
                                + [shown_user]
                                + ['WRONG!!!']
                                + [answer_content]
                                + [feedback_content]
                                + [str(math.floor(tasksecs))]
                                + [str(math.floor(fbsecs))])
                                """
                    results.append([prev_user.real_name]
                                + [shown_user]
                                + ['WRONG!!!']
                                + [answer_content]
                                + [feedback_content]
                                + [str(math.floor(tasksecs))]
                                + [str(math.floor(fbsecs))])
                else:
                    """
                    writer.writerow([shown_user]
                                + ['WRONG!!!']
                                + [answer_content]
                                + [feedback_content]
                                + [str(math.floor(tasksecs))]
                                + [str(math.floor(fbsecs))])
                                """
                    results.append([shown_user]
                                + ['WRONG!!!']
                                + [answer_content]
                                + [feedback_content]
                                + [str(math.floor(tasksecs))]
                                + [str(math.floor(fbsecs))])

        pt_dt = prev_ans.answered_on
        prev_ans = answer
        temp_bool = not temp_bool

    return results


# Route to test feedback output at feedback/test
@feedback.route("/test")
def test():
    #taskids = Answer.query.add_column("task_id").all() TODO: a query from database would help testing

    period1 = datetime.min
    period2 = datetime.max

    # Filtering now works so this will not return any answers
    answers0 = get_all_feedback_answers([], True, True, 'username', period1, period2)

    # For choosing specific tasks for the report a list of tasks is given
    # TODO: check real call to see how task_ids from different pages can be called
    nro1 = 19
    taskid1 = TaskId(nro1, "sample_item")  # generating TaskIds - (pageid int, taskname string)
    taskid2 = TaskId(nro1, "item1")
    taskid3 = TaskId(nro1, "feedback1")
    taskid4 = TaskId(nro1, "item2")
    taskid5 = TaskId(nro1, "feedback2")
    taskids_test = [taskid1, taskid2, taskid3, taskid4, taskid5]
    ids_as_string1 = task_ids_to_strlist(taskids_test)
    answers1 = get_all_feedback_answers(taskids_test, False, True,
                                        'username', period1, period2)

    # Testing with taskIds from two documents
    nro2 = 22
    taskid21 = TaskId(nro2, "sample_item")
    taskid22 = TaskId(nro2, "item1")
    taskid23 = TaskId(nro2, "feedback1")
    taskid24 = TaskId(nro2, "item2")
    taskid25 = TaskId(nro2, "feedback2")
    taskids_test2 = [taskid1, taskid2, taskid3, taskid4, taskid5, taskid21, taskid22, taskid23, taskid24, taskid25]
    ids_as_string2 = task_ids_to_strlist(taskids_test2)
    answers2 = get_all_feedback_answers(taskids_test2, True, True,
                                        'username', period1, period2)

    # First returns empty, then from document nro1 and then document nro2 (change above if different)
    return csv_response(answers1, 'excel')
    """ return json_response({'answers0': answers0,
                          'answers1': answers1,
                          'answers2': answers2})"""
