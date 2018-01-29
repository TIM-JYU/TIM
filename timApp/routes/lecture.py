import json
import threading
import time
from datetime import timezone, datetime
from random import randrange
from typing import List, Optional

import dateutil.parser
from flask import Blueprint, render_template
from flask import Response
from flask import abort
from flask import current_app
from flask import request
from flask import session
from sqlalchemy.orm.exc import NoResultFound

from timApp.accesshelper import verify_ownership, get_doc_or_abort
from timApp.common import has_ownership, \
    get_user_settings
from timApp.documentmodel.randutils import hashfunc
from timApp.requesthelper import get_option, verify_json_params
from timApp.responsehelper import json_response, ok_response, empty_response
from timApp.routes.login import log_in_as_anonymous
from timApp.routes.qst import get_question_data_from_document, delete_key, create_points_table, \
    calculate_points_from_json_answer, calculate_points
from timApp.sessioninfo import get_current_user_id, logged_in, get_current_user_name, get_current_user_object, \
    current_user_in_lecture
from timApp.tim_app import app
from timApp.timdb.models.askedjson import get_asked_json_by_hash, AskedJson
from timApp.timdb.models.askedquestion import AskedQuestion, get_asked_question
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.lecture import Lecture
from timApp.timdb.models.lectureanswer import LectureAnswer, get_totals
from timApp.timdb.models.message import Message
from timApp.timdb.models.user import User
from timApp.timdb.tempdb_models import TempDb, Runningquestion
from timApp.timdb.tim_models import db, Question

lecture_routes = Blueprint('lecture',
                           __name__,
                           url_prefix='')


def is_lecturer_of(l: Lecture):
    return l.lecturer == get_current_user_id()


def verify_is_lecturer(l: Lecture):
    if not is_lecturer_of(l):
        abort(400, 'Only lecturer can perform this action.')


@lecture_routes.route('/getLectureInfo')
def get_lecture_info():
    """Route to get info from lectures.

    Gives answers, and messages and other necessary info.

    """
    lecture = get_lecture_from_request(check_access=False)
    messages = lecture.messages.order_by(Message.timestamp.asc()).all()
    is_lecturer = is_lecturer_of(lecture)
    current_user = get_current_user_id()
    lecture_questions: List[AskedQuestion] = lecture.asked_questions.all()

    if is_lecturer:
        answers = [a for q in lecture_questions for a in q.answers.all()]
        answerers = list({a.user for a in answers})
    else:
        answers = [a for q in lecture_questions for a in q.answers.filter_by(user_id=current_user)]
        answerers = [get_current_user_object()]

    return json_response(
        {
            "answerers": answerers,
            "answers": answers,
            "isLecturer": is_lecturer,
            "messages": messages,
            "questions": lecture_questions,
        })


@lecture_routes.route('/getLectureAnswerTotals/<int:lecture_id>')
def get_lecture_answer_totals(lecture_id):
    is_lecturer = is_lecturer_of(Lecture.find_by_id(lecture_id))
    results = get_totals(lecture_id, None if is_lecturer else get_current_user_id())
    sum_field_name = get_option(request, 'sum_field_name', 'sum')
    count_field_name = get_option(request, 'count_field_name', 'count')

    def generate_text():
        for a in results:
            yield f'{a["name"]};{sum_field_name};{a["sum"]}\n'
        yield '\n'
        for a in results:
            yield f'{a["name"]};{count_field_name};{a["count"]}\n'
    return Response(generate_text(), mimetype='text/plain')


@lecture_routes.route('/getAllMessages')
def get_all_messages():
    """Route to get all the messages from some lecture.
    """
    lecture = get_lecture_from_request(check_access=False)
    messages = lecture.messages.order_by(Message.timestamp.asc()).all()
    return json_response(messages)


@lecture_routes.route('/getUpdates')
def get_updates():
    # taketime("before update")
    ret = do_get_updates(request)
    # taketime("after update")
    return ret


EXTRA_FIELD_NAME = "extra"


def do_get_updates(request):
    """Gets updates from some lecture.

    Checks updates in 1 second frequently and answers if there is updates.

    """
    if not request.args.get('c'):
        abort(400, "Bad request")
    client_last_id = int(request.args.get('c'))  # client_message_id'))
    current_question_id = None
    current_points_id = None
    if 'i' in request.args:
        current_question_id = int(request.args.get('i'))  # current_question_id'))
    if 'p' in request.args:
        current_points_id = int(request.args.get('p'))  # current_points_id'))

    use_wall = get_option(request, 'm', False)  # 'get_messages'
    session['use_wall'] = use_wall
    use_questions = get_option(request, 'q', False)  # 'get_questions'
    session['use_questions'] = use_questions

    tempdb = get_tempdb()
    step = 0
    lecture = get_current_lecture()

    doc_id = request.args.get("d")  # "doc_id"
    if doc_id:
        doc_id = int(doc_id)
    if not lecture:
        return get_running_lectures(doc_id)
    lecture_id = lecture.lecture_id
    if not lecture.is_running:
        empty_lecture(lecture)
        db.session.commit()
        return get_running_lectures(doc_id)

    list_of_new_messages = []

    lecturers = []
    students = []
    current_user = get_current_user_id()
    user_name = get_current_user_name()

    time_now = str(datetime.now(timezone.utc).strftime("%H:%M:%S"))
    tempdb.useractivity.update_or_add_activity(lecture_id, current_user, time_now)

    options = lecture.options_parsed
    teacher_poll = options.get("teacher_poll", "")
    teacher_poll = teacher_poll.split(";")
    poll_interval_ms = 4000
    long_poll = False
    # noinspection PyBroadException
    try:
        poll_interval_ms = int(options.get("poll_interval", 4))*1000
        long_poll = bool(options.get("long_poll", False))
    except:
        pass

    # noinspection PyBroadException
    try:
        poll_interval_t_ms = int(options.get("poll_interval_t", 1))*1000
        long_poll_t = bool(options.get("long_poll_t", False))
    except:
        pass

    poll_interval_ms += randrange(-100, 500)

    if teacher_poll:
        # noinspection PyBroadException
        try:
            if teacher_poll.index(user_name) >= 0:
                poll_interval_ms = poll_interval_t_ms
                long_poll = long_poll_t
        except:
            pass

    is_lecturer = is_lecturer_of(lecture)

    lecture_ending = 100
    base_resp = None
    # Jos poistaa tämän while loopin, muuttuu long pollista perinteiseksi polliksi
    while step <= 10:
        lecture_ending = check_if_lecture_is_ending(lecture)
        if is_lecturer:
            lecturers, students = get_lecture_users(tempdb, lecture)
            poll_interval_ms = poll_interval_t_ms
            long_poll = long_poll_t
        # Gets new messages if the wall is in use.
        if use_wall:
            list_of_new_messages = lecture.messages.filter(Message.msg_id > client_last_id).order_by(
                Message.msg_id.asc()).all()

        base_resp = {
            "msgs": list_of_new_messages,
            "lectureEnding": lecture_ending,
            "lectureId": lecture_id,
            "lecturers": lecturers,
            "ms": poll_interval_ms,
            "students": students,
        }

        # Check if current question is still running and user hasn't already answered on it on another tab
        # Return also questions new end time if it is extended

        if current_question_id:
            resp = {
                **base_resp,
                EXTRA_FIELD_NAME: {
                    "new_end_time": None,
                }
            }

            question = tempdb.runningquestions.get_running_question_by_id(current_question_id)
            already_answered = tempdb.usersanswered.has_user_info(current_question_id, current_user)
            if question and not already_answered:
                already_extended = tempdb.usersextended.has_user_info(current_question_id, current_user)
                if not already_extended:
                    tempdb.usersextended.add_user_info(lecture_id, current_question_id, current_user)
                    # Return this is question has been extended
                    resp[EXTRA_FIELD_NAME]['new_end_time'] = question.end_time
                    return json_response(resp)
            else:
                # Return this if question has ended or user has answered to it
                return json_response(resp)

        if current_points_id:
            resp = {
                **base_resp,
                EXTRA_FIELD_NAME: {
                    "points_closed": True,
                }
            }
            already_closed = tempdb.pointsclosed.has_user_info(current_points_id, current_user)
            if already_closed:
                return json_response(resp)

        # Gets new questions if the questions are in use.
        if use_questions:
            new_question = get_new_question(lecture_id, current_question_id, current_points_id)
            if new_question:
                return json_response({**base_resp, EXTRA_FIELD_NAME: new_question})

        if list_of_new_messages:
            return json_response(base_resp)

        if not long_poll or current_app.config['TESTING']:
            # Don't loop when testing
            break
        # For long poll wait 1 sek before new check.
        time.sleep(1)
        step += 1

    if lecture_ending != 100 or lecturers or students:
        return json_response(base_resp)

    return json_response({"ms": poll_interval_ms})  # no new updates


@lecture_routes.route('/getQuestionManually')
def get_question_manually():
    """Route to use to get question manually (instead of getting question in /getUpdates)."""
    lecture = get_current_lecture_or_abort()
    new_question = get_new_question(lecture.lecture_id, None, None, True)
    return json_response(new_question)


def get_new_question(lecture_id, current_question_id=None, current_points_id=None, force=False):
    """
    :param current_points_id: TODO: what is this?
    :param current_question_id: The id of the current question.
    :param lecture_id: lecture to get running questions from
    :param force: Return question, even if it already has been shown to user
    :return: None if no questions are running
             dict with data of new question if there is a question running and user hasn't answered to that question.
             {'type': 'already_answered'} if there is a question running and user has answered to that.
    """
    tempdb = get_tempdb()
    current_user = get_current_user_id()
    question = tempdb.runningquestions.get_lectures_running_questions(lecture_id)
    if question:
        question = question[0]
        asked_id = question.asked_id
        already_shown = tempdb.usersshown.has_user_info(asked_id, current_user)
        already_answered = tempdb.usersanswered.has_user_info(asked_id, current_user)
        if already_answered:
            if force:
                return {'type': 'already_answered'}
            else:
                return None
        if (not already_shown or force) or (asked_id != current_question_id):
            q = get_asked_question(asked_id)
            answer = q.answers.filter_by(user_id=current_user).first()
            tempdb.usersshown.add_user_info(lecture_id, asked_id, current_user)
            tempdb.usersextended.add_user_info(lecture_id, asked_id, current_user)
            return {'type': 'answer', 'data': answer} if answer else {'type': 'question', 'data': q}
    else:
        question_to_show_points = tempdb.showpoints.get_currently_shown_points(lecture_id)
        if question_to_show_points:
            asked_id = question_to_show_points[0].asked_id
            already_shown = tempdb.pointsshown.has_user_info(asked_id, current_user)
            already_closed = tempdb.pointsclosed.has_user_info(asked_id, current_user)
            if already_closed:
                if force:
                    tempdb.pointsclosed.delete_user_info(lecture_id, asked_id, current_user)
                else:
                    return None
            if not (already_shown or force) or (asked_id != current_points_id):
                question = get_asked_question(asked_id)
                tempdb.pointsshown.add_user_info(lecture_id, asked_id, current_user)
                answer = question.answers.filter_by(user_id=current_user).first()
                if answer:
                    return {'type': 'result', 'data': answer}
        return None


def check_if_lecture_is_ending(lecture: Lecture):
    """Checks if the lecture is about to end. 1 -> ends in 1 min. 5 -> ends in 5 min. 100 -> goes on atleast for 5 mins.

    :param lecture: The lecture object.
    :return:

    """
    lecture_ending = 100
    if is_lecturer_of(lecture):
        time_now = datetime.now(timezone.utc)
        ending_time = lecture.end_time
        time_left = ending_time - time_now
        if time_left.total_seconds() <= 60:
            return 1
        elif time_left.total_seconds() <= 60 * 5:
            return 5
    return lecture_ending


@lecture_routes.route('/sendMessage', methods=['POST'])
def send_message():
    """Route to add message to database."""
    new_message = request.args.get("message")
    lecture = get_current_lecture_or_abort()
    msg = Message(message=new_message, user_id=get_current_user_id())
    lecture.messages.append(msg)
    db.session.commit()
    return json_response(msg)


def get_lecture_session_data():
    for k in ('use_wall', 'use_questions'):
        if session.get(k) is None:
            session[k] = True
    return {
        'useWall': session['use_wall'],
        'useQuestions': session['use_questions'],
    }


def lecture_dict(lecture: Lecture):
    tempdb = get_tempdb()
    is_lecturer = is_lecturer_of(lecture)
    lecturers, students = get_lecture_users(tempdb, lecture) if is_lecturer else ([], [])
    return {
        "lecture": lecture,
        "isInLecture": current_user_in_lecture(),
        "isLecturer": is_lecturer,
        "lecturers": lecturers,
        "students": students,
        **get_lecture_session_data(),
    }


@lecture_routes.route('/checkLecture', methods=['GET'])
def check_lecture():
    """Route to check if the current user is in some lecture in specific document."""
    lectures = get_current_user_object().lectures.all()
    lecture = lectures[0] if lectures else None

    if lecture:
        if lecture.is_running:
            return json_response(lecture_dict(lecture))
        else:
            leave_lecture_function(lecture)
            empty_lecture(lecture)
            db.session.commit()
    doc_id = request.args.get('doc_id')
    if doc_id is not None:
        return get_running_lectures(int(doc_id))
    else:
        return empty_response()


@lecture_routes.route("/startFutureLecture", methods=['POST'])
def start_future_lecture():
    if not request.args.get('lecture_code') or not request.args.get("doc_id"):
        abort(400)

    lecture_code = request.args.get('lecture_code')
    doc_id = int(request.args.get("doc_id"))
    d = get_doc_or_abort(doc_id)
    verify_ownership(d)
    lecture = Lecture.find_by_code(lecture_code, doc_id)
    time_now = datetime.now(timezone.utc)
    lecture.start_time = time_now
    lecture.users.append(get_current_user_object())
    db.session.commit()
    return json_response(lecture_dict(lecture))


@lecture_routes.route('/getAllLecturesFromDocument', methods=['GET'])
def get_all_lectures():
    if not request.args.get('doc_id'):
        abort(400)

    doc_id = int(request.args.get('doc_id'))

    lectures = Lecture.get_all_in_document(doc_id)
    time_now = datetime.now(timezone.utc)
    current_lectures = []
    past_lectures = []
    future_lectures = []
    for lecture in lectures:
        if lecture.start_time <= time_now < lecture.end_time:
            current_lectures.append(lecture)
        elif lecture.end_time <= time_now:
            past_lectures.append(lecture)
        else:
            future_lectures.append(lecture)

    return json_response(
        {"currentLectures": current_lectures, "futureLectures": future_lectures, "pastLectures": past_lectures})


@lecture_routes.route('/showLectureInfo/<int:lecture_id>', methods=['GET'])
def show_lecture_info(lecture_id):
    lecture = Lecture.find_by_id(lecture_id)
    if not lecture:
        abort(400, 'Lecture not found')

    doc = DocEntry.find_by_id(lecture.doc_id)
    lectures = get_current_user_object().lectures.all()
    settings = get_user_settings()
    return render_template("lectureInfo.html",
                           item=doc,
                           lecture=lecture,
                           in_lecture=len(lectures) > 0,
                           settings=settings,
                           translations=doc.translations)


@lecture_routes.route('/showLectureInfoGivenName')
def show_lecture_info_given_name():
    if 'lecture_id' in request.args:
        lecture = Lecture.find_by_id(int(request.args.get('lecture_id')))
    else:
        lecture = Lecture.find_by_code(request.args.get('lecture_code'), int(request.args.get('doc_id')))
    if not lecture:
        abort(400)

    return json_response(lecture.to_json(show_password=is_lecturer_of(lecture)))


@lecture_routes.route('/lectureNeedsPassword')
def lecture_needs_password():
    if 'lecture_id' in request.args:
        lecture = Lecture.find_by_id(int(request.args.get('lecture_id')))
    else:
        lecture = Lecture.find_by_code(request.args.get('lecture_code'), int(request.args.get('doc_id')))
    if not lecture:
        abort(400)
    return json_response(lecture.password != '')


def get_lecture_users(tempdb, lecture: Lecture):
    lecturers = []
    students = []

    activity = tempdb.useractivity.get_all_user_activity(lecture.lecture_id)

    for user in activity:
        user_id = user.user_id
        active = user.active
        person = {
            "user": User.get_by_id(user_id),
            "active": active,
        }
        if lecture.lecturer == user_id:
            lecturers.append(person)
        else:
            students.append(person)

    return lecturers, students


def get_running_lectures(doc_id=None):
    """Gets all running and future lectures.

    :param doc_id: The document id for which to get lectures.

    """
    time_now = datetime.now(timezone.utc)
    list_of_lectures = []
    is_lecturer = False
    if doc_id:
        list_of_lectures = Lecture.get_all_in_document(doc_id, time_now)
        d = get_doc_or_abort(doc_id)
        is_lecturer = bool(has_ownership(d))
    current_lectures = []
    future_lectures = []
    for lecture in list_of_lectures:
        if lecture.start_time <= time_now < lecture.end_time:
            current_lectures.append(lecture)
        else:
            future_lectures.append(lecture)
    return json_response(
        {
            "isLecturer": is_lecturer,
            "lectures": current_lectures,
            "futureLectures": future_lectures,
        })


@lecture_routes.route('/createLecture', methods=['POST'])
def create_lecture():
    doc_id, start_time, end_time, lecture_code = verify_json_params('doc_id', 'start_time', 'end_time', 'lecture_code')
    start_time = dateutil.parser.parse(start_time)
    end_time = dateutil.parser.parse(end_time)
    lecture_id, password, options = verify_json_params('lecture_id', 'password', 'options', require=False)
    d = get_doc_or_abort(doc_id)
    verify_ownership(d)

    if not options:
        options = {}

    if not password:
        password = ""
    current_user = get_current_user_id()
    lec = Lecture.find_by_code(lecture_code, doc_id)
    if lec and not lecture_id:
        abort(400, "Can't create two or more lectures with the same name to the same document.")

    options = json.dumps(options)
    if lecture_id is None:
        lecture = Lecture(doc_id=doc_id, lecturer=current_user)
        db.session.add(lecture)
    else:
        lecture = Lecture.find_by_id(lecture_id)
        if not lecture:
            return abort(404)
    lecture.start_time = start_time
    lecture.end_time = end_time
    lecture.password = password
    lecture.lecture_code = lecture_code
    lecture.options = options

    current_time = datetime.now(timezone.utc)

    if start_time <= current_time <= end_time:
        lecture.users.append(get_current_user_object())
    db.session.commit()
    return json_response(lecture)


def empty_lecture(lec: Lecture):
    lec.users = []
    clean_dictionaries_by_lecture(lec.lecture_id)


@lecture_routes.route('/endLecture', methods=['POST'])
def end_lecture():
    lecture = get_lecture_from_request()
    now = datetime.now(timezone.utc)
    lecture.end_time = now
    empty_lecture(lecture)
    db.session.commit()
    return get_running_lectures(lecture.doc_id)


def clean_dictionaries_by_lecture(lecture_id):
    """Cleans data from lecture that isn't running anymore.

    :param lecture_id: The lecture id.

    """
    tempdb = get_tempdb()
    tempdb.runningquestions.delete_lectures_running_questions(lecture_id)
    tempdb.usersshown.delete_all_from_lecture(lecture_id)
    tempdb.usersextended.delete_all_from_lecture(lecture_id)
    tempdb.useractivity.delete_lecture_activity(lecture_id)
    tempdb.newanswers.delete_lecture_answers(lecture_id)
    tempdb.showpoints.stop_showing_points(lecture_id)
    tempdb.pointsshown.delete_all_from_lecture(lecture_id)


@lecture_routes.route('/extendLecture', methods=['POST'])
def extend_lecture():
    new_end_time = request.args.get("new_end_time")
    if not new_end_time:
        abort(400)
    lecture = get_lecture_from_request()
    lecture.end_time = new_end_time
    db.session.commit()
    return ok_response()


@lecture_routes.route('/deleteLecture', methods=['POST'])
def delete_lecture():
    lecture = get_lecture_from_request()
    empty_lecture(lecture)
    Message.query.filter_by(lecture_id=lecture.lecture_id).delete()
    LectureAnswer.query.filter_by(lecture_id=lecture.lecture_id).delete()
    AskedQuestion.query.filter_by(lecture_id=lecture.lecture_id).delete()
    db.session.delete(lecture)
    db.session.commit()

    return get_running_lectures(lecture.doc_id)


def get_lecture_from_request(check_access=True) -> Lecture:
    lec_id_str = request.args.get("lecture_id")
    if not lec_id_str:
        abort(400)
    lecture_id = int(lec_id_str)
    lecture = Lecture.find_by_id(lecture_id)
    if not lecture:
        abort(404)
    if check_access:
        d = get_doc_or_abort(lecture.doc_id)
        verify_ownership(d)
    return lecture


@lecture_routes.route('/joinLecture', methods=['POST'])
def join_lecture():
    """Route to join lecture.

    Checks that the given password is correct.

    """
    if not request.args.get("doc_id") or not request.args.get("lecture_code"):
        abort(400, "Missing parameters")
    tempdb = get_tempdb()
    doc_id = int(request.args.get("doc_id"))
    lecture_code = request.args.get("lecture_code")
    password_quess = request.args.get("password_quess")
    lecture = Lecture.find_by_code(lecture_code, doc_id)
    if not lecture:
        return abort(404, 'Lecture with this code was not found.')
    lecture_id = lecture.lecture_id
    current_user = get_current_user_id()

    lecture_ended = not lecture.is_running

    # TODO Allow lecturer always join, even if the lecture is full
    lecture_full = lecture.is_full

    correct_password = True
    if lecture.password != password_quess:
        correct_password = False

    lectures = get_current_user_object().lectures.all()
    if not lecture_ended and not lecture_full and correct_password:
        if not logged_in():
            anon_user = log_in_as_anonymous(session)  # TODO check this if g.user should be reset
            current_user = anon_user.id
        if lectures:
            leave_lecture_function(lectures[0])
        lecture.users.append(get_current_user_object())
        db.session.commit()

        time_now = str(datetime.now(timezone.utc).strftime("%H:%M:%S"))
        tempdb.useractivity.update_or_add_activity(lecture_id, current_user, time_now)

        session['in_lecture'] = [lecture_id]

    return json_response(
        {
            "correctPassword": correct_password,
            **lecture_dict(lecture),
        })


@lecture_routes.route('/leaveLecture', methods=['POST'])
def leave_lecture():
    lecture_id = get_option(request, 'lecture_id', None, cast=int)
    if not lecture_id:
        abort(400)
    lecture = get_lecture_from_request(check_access=False)
    leave_lecture_function(lecture)
    db.session.commit()
    return ok_response()


def leave_lecture_function(lecture: Lecture):
    lecture_id = lecture.lecture_id
    if 'in_lecture' in session:
        lecture_list = session['in_lecture']
        if lecture_id in lecture_list:
            lecture_list.remove(lecture_id)
        session['in_lecture'] = lecture_list
    lecture.users.remove(get_current_user_object())


@lecture_routes.route("/extendQuestion", methods=['POST'])
def extend_question():
    asked_id = int(request.args.get('asked_id'))
    extend = int(request.args.get('extend'))

    tempdb = get_tempdb()
    tempdb.runningquestions.extend_question(asked_id, extend * 1000)

    return ok_response()


def get_current_lecture() -> Optional[Lecture]:
    u = get_current_user_object()
    lectures: List[Lecture] = u.lectures.all()
    if not lectures:
        return None
    if len(lectures) > 1:
        raise Exception(f'User {u.name} has joined to multiple lectures which should not be possible.')
    return lectures[0]


def get_current_lecture_or_abort() -> Lecture:
    lec = get_current_lecture()
    if not lec:
        return abort(400, 'Not joined to any lecture')
    return lec


@lecture_routes.route("/askQuestion", methods=['POST'])
def ask_question():
    if not request.args.get('doc_id') or not \
            (request.args.get('question_id') or request.args.get('asked_id') or request.args.get('par_id')):
        abort(400, "Bad request")
    doc_id = int(request.args.get('doc_id'))
    question_id = None
    asked_id = None
    par_id = None
    if request.args.get('question_id'):
        question_id = int(request.args.get('question_id'))
    elif request.args.get('asked_id'):
        asked_id = int(request.args.get('asked_id'))
    else:
        par_id = request.args.get('par_id')

    lecture = get_current_lecture_or_abort()
    verify_is_lecturer(lecture)
    lecture_id = lecture.lecture_id

    if question_id or par_id:
        if question_id:
            question = Question.query.get(question_id)  # Old version???
            question_json_str = question.questionjson
            markup = json.loads(question_json_str)
        else:
            d = get_doc_or_abort(doc_id)
            markup = get_question_data_from_document(d, par_id)
            question_json_str = json.dumps(markup.markup)
        question_hash = hashfunc(question_json_str)
        asked_json = get_asked_json_by_hash(question_hash)
        if not asked_json:
            asked_json = AskedJson(json=question_json_str, hash=question_hash)
        asked_time = datetime.now(timezone.utc)

        # Set points and expl as None because they're already contained in the JSON.
        # Only if /updatePoints is called, they are set.
        question = AskedQuestion(lecture_id=lecture_id, doc_id=doc_id, asked_time=asked_time, points=None, expl=None,
                           asked_json=asked_json)
        db.session.add(question)
        db.session.commit()
    elif asked_id:
        question = get_asked_question(asked_id)
        if not question:
            abort(404, 'Asked question not found.')
        lecture_id = question.lecture_id
        markup = question.asked_json.to_json()
    else:
        return abort(400, 'Missing parameters')

    question_timelimit = 0
    try:
        tl = markup.get("timeLimit", "0")
        if not tl:
            tl = "0"
        question_timelimit = int(tl)
    except:
        pass

    ask_time = int(time.time() * 1000)
    end_time = ask_time + question_timelimit * 1000
    thread_to_stop_question = threading.Thread(target=stop_question_from_running,
                                               args=(lecture_id, asked_id, question_timelimit, end_time))

    thread_to_stop_question.start()

    tempdb = get_tempdb()
    delete_question_temp_data(asked_id, lecture_id, tempdb)

    tempdb.runningquestions.add_running_question(lecture_id, asked_id, ask_time, end_time)

    return json_response(question)


def delete_question_temp_data(asked_id, lecture_id, tempdb):
    tempdb.runningquestions.delete_lectures_running_questions(lecture_id)
    tempdb.usersshown.delete_all_from_question(asked_id)
    tempdb.usersextended.delete_all_from_question(asked_id)
    tempdb.newanswers.delete_question_answers(asked_id)
    tempdb.showpoints.stop_showing_points(lecture_id)
    tempdb.pointsshown.delete_all_from_lecture(lecture_id)
    tempdb.pointsclosed.delete_all_from_lecture(lecture_id)


@lecture_routes.route('/showAnswerPoints', methods=['POST'])
def show_points():
    if 'asked_id' not in request.args:
        abort(400)
    lecture = get_current_lecture_or_abort()
    verify_is_lecturer(lecture)
    asked_id = int(request.args.get('asked_id'))
    lecture_id = lecture.lecture_id

    tempdb = get_tempdb()
    tempdb.showpoints.stop_showing_points(lecture_id)
    tempdb.showpoints.add_show_points(lecture_id, asked_id)

    current_question_id = None
    current_points_id = None
    if 'current_question_id' in request.args:
        current_question_id = int(request.args.get('current_question_id'))
    if 'current_points_id' in request.args:
        current_points_id = int(request.args.get('current_points_id'))
    new_question = get_new_question(lecture_id, current_question_id, current_points_id)
    if new_question is not None:
        return json_response(new_question)
    return empty_response()


@lecture_routes.route('/updatePoints/', methods=['POST'])
def update_question_points():
    """Route to get add question to database."""
    if 'asked_id' not in request.args or 'points' not in request.args:
        abort(400)
    asked_id = int(request.args.get('asked_id'))
    points = request.args.get('points')
    expl = request.args.get('expl')
    asked_question = get_asked_question(asked_id)
    verify_is_lecturer(asked_question.lecture)
    asked_question.points = points
    asked_question.expl = expl
    points_table = create_points_table(points)
    question_answers: List[LectureAnswer] = asked_question.answers.all()
    for answer in question_answers:
        answer.points = calculate_points(answer.answer, points_table)
    db.session.commit()
    return ok_response()


def stop_question_from_running(lecture_id, asked_id, question_timelimit, end_time):
    with app.app_context():
        if question_timelimit == 0:
            return
        tempdb = get_tempdb()
        # Adding extra time to limit so when people gets question a bit later than others they still get to answer
        extra_time = 3
        end_time += extra_time * 1000
        while int(time.time() * 1000) < end_time:  # TODO: check carefully if any sense
            time.sleep(1)
            stopped = True
            question = tempdb.runningquestions.get_running_question_by_id(asked_id)
            if question:
                end_time = extra_time * 1000 + question.end_time
                stopped = False

            if stopped:
                tempdb.newanswers.delete_question_answers(asked_id)
                return

        tempdb.runningquestions.delete_running_question(asked_id)
        tempdb.usersshown.delete_all_from_question(asked_id)
        tempdb.usersextended.delete_all_from_question(asked_id)
        tempdb.usersanswered.delete_all_from_lecture(asked_id)
        tempdb.newanswers.delete_question_answers(asked_id)


@lecture_routes.route("/getQuestionByParId", methods=['GET'])
def get_question_by_par_id():
    if not request.args.get("par_id") or not request.args.get("doc_id"):
        abort(400)
    doc_id = int(request.args.get('doc_id'))
    par_id = request.args.get('par_id')
    edit = request.args.get('edit', False)
    d = get_doc_or_abort(doc_id)
    verify_ownership(d)
    question = get_question_data_from_document(d, par_id, edit)
    return json_response(question._asdict())


@lecture_routes.route("/getAskedQuestionById", methods=['GET'])
def get_asked_question_by_id():
    if not request.args.get("asked_id"):
        abort(400)
    asked_id = int(request.args.get('asked_id'))
    question = get_asked_question(asked_id)
    verify_is_lecturer(question.lecture)
    return json_response(question)


@lecture_routes.route("/stopQuestion", methods=['POST'])
def stop_question():
    """Route to stop question from running."""
    if not request.args.get("asked_id"):
        abort(400)
    asked_id = int(request.args.get('asked_id'))
    tempdb = get_tempdb()
    lecture = get_current_lecture_or_abort()
    verify_is_lecturer(lecture)
    tempdb.runningquestions.delete_running_question(asked_id)
    tempdb.usersshown.delete_all_from_question(asked_id)
    tempdb.usersanswered.delete_all_from_question(asked_id)
    return ok_response()


@lecture_routes.route("/getLectureAnswers", methods=['GET'])
def get_lecture_answers():
    """Changing this to long poll requires removing threads."""
    asked_id_str = request.args.get('asked_id')
    if not asked_id_str:
        return abort(400, "Bad request")

    asked_id = int(asked_id_str)

    try:
        rq = Runningquestion.query.filter_by(asked_id=asked_id).one()
    except NoResultFound:
        return abort(400, 'No running question')
    lecture = Lecture.query.get(rq.lecture_id)
    question = get_asked_question(asked_id)
    verify_ownership(lecture.doc_id)
    tempdb = get_tempdb()

    step = 0
    user_ids = []
    while step <= 10:
        step = 11
        user_ids = tempdb.newanswers.get_new_answers(asked_id)
        if user_ids:
            break

        step += 1
        # time.sleep(1)

    lecture_answers = question.answers.filter(LectureAnswer.user_id.in_(user_ids)).all() if user_ids else []

    return json_response(lecture_answers)


@lecture_routes.route("/answerToQuestion", methods=['PUT'])
def answer_to_question():
    if not request.args.get("asked_id") or not request.args.get('input'):
        abort(400, "Bad request")

    tempdb = get_tempdb()

    asked_id = int(request.args.get("asked_id"))
    req_input = json.loads(request.args.get("input"))
    answer = req_input['answers']
    whole_answer = answer
    lecture = get_current_lecture_or_abort()
    lecture_id = lecture.lecture_id
    current_user = get_current_user_id()
    asked_question = get_asked_question(asked_id)

    lecture_answer: LectureAnswer = asked_question.answers.filter_by(user_id=current_user).first()

    question = tempdb.runningquestions.get_running_question_by_id(asked_id)
    already_answered = tempdb.usersanswered.has_user_info(asked_id, current_user)
    if not question:
        return json_response({"questionLate": "The question has already finished. Your answer was not saved."})
    if already_answered:
        return json_response({"alreadyAnswered": "You have already answered to question. Your first answer is saved."})

    tempdb.usersanswered.add_user_info(lecture_id, asked_id, current_user)

    if (not lecture_answer) or (lecture_answer and answer != lecture_answer.answer):
        time_now = datetime.now(timezone.utc)
        question_points = asked_question.points
        points_table = create_points_table(question_points)
        points = calculate_points_from_json_answer(answer, points_table)
        answer = json.dumps(whole_answer)
        if lecture_answer and current_user != 0:
            lecture_answer.answered_on = time_now
            lecture_answer.answer = answer
            lecture_answer.points = points
        else:
            ans = LectureAnswer(user_id=current_user, question_id=asked_id, lecture_id=lecture_id, answer=answer,
                                answered_on=time_now, points=points)
            db.session.add(ans)
        db.session.commit()
        tempdb.newanswers.user_answered(lecture_id, asked_id, current_user)

    return ok_response()


@lecture_routes.route("/closePoints", methods=['PUT'])
def close_points():
    if not request.args.get("asked_id"):
        abort(400, "Bad request")

    tempdb = get_tempdb()
    lecture = get_current_lecture_or_abort()

    asked_id = int(request.args.get("asked_id"))
    lecture_id = lecture.lecture_id
    current_user = get_current_user_id()

    points = tempdb.showpoints.get_currently_shown_points(lecture_id)
    if points:
        tempdb.pointsclosed.add_user_info(lecture_id, asked_id, current_user)

    return ok_response()


def get_tempdb():
    return TempDb(session=db.session)
