import datetime
import json
import threading
import time
from time import mktime

from flask import Blueprint, request, abort, session, render_template, current_app

import models
from documentmodel.randutils import hashfunc
from models import db
from routes.common import getTimDb, getCurrentUserId, jsonResponse, verify_ownership, get_rights, has_ownership, \
    get_user_settings

lecture_routes = Blueprint('lecture',
                           __name__,
                           url_prefix='')


@lecture_routes.route('/getLectureInfo')
def get_lecture_info():
    """Route to get info from lectures. Gives answers, and messages and other necessary info."""
    if not request.args.get("lecture_id"):
        abort(400, "Bad request, missing lecture id")
    lecture_id = int(request.args.get("lecture_id"))
    messages = get_all_messages(lecture_id)
    timdb = getTimDb()
    question_ids = []
    answerers = []

    is_lecturer = False
    current_user = getCurrentUserId()
    if timdb.lectures.get_lecture(lecture_id)[0].get("lecturer") == current_user:
        is_lecturer = True

    if is_lecturer:
        answer_dicts = timdb.lecture_answers.get_answers_to_questions_from_lecture(lecture_id)
    else:
        answer_dicts = timdb.lecture_answers.get_user_answers_to_questions_from_lecture(lecture_id, current_user)

    added_users = []
    for singleDict in answer_dicts:
        singleDict['user_name'] = timdb.users.getUser(singleDict['user_id']).get("name")
        if singleDict['question_id'] not in question_ids:
            question_ids.append(singleDict['question_id'])
        if singleDict['user_id'] not in added_users:
            added_users.append(singleDict['user_id'])
            answerers.append({'user_name': singleDict['user_name'], 'user_id': singleDict['user_id']})

    lecture_questions = timdb.questions.get_multiple_asked_questions(question_ids)

    return jsonResponse(
        {"messages": messages, "answerers": answerers, "answers": answer_dicts, "questions": lecture_questions,
         "isLecturer": is_lecturer, "user": {'user_name': timdb.users.getUser(current_user)['name'],
                                             'user_id': current_user}})


@lecture_routes.route('/getAllMessages')
def get_all_messages(param_lecture_id=-1):
    """Route to get all the messages from some lecture.
       Tulisi hakea myös kaikki aukiolevat kysymykset, joihin käyttäjä ei ole vielä vastannut."""
    if not request.args.get("lecture_id") and param_lecture_id is -1:
        abort(400, "Bad request, missing lecture id")
    timdb = getTimDb()
    if request.args.get("lecture_id"):
        lecture_id = int(request.args.get("lecture_id"))
    else:
        lecture_id = param_lecture_id

    # Prevents previously asked question to be asked from user and new questions from people who just came to lecture
    # current_user = getCurrentUserId()
    # for triple in __question_to_be_asked:
    #     if triple[0] == lecture_id and current_user not in triple[2]:
    #         triple[2].append(current_user)

    messages = timdb.messages.get_messages(lecture_id)
    if len(messages) > 0:
        list_of_new_messages = []
        for message in messages:
            user = timdb.users.getUser(message.get('user_id'))
            time_as_time = datetime.datetime.fromtimestamp(
                mktime(time.strptime(message.get("timestamp"), "%Y-%m-%d %H:%M:%S.%f")))
            list_of_new_messages.append(
                {"sender": user.get('name'),
                 "time": time_as_time.strftime('%H:%M:%S'),
                 "message": message.get('message')})

        # When using this same method just to get the messages for lectureInfo
        if param_lecture_id is not -1:
            return list_of_new_messages

        return jsonResponse(
            {"status": "results", "data": list_of_new_messages, "lastid": messages[-1].get('msg_id'),
             "lectureId": lecture_id})

    # When using this same method just to get the messages for lectureInfo
    if param_lecture_id is not -1:
        return []

    return jsonResponse({"status": "no-results", "data": [], "lastid": -1, "lectureId": lecture_id})


@lecture_routes.route('/getUpdates')
def get_updates():
    """Gets updates from some lecture. Checks updates in 1 second frequently and answers if there is updates."""
    if not request.args.get('client_message_id') or not request.args.get("lecture_id") or not request.args.get(
            'is_lecturer'):
        abort(400, "Bad request")
    client_last_id = int(request.args.get('client_message_id'))
    current_question_id = None
    current_points_id = None
    if 'current_question_id' in request.args:
        current_question_id = int(request.args.get('current_question_id'))
    if 'current_points_id' in request.args:
        current_points_id = int(request.args.get('current_points_id'))

    use_wall = False
    use_questions = False
    if request.args.get('get_messages') == "true":
        session['use_wall'] = True
        use_wall = True
    else:
        session['use_wall'] = False

    if request.args.get('get_questions') == "true":
        session['use_questions'] = True
        use_questions = True
    else:
        session['use_questions'] = False

    helper = request.args.get("lecture_id")
    if len(helper) > 0:
        lecture_id = int(float(helper))
    else:
        lecture_id = -1

    timdb = getTimDb()
    tempdb = getTempDb()
    step = 0

    doc_id = request.args.get("doc_id")
    if doc_id:
        doc_id = int(doc_id)
    if not check_if_lecture_is_running(lecture_id):
        timdb.lectures.delete_users_from_lecture(lecture_id)
        clean_dictionaries_by_lecture(lecture_id)
        return get_running_lectures(doc_id)

    list_of_new_messages = []
    last_message_id = -1

    lecturers = []
    students = []

    time_now = str(datetime.datetime.now().strftime("%H:%M:%S"))
    tempdb.useractivity.update_or_add_activity(lecture_id, getCurrentUserId(), time_now)

    lecture = timdb.lectures.get_lecture(lecture_id)

    current_user = getCurrentUserId()
    lecture_ending = 100

    # Jos poistaa tämän while loopin, muuttuu long pollista perinteiseksi polliksi
    while step <= 10:
        lecturers, students = get_lecture_users(timdb, tempdb, lecture_id)
        # Gets new messages if the wall is in use.
        if use_wall:
            last_message = timdb.messages.get_last_message(lecture_id)
            if last_message:
                last_message_id = last_message[-1].get('msg_id')
                if last_message_id != client_last_id:
                    messages = timdb.messages.get_new_messages(lecture_id, client_last_id)
                    messages.reverse()

                    for message in messages:
                        user = timdb.users.getUser(message.get('user_id'))
                        time_as_time = datetime.datetime.fromtimestamp(
                            mktime(time.strptime(message.get("timestamp"), "%Y-%m-%d %H:%M:%S.%f")))
                        list_of_new_messages.append(
                            {"sender": user.get('name'),
                             "time": time_as_time.strftime('%H:%M:%S'),
                             "message": message.get('message')})
                    last_message_id = messages[-1].get('msg_id')

        # Check if current question is still running and user hasn't already answered on it on another tab
        # Return also questions new end time if it is extended
        if current_question_id:
            resp = {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                    "lectureId": lecture_id, "question": True, "isLecture": True, "lecturers": lecturers,
                    "students": students, "lectureEnding": lecture_ending, "new_end_time": None}

            question = tempdb.runningquestions.get_running_question_by_id(current_question_id)
            already_answered = tempdb.usersanswered.has_user_info(current_question_id, current_user)
            if question and not already_answered:
                already_extended = tempdb.usersextended.has_user_info(current_question_id, current_user)
                if not already_extended:
                    tempdb.usersextended.add_user_info(lecture_id, current_question_id, current_user)
                    # Return this is question has been extended
                    resp['new_end_time'] = question.end_time
                    return jsonResponse(resp)
            else:
                # Return this if question has ended or user has answered to it
                return jsonResponse(resp)

        if current_points_id:
            resp = {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                    "lectureId": lecture_id, "question": True, "isLecture": True, "lecturers": lecturers,
                    "students": students, "lectureEnding": lecture_ending, "points_closed": True}
            already_closed = tempdb.pointsclosed.has_user_info(current_points_id, current_user)
            if already_closed:
                return jsonResponse(resp)

        # Gets new questions if the questions are in use.
        if use_questions:
            new_question = get_new_question(lecture_id, current_question_id, current_points_id)
            if new_question is not None:
                lecture_ending = check_if_lecture_is_ending(current_user, timdb, lecture_id)
                resp = {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                        "lectureId": lecture_id, "isLecture": True, "lecturers": lecturers,
                        "students": students, "lectureEnding": lecture_ending}
                resp.update(new_question)
                return jsonResponse(resp)

        if len(list_of_new_messages) > 0:
            if len(lecture) > 0 and lecture[0].get("lecturer") == current_user:
                lecture_ending = check_if_lecture_is_ending(current_user, timdb, lecture_id)
                lecturers, students = get_lecture_users(timdb, tempdb, lecture_id)
            return jsonResponse(
                {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                 "lectureId": lecture_id, "isLecture": True, "lecturers": lecturers, "students": students,
                 "lectureEnding": lecture_ending})

        db.session.remove()
        # Myös tämä sleep kannattaa poistaa.
        time.sleep(1)
        step += 1

    if len(lecture) > 0 and lecture[0].get("lecturer") == current_user:
        lecture_ending = check_if_lecture_is_ending(current_user, timdb, lecture_id)

    return jsonResponse(
        {"status": "no-results", "data": ["No new messages"], "lastid": client_last_id, "lectureId": lecture_id,
         "isLecture": True, "lecturers": lecturers, "students": students, "lectureEnding": lecture_ending})


@lecture_routes.route('/getQuestionManually')
def get_question_manually():
    """Route to use to get question manually (instead of getting question in /getUpdates)."""
    if not request.args.get('lecture_id'):
        abort(400, "Bad request")
    lecture_id = int(request.args.get('lecture_id'))
    new_question = get_new_question(lecture_id, None, None, True)
    return jsonResponse(new_question)


def get_new_question(lecture_id, current_question_id=None, current_points_id=None, force=False):
    """
    :param current_points_id: TODO: what is this?
    :param current_question_id: The id of the current question.
    :param lecture_id: lecture to get running questions from
    :param force: Return question, even if it already has been shown to user
    :return: None if no questions are running
             dict with data of new question if there is a question running and user hasn't answered to that question.
             {'already_answered': True} if there is a question running and user has answered to that.
    """
    timdb = getTimDb()
    tempdb = getTempDb()
    current_user = getCurrentUserId()
    question = tempdb.runningquestions.get_lectures_running_questions(lecture_id)
    if question:
        question = question[0]
        asked_id = question.asked_id
        already_shown = tempdb.usersshown.has_user_info(asked_id, current_user)
        already_answered = tempdb.usersanswered.has_user_info(asked_id, current_user)
        if already_answered:
            if force:
                return {'already_answered': True}
            else:
                return None
        if (not already_shown or force) or (asked_id != current_question_id):
            ask_time = question.ask_time
            question_json = timdb.questions.get_asked_question(asked_id)[0]["json"]
            answer = timdb.lecture_answers.get_user_answer_to_question(asked_id, current_user)
            tempdb.usersshown.add_user_info(lecture_id, asked_id, current_user)
            tempdb.usersextended.add_user_info(lecture_id, asked_id, current_user)
            if answer:
                answer = answer[0]['answer']
            else:
                answer = ''
            return {'question': True, 'askedId': asked_id, 'asked': ask_time, 'questionJson': question_json,
                    "answer": answer}
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
                question = timdb.questions.get_asked_question(asked_id)[0]
                tempdb.pointsshown.add_user_info(lecture_id, asked_id, current_user)
                answer = timdb.lecture_answers.get_user_answer_to_question(asked_id, current_user)
                if answer:
                    answer = answer[0]['answer']
                    return {"result": True, 'askedId': asked_id, "questionJson": question["json"], "answer": answer,
                            "expl": question["expl"]}
        return None


def check_if_lecture_is_ending(current_user, timdb, lecture_id):
    """Checks if the lecture is about to end.
    1 -> ends in 1 min. 5 -> ends in 5 min. 100 -> goes on atleast for 5 mins.
    :param current_user: The current user id.
    :param timdb: The TimDb object.
    :param lecture_id: The lecture id.
    :return: """
    lecture = timdb.lectures.get_lecture(lecture_id)
    lecture_ending = 100
    if len(lecture) > 0 and lecture[0].get("lecturer") == current_user:
        time_now = datetime.datetime.now()
        ending_time = datetime.datetime.fromtimestamp(
            mktime(time.strptime(lecture[0].get("end_time"), "%Y-%m-%d %H:%M")))
        time_left = str(ending_time - time_now)
        splitted_time = time_left.split(",")
        if len(splitted_time) == 1:
            h, m, s = splitted_time[0].split(":")
            hours_as_min = int(h) * 60
            if hours_as_min + int(m) < 1:
                lecture_ending = 1
            elif hours_as_min + int(m) < 5:
                lecture_ending = 5

    return lecture_ending


@lecture_routes.route('/sendMessage', methods=['POST'])
def send_message():
    """Route to add message to database."""
    timdb = getTimDb()
    new_message = request.args.get("message")
    lecture_id = int(request.args.get("lecture_id"))

    new_timestamp = str(datetime.datetime.now())
    msg_id = timdb.messages.add_message(getCurrentUserId(), lecture_id, new_message, new_timestamp, True)
    return jsonResponse(msg_id)


@lecture_routes.route('/getQuestion')
def get_question():
    doc_id = request.args.get('doc_id')
    par_index = request.args.get('par_index')
    timdb = getTimDb()
    question = timdb.questions.get_paragraphs_question(doc_id, par_index)
    return jsonResponse(question)


@lecture_routes.route('/getQuestions', methods=['GET'])
def get_all_questions():
    timdb = getTimDb()
    questions = timdb.questions.get_questions()
    return jsonResponse(questions)


@lecture_routes.route('/addQuestion/', methods=['POST'])
def add_question():
    # TODO: Only lecturers should be able to create questions.
    question_id = None
    if request.args.get('question_id'):
        question_id = int(request.args.get('question_id'))
    question_title = request.args.get('question_title')
    answer = request.args.get('answer')
    doc_id = int(request.args.get('doc_id'))
    timdb = getTimDb()
    if not timdb.users.has_edit_access(getCurrentUserId(), doc_id):
        abort(403)
    par_id = request.args.get('par_id')
    points = request.args.get('points')
    if points is None:
        points = ''
    expl = request.args.get('expl')
    if expl is None:
        expl = '{}'
    question_json = request.args.get('questionJson')

    if not question_id:
        questions = timdb.questions.add_questions(doc_id, par_id, question_title, answer, question_json, points, expl)
    else:
        questions = timdb.questions.update_question(question_id, doc_id, par_id, question_title, answer, question_json,
                                                    points, expl)
    return jsonResponse(timdb.questions.get_question(questions)[0])


@lecture_routes.route('/checkLecture', methods=['GET'])
def check_lecture():
    """Route to check if the current user is in some lecture in specific document."""
    timdb = getTimDb()
    tempdb = getTempDb()
    current_user = getCurrentUserId()
    is_in_lecture, lecture_id, = timdb.lectures.check_if_in_any_lecture(current_user)
    lecture = timdb.lectures.get_lecture(lecture_id)
    lecturers = []
    students = []
    if lecture:
        if check_if_lecture_is_running(lecture_id):
            lecture_code = lecture[0].get("lecture_code")
            if lecture[0].get("lecturer") == current_user:
                is_lecturer = True
                lecturers, students = get_lecture_users(timdb, tempdb, lecture_id)
            else:
                is_lecturer = False

            if "use_wall" in session:
                use_wall = session['use_wall']
            else:
                use_wall = True
                session['use_wall'] = True

            if "use_questions" in session:
                use_question = session['use_questions']
            else:
                use_question = True
                session['use_questions'] = True

            doc_name = timdb.documents.get_document(lecture[0].get("doc_id"))["name"]

            return jsonResponse({"isInLecture": is_in_lecture, "lectureId": lecture_id, "lectureCode": lecture_code,
                                 "isLecturer": is_lecturer, "startTime": lecture[0].get("start_time"),
                                 "endTime": lecture[0].get("end_time"), "lecturers": lecturers, "students": students,
                                 "useWall": use_wall, "useQuestions": use_question, "doc_name": doc_name})
        else:
            leave_lecture_function(lecture_id)
            timdb.lectures.delete_users_from_lecture(lecture_id)
            clean_dictionaries_by_lecture(lecture_id)
    if 'doc_id' in request.args:
        return get_running_lectures(int(request.args['doc_id']))
    else:
        return jsonResponse("")


@lecture_routes.route("/startFutureLecture", methods=['POST'])
def start_future_lecture():
    if not request.args.get('lecture_code') or not request.args.get("doc_id"):
        abort(400)

    timdb = getTimDb()
    tempdb = getTempDb()
    lecture_code = request.args.get('lecture_code')
    doc_id = int(request.args.get("doc_id"))
    verify_ownership(doc_id)
    lecture = timdb.lectures.get_lecture_by_code(lecture_code, doc_id)
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    lecture = timdb.lectures.update_lecture_starting_time(lecture, time_now)
    timdb.lectures.join_lecture(lecture.get("lecture_id"), getCurrentUserId(), True)
    students, lecturers = get_lecture_users(timdb, tempdb, lecture.get("lecture_id"))
    return jsonResponse({"isLecturer": True, "lectureCode": lecture_code, "startTime": lecture.get("start_time"),
                         "endTime": lecture.get("end_time"), "lectureId": lecture.get("lecture_id"),
                         "students": students,
                         "lecturers": lecturers})


@lecture_routes.route('/getAllLecturesFromDocument', methods=['GET'])
def get_all_lectures():
    if not request.args.get('doc_id'):
        abort(400)

    doc_id = int(request.args.get('doc_id'))
    timdb = getTimDb()

    lectures = timdb.lectures.get_all_lectures_from_document(doc_id)
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    current_lectures = []
    past_lectures = []
    future_lectures = []
    for lecture in lectures:
        lecture_info = {"lecture_id": lecture.get("lecture_id"), "lecture_code": lecture.get('lecture_code'),
                        "target": "/showLectureInfo/" + str(lecture.get("lecture_id")),
                        "is_access_code": not (lecture.get("password") == "")}
        if lecture.get("start_time") <= time_now < lecture.get("end_time"):
            current_lectures.append(lecture_info)
        elif lecture.get("end_time") <= time_now:
            past_lectures.append(lecture_info)
        else:
            future_lectures.append(lecture_info)

    return jsonResponse(
        {"currentLectures": current_lectures, "futureLectures": future_lectures, "pastLectures": past_lectures})


@lecture_routes.route('/showLectureInfo/<int:lecture_id>', methods=['GET'])
def show_lecture_info(lecture_id):
    timdb = getTimDb()
    lecture = timdb.lectures.get_lecture(lecture_id)
    if len(lecture) <= 0:
        abort(400)

    lecture = lecture[0]
    doc = timdb.documents.resolve_doc_id_name(str(lecture.get('doc_id')))
    in_lecture, lecture_ids = timdb.lectures.check_if_in_any_lecture(getCurrentUserId())
    settings = get_user_settings()
    return render_template("lectureInfo.html",
                           doc=doc,
                           lectureId=lecture_id,
                           lectureCode=lecture.get("lecture_code"),
                           lectureStartTime=lecture.get("start_time"),
                           lectureEndTime=lecture.get("end_time"),
                           in_lecture=in_lecture,
                           settings=settings,
                           rights=get_rights(doc['id']),
                           translations=timdb.documents.get_translations(doc['id']))


@lecture_routes.route('/showLectureInfoGivenName/', methods=['GET'])
def show_lecture_info_given_name():
    timdb = getTimDb()
    if 'lecture_id' in request.args:
        lecture = timdb.lectures.get_lecture(int(request.args.get('lecture_id')))
    else:
        lecture = timdb.lectures.get_lecture_by_name(request.args.get('lecture_code'), int(request.args.get('doc_id')))
    if len(lecture) <= 0:
        abort(400)

    lecture = lecture[0]
    lecturer = lecture.get("lecturer")
    current_user = getCurrentUserId()

    response = {"docId": lecture.get("doc_id"), "lectureId": lecture.get("lecture_id"),
                "lectureCode": lecture.get("lecture_code"), "lectureStartTime": lecture.get("start_time"),
                "lectureEndTime": lecture.get("end_time")}
    if lecturer == current_user:
        response["password"] = lecture.get("password")

    return jsonResponse(response)


@lecture_routes.route('/lectureNeedsPassword/', methods=['GET'])
def lecture_needs_password():
    timdb = getTimDb()
    if 'lecture_id' in request.args:
        lecture = timdb.lectures.get_lecture(int(request.args.get('lecture_id')))
    else:
        lecture = timdb.lectures.get_lecture_by_name(request.args.get('lecture_code'), int(request.args.get('doc_id')))
    if len(lecture) <= 0:
        abort(400)
    lecture = lecture[0]
    return jsonResponse(lecture.get("password") != '')


def get_lecture_users(timdb, tempdb, lecture_id):
    lecture = timdb.lectures.get_lecture(lecture_id)
    lecturers = []
    students = []

    activity = tempdb.useractivity.get_all_user_activity(lecture_id)

    for user in activity:
        user_id = user.user_id
        active = user.active
        if lecture[0].get("lecturer") == user_id:
            lecturer = {"name": timdb.users.getUser(user_id).get("name"),
                        "active": active}
            lecturers.append(lecturer)
        else:
            student = {"name": timdb.users.getUser(user_id).get("name"),
                       "active": active, "user_id": user_id}
            students.append(student)

    return lecturers, students


def check_if_lecture_is_running(lecture_id):
    timdb = getTimDb()
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    return timdb.lectures.check_if_lecture_is_running(lecture_id, time_now)


def check_if_lecture_is_full(lecture_id):
    timdb = getTimDb()
    return timdb.lectures.check_if_lecture_is_full(lecture_id)


def get_running_lectures(doc_id=None):
    """Gets all running and future lectures.
    :param doc_id: The document id for which to get lectures.
    """
    timdb = getTimDb()
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    lecture_code = "Not running"
    list_of_lectures = []
    is_lecturer = False
    if doc_id:
        list_of_lectures = timdb.lectures.get_document_lectures(doc_id, time_now)
        is_lecturer = has_ownership(doc_id)
    current_lecture_codes = []
    future_lectures = []
    for lecture in list_of_lectures:
        if lecture.get("start_time") <= time_now < lecture.get("end_time"):
            current_lecture_codes.append({"lecture_code": lecture.get("lecture_code"),
                                          "is_access_code": not (lecture.get("password") == "")})
        else:
            future_lectures.append(
                {"lecture_code": lecture.get("lecture_code"),
                 "lecture_start": lecture.get("start_time")})
    return jsonResponse(
        {"isLecturer": is_lecturer, "lectures": current_lecture_codes, "futureLectures": future_lectures,
         "lectureCode": lecture_code})


@lecture_routes.route('/createLecture', methods=['POST'])
def create_lecture():
    if not request.args.get("doc_id") or not request.args.get("start_date") or not request.args.get(
            "end_date") or not request.args.get("lecture_code"):
        abort(400, "Missing parameters")
    lecture_id = -1
    if request.args.get("lecture_id"):
        lecture_id = int(request.args.get("lecture_id"))
    doc_id = int(request.args.get("doc_id"))
    verify_ownership(doc_id)
    timdb = getTimDb()
    start_time = request.args.get("start_date")
    end_time = request.args.get("end_date")
    lecture_code = request.args.get("lecture_code")
    password = request.args.get("password")
    if 'max_students' in request.args:
        max_students = request.args.get('max_students')
    else:
        max_students = ''

    options = {}
    if max_students != "":
        options['max_students'] = max_students

    if not password:
        password = ""
    current_user = getCurrentUserId()
    if not timdb.lectures.check_if_correct_name(doc_id, lecture_code, lecture_id):
        abort(400, "Can't create two or more lectures with the same name to the same document.")

    options = json.dumps(options)
    if lecture_id < 0:
        lecture_id = timdb.lectures.create_lecture(doc_id, current_user, start_time, end_time, lecture_code, password,
                                                   options, True)
    else:
        timdb.lectures.update_lecture(lecture_id, doc_id, current_user, start_time, end_time, lecture_code, password,
                                      options)

    current_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")

    if start_time <= current_time <= end_time:
        timdb.lectures.join_lecture(lecture_id, current_user, True)
    return jsonResponse({"lectureId": lecture_id})


@lecture_routes.route('/endLecture', methods=['POST'])
def end_lecture():
    if not request.args.get("doc_id") or not request.args.get("lecture_id"):
        abort(400)

    doc_id = int(request.args.get("doc_id"))
    lecture_id = int(request.args.get("lecture_id"))
    verify_ownership(doc_id)
    timdb = getTimDb()
    timdb.lectures.delete_users_from_lecture(lecture_id)

    now = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    timdb.lectures.set_end_for_lecture(lecture_id, str(now))

    clean_dictionaries_by_lecture(lecture_id)

    return get_running_lectures(doc_id)


def clean_dictionaries_by_lecture(lecture_id):
    """Cleans data from lecture that isn't running anymore.
    :param lecture_id: The lecture id.
    """
    tempdb = getTempDb()
    tempdb.runningquestions.delete_lectures_running_questions(lecture_id)
    tempdb.usersshown.delete_all_from_lecture(lecture_id)
    tempdb.usersextended.delete_all_from_lecture(lecture_id)
    tempdb.useractivity.delete_lecture_activity(lecture_id)
    tempdb.newanswers.delete_lecture_answers(lecture_id)
    tempdb.showpoints.stop_showing_points(lecture_id)
    tempdb.pointsshown.delete_all_from_lecture(lecture_id)


@lecture_routes.route('/extendLecture', methods=['POST'])
def extend_lecture():
    if not request.args.get("doc_id") or not request.args.get("lecture_id") or not request.args.get("new_end_time"):
        abort(400)
    doc_id = int(request.args.get("doc_id"))
    lecture_id = int(request.args.get("lecture_id"))
    new_end_time = request.args.get("new_end_time")
    verify_ownership(doc_id)
    timdb = getTimDb()
    timdb.lectures.extend_lecture(lecture_id, new_end_time)
    return jsonResponse("")


@lecture_routes.route('/deleteLecture', methods=['POST'])
def delete_lecture():
    if not request.args.get("doc_id") or not request.args.get("lecture_id"):
        abort(400)
    doc_id = int(request.args.get("doc_id"))
    verify_ownership(doc_id)
    lecture_id = int(request.args.get("lecture_id"))
    timdb = getTimDb()
    timdb.messages.delete_messages_from_lecture(lecture_id, True)
    timdb.lectures.delete_users_from_lecture(lecture_id, True)

    timdb.lectures.delete_lecture(lecture_id, True)

    clean_dictionaries_by_lecture(lecture_id)

    return get_running_lectures(doc_id)


@lecture_routes.route('/joinLecture', methods=['POST'])
def join_lecture():
    """Route to join lecture. Checks that the given password is correct."""
    if not request.args.get("doc_id") or not request.args.get("lecture_code"):
        abort(400, "Missing parameters")
    timdb = getTimDb()
    tempdb = getTempDb()
    doc_id = int(request.args.get("doc_id"))
    lecture_code = request.args.get("lecture_code")
    password_quess = request.args.get("password_quess")
    lecture_id = timdb.lectures.get_lecture_by_code(lecture_code, doc_id)
    current_user = getCurrentUserId()

    if not check_if_lecture_is_running(lecture_id):
        return jsonResponse({'lecture_ended': True})

    # TODO Allow lecturer always join, even if the lecture is full
    if check_if_lecture_is_full(lecture_id):
        return jsonResponse({'lecture_full': True})

    if current_user == 0:
        user_name = 'Anonymous'
        user_real_name = 'Guest'
        user_id = timdb.users.createAnonymousUser(user_name, user_real_name)
        session['user_id'] = user_id
        session['user_name'] = user_name
        session['real_name'] = user_real_name
        current_user = user_id

    lecture = timdb.lectures.get_lecture(lecture_id)
    if lecture[0].get("password") != password_quess:
        return jsonResponse({"correctPassword": False})

    doc_name = timdb.documents.get_document(lecture[0].get("doc_id"))["name"]

    in_lecture, current_lecture_id, = timdb.lectures.check_if_in_any_lecture(current_user)
    if in_lecture:
        leave_lecture_function(current_lecture_id)
    timdb.lectures.join_lecture(lecture_id, current_user, True)

    time_now = str(datetime.datetime.now().strftime("%H:%M:%S"))
    tempdb.useractivity.update_or_add_activity(lecture_id, current_user, time_now)

    session['in_lecture'] = [lecture_id]

    lecturers = []
    students = []
    if lecture[0].get("lecturer") == current_user:
        is_lecturer = True
        lecturers, students = get_lecture_users(timdb, tempdb, lecture_id)
    else:
        is_lecturer = False
    return jsonResponse(
        {"correctPassword": True, "inLecture": True, "lectureId": lecture_id, "isLecturer": is_lecturer,
         "lectureCode": lecture_code, "startTime": lecture[0].get("start_time"),
         "endTime": lecture[0].get("end_time"), "lecturers": lecturers, "students": students, "doc_name": doc_name})


@lecture_routes.route('/leaveLecture', methods=['POST'])
def leave_lecture():
    lecture_id = int(request.args.get("lecture_id"))
    leave_lecture_function(lecture_id)
    if 'doc_id' in request.args:
        return get_running_lectures(int(request.args['doc_id']))
    else:
        return jsonResponse("")


def leave_lecture_function(lecture_id):
    timdb = getTimDb()
    current_user = getCurrentUserId()
    if 'in_lecture' in session:
        lecture_list = session['in_lecture']
        if lecture_id in lecture_list:
            lecture_list.remove(lecture_id)
        session['in_lecture'] = lecture_list
    timdb.lectures.leave_lecture(lecture_id, current_user, True)

    # if (current_user, lecture_id) in __user_activity:
    #    del __user_activity[current_user, lecture_id]


@lecture_routes.route("/questions/<int:doc_id>")
def get_questions(doc_id):
    verify_ownership(doc_id)
    timdb = getTimDb()
    questions = timdb.questions.get_doc_questions(doc_id)
    return jsonResponse(questions)


@lecture_routes.route("/getLectureWithName", methods=['POST'])
def get_lecture_with_name(lecture_code, doc_id):
    verify_ownership(doc_id)
    timdb = getTimDb()
    lecture = timdb.lectures.get_lecture_by_code(lecture_code, doc_id)
    return jsonResponse(lecture)


@lecture_routes.route("/extendQuestion", methods=['POST'])
def extend_question():
    lecture_id = int(request.args.get('lecture_id'))
    asked_id = int(request.args.get('asked_id'))
    extend = int(request.args.get('extend'))

    tempdb = getTempDb()
    tempdb.runningquestions.extend_question(asked_id, extend * 1000)

    return jsonResponse('Extended')


@lecture_routes.route("/askQuestion", methods=['POST'])
def ask_question():
    if not request.args.get('doc_id') or not \
            (request.args.get('question_id') or request.args.get('asked_id')) or not request.args.get('lecture_id'):
        abort(400, "Bad request")
    doc_id = int(request.args.get('doc_id'))
    lecture_id = int(request.args.get('lecture_id'))
    question_id = None
    asked_id = None
    if 'question_id' in request.args:
        question_id = int(request.args.get('question_id'))
    else:
        asked_id = int(request.args.get('asked_id'))

    verify_ownership(doc_id)

    if lecture_id < 0:
        abort(400, "Not valid lecture id")

    timdb = getTimDb()
    if question_id:
        question = timdb.questions.get_question(question_id)[0]
        question_json_str = question.get("questionJson")
        question_hash = hashfunc(question.get("questionJson"))
        asked_hash = timdb.questions.get_asked_json_by_hash(question_hash)
        if asked_hash:
            asked_json_id = asked_hash[0].get("asked_json_id")
        else:
            asked_json_id = timdb.questions.add_asked_json(question_json_str, question_hash)
        asked_time = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f"))
        asked_id = timdb.questions.add_asked_questions(lecture_id, doc_id, None, asked_time, question.get("points"),
                                                       asked_json_id, question.get("expl"))
    else:
        question = timdb.questions.get_asked_question(asked_id)[0]
        asked_json = timdb.questions.get_asked_json_by_id(question["asked_json_id"])[0]
        asked_json_id = asked_json["asked_json_id"]
        question_json_str = asked_json["json"]
    question_json = json.loads(question_json_str)

    if not question_json["TIMELIMIT"]:
        question_timelimit = 0
    else:
        question_timelimit = int(question_json["TIMELIMIT"])

    ask_time = int(time.time() * 1000)
    end_time = ask_time + question_timelimit * 1000
    thread_to_stop_question = threading.Thread(target=stop_question_from_running,
                                               args=(lecture_id, asked_id, question_timelimit, end_time))

    thread_to_stop_question.start()

    verify_ownership(int(doc_id))
    tempdb = getTempDb()
    delete_question_temp_data(asked_id, lecture_id, tempdb)

    tempdb.runningquestions.add_running_question(lecture_id, asked_id, ask_time, end_time)

    return jsonResponse(asked_id)


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
    if 'asked_id' not in request.args or 'lecture_id' not in request.args:
        abort("400")
    asked_id = int(request.args.get('asked_id'))
    lecture_id = int(request.args.get('lecture_id'))

    tempdb = getTempDb()
    tempdb.showpoints.stop_showing_points(lecture_id)
    tempdb.showpoints.add_show_points(lecture_id, asked_id)

    return jsonResponse("")


@lecture_routes.route('/updatePoints/', methods=['POST'])
def update_question_points():
    """Route to get add question to database"""
    if 'asked_id' not in request.args or 'points' not in request.args:
        abort("400")
    asked_id = int(request.args.get('asked_id'))
    points = request.args.get('points')
    timdb = getTimDb()
    asked_question = timdb.questions.get_asked_question(asked_id)[0]
    lecture_id = int(asked_question['lecture_id'])
    if not check_if_is_lecturer(lecture_id):
        abort("400")
    timdb.questions.update_asked_question_points(asked_id, points)
    points_table = create_points_table(points)
    question_answers = timdb.lecture_answers.get_answers_to_question(asked_id)
    for answer in question_answers:
        user_points = calculate_points(answer['answer'], points_table)
        timdb.lecture_answers.update_answer_points(answer['answer_id'], user_points)
    return jsonResponse("")


def stop_question_from_running(lecture_id, asked_id, question_timelimit, end_time):
    with current_app.app_context():
        if question_timelimit == 0:
            return
        tempdb = getTempDb()
        # Adding extra time to limit so when people gets question a bit later than others they still get to answer
        extra_time = 3
        end_time += extra_time * 1000
        while int(time.time() * 1000) < end_time:
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


@lecture_routes.route("/getQuestionById", methods=['GET'])
def get_question_by_id():
    if not request.args.get("question_id"):
        abort("400")
    # doc_id = int(request.args.get('doc_id'))
    question_id = int(request.args.get('question_id'))

    # verifyOwnership(doc_id)
    timdb = getTimDb()
    question = timdb.questions.get_question(question_id)
    return jsonResponse(question[0])


@lecture_routes.route("/getAskedQuestionById", methods=['GET'])
def get_asked_question_by_id():
    if not request.args.get("asked_id"):
        abort("400")
    # doc_id = int(request.args.get('doc_id'))
    asked_id = int(request.args.get('asked_id'))
    timdb = getTimDb()
    question = timdb.questions.get_asked_question(asked_id)[0]
    lecture_id = question['lecture_id']
    if not check_if_is_lecturer(lecture_id):
        abort("400")
    return jsonResponse(question)


def check_if_is_lecturer(lecture_id):
    timdb = getTimDb()
    current_user = getCurrentUserId()
    return timdb.lectures.get_lecture(lecture_id)[0].get("lecturer") == current_user


@lecture_routes.route("/stopQuestion", methods=['POST'])
def stop_question():
    """Route to stop question from running."""
    if not request.args.get("asked_id") or not request.args.get("lecture_id"):
        abort("400")
    asked_id = int(request.args.get('asked_id'))
    lecture_id = int(request.args.get('lecture_id'))
    timdb = getTimDb()
    tempdb = getTempDb()
    current_user = getCurrentUserId()
    lecture = timdb.lectures.get_lecture(lecture_id)
    if lecture:
        if lecture[0].get("lecturer") != current_user:
            abort("400", "You cannot stop questions on someone elses lecture.")
        tempdb.runningquestions.delete_running_question(asked_id)
        tempdb.usersshown.delete_all_from_question(asked_id)
        tempdb.usersanswered.delete_all_from_question(asked_id)
    return jsonResponse("")


@lecture_routes.route("/deleteQuestion", methods=['POST'])
def delete_question():
    if not request.args.get("question_id") or not request.args.get('doc_id'):
        abort("400")

    doc_id = int(request.args.get('doc_id'))
    question_id = int(request.args.get('question_id'))

    verify_ownership(doc_id)
    timdb = getTimDb()
    timdb.questions.delete_question(question_id)
    timdb.lecture_answers.delete_answers_from_question(question_id)

    return jsonResponse("")


@lecture_routes.route("/getLectureAnswers", methods=['GET'])
def get_lecture_answers():
    """Changing this to long poll requires removing threads."""
    if not request.args.get('asked_id') or not request.args.get('doc_id') or not request.args.get('lecture_id'):
        abort(400, "Bad request")

    verify_ownership(int(request.args.get('doc_id')))
    asked_id = int(request.args.get('asked_id'))
    lecture_id = int(request.args.get('lecture_id'))

    tempdb = getTempDb()

    step = 0
    user_ids = []
    while step <= 10:
        question = tempdb.runningquestions.get_running_question_by_id(asked_id)
        if not question:
            return jsonResponse({"noAnswer": True})
        user_ids = tempdb.newanswers.get_new_answers(asked_id)
        if user_ids:
            break

        step += 1
        time.sleep(1)

    timdb = getTimDb()
    lecture_answers = []

    for user_id in user_ids:
        lecture_answers.append(timdb.lecture_answers.get_user_answer_to_question(asked_id, user_id)[0])

    latest_answer = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f"))

    return jsonResponse({"answers": lecture_answers, "askedId": asked_id, "latestAnswer": latest_answer})


@lecture_routes.route("/answerToQuestion", methods=['PUT'])
def answer_to_question():
    if not request.args.get("asked_id") or not request.args.get('answers') or not request.args.get('lecture_id'):
        abort(400, "Bad request")

    timdb = getTimDb()
    tempdb = getTempDb()

    asked_id = int(request.args.get("asked_id"))
    answer = request.args.get("answers")
    whole_answer = answer
    lecture_id = int(request.args.get("lecture_id"))
    current_user = getCurrentUserId()

    lecture_answer = timdb.lecture_answers.get_user_answer_to_question(asked_id, current_user)

    question = tempdb.runningquestions.get_running_question_by_id(asked_id)
    already_answered = tempdb.usersanswered.has_user_info(asked_id, current_user)
    if not question:
        return jsonResponse({"questionLate": "The question has already finished. Your answer was not saved."})
    if already_answered:
        return jsonResponse({"alreadyAnswered": "You have already answered to question. Your first answer is saved."})

    tempdb.usersanswered.add_user_info(lecture_id, asked_id, current_user)

    if (not lecture_answer) or (lecture_answer and answer != lecture_answer[0]["answer"]):
        time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f"))
        question_points = timdb.questions.get_asked_question(asked_id)[0].get("points")
        points_table = create_points_table(question_points)
        points = calculate_points(answer, points_table)
        if lecture_answer and current_user != 0:
            timdb.lecture_answers.update_answer(lecture_answer[0]["answer_id"], current_user, asked_id,
                                                lecture_id, whole_answer, time_now, points)
        else:
            timdb.lecture_answers.add_answer(current_user, asked_id, lecture_id, whole_answer, time_now,
                                             points)
        tempdb.newanswers.user_answered(lecture_id, asked_id, current_user)

    return jsonResponse("")


@lecture_routes.route("/closePoints", methods=['PUT'])
def close_points():
    if not request.args.get("asked_id") or not request.args.get('lecture_id'):
        abort(400, "Bad request")

    tempdb = getTempDb()

    asked_id = int(request.args.get("asked_id"))
    lecture_id = int(request.args.get("lecture_id"))
    current_user = getCurrentUserId()

    points = tempdb.showpoints.get_currently_shown_points(lecture_id)
    if points:
        tempdb.pointsclosed.add_user_info(lecture_id, asked_id, current_user)

    return jsonResponse("")


def create_points_table(points):
    points_table = []
    if points and points != '':
        points_split = points.split('|')
        for row in points_split:
            row_points = row.split(';')
            row_points_dict = {}
            for col in row_points:
                if col != '':
                    col_points = col.split(':', 2)
                    if len(col_points) == 1:
                        row_points_dict[col_points[0]] = 1
                    else:
                        row_points_dict[col_points[0]] = float(col_points[1])
            points_table.append(row_points_dict)
    return points_table


def calculate_points(answer, points_table):
    single_answers = []
    all_answers = answer.split('|')
    for answer in all_answers:
        single_answers.append(answer.split(','))

    points = 0.0
    for (oneAnswer, point_row) in zip(single_answers, points_table):
        for oneLine in oneAnswer:
            if oneLine in point_row:
                points += point_row[oneLine]
    return points


def user_in_lecture():
    timdb = getTimDb()
    current_user = getCurrentUserId()
    in_lecture, lecture_id, = timdb.lectures.check_if_in_any_lecture(current_user)
    if in_lecture:
        in_lecture = check_if_lecture_is_running(lecture_id)
    return in_lecture


def getTempDb():
    return models.tempdb
