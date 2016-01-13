# -*- coding: utf-8 -*-

import contracts
import imghdr
import io
import re
import datetime
from time import mktime
import posixpath
import threading

from flask import Flask, Blueprint
from flask import stream_with_context
from flask import render_template
from flask import send_from_directory
from werkzeug.contrib.profiler import ProfilerMiddleware
from werkzeug.utils import secure_filename
from flask.helpers import send_file
from bs4 import UnicodeDammit

from routes.groups import groups
from tim_app import app

# IMPORTANT: We want to disable contracts (if requested) as early as possible
# before any @contract decorator is encountered.
from timdb.users import ANONYMOUS_GROUPNAME

if app.config['CONTRACTS_ENABLED']:
    print('Contracts are ENABLED')
else:
    contracts.disable_all()
    print('Contracts are DISABLED')

from ReverseProxied import ReverseProxied
import containerLink
from documentmodel.document import Document
from routes.cache import cache
from routes.answer import answers
from routes.edit import edit_page, par_response
from routes.manage import manage_page
from routes.view import view_page
from routes.slide import slide_page
from routes.login import login_page
from routes.logger import logger_bp
from timdb.timdbbase import TimDbException
from plugin import PluginException
from routes.settings import settings_page
from routes.common import *
from documentmodel.randutils import hashfunc
import models
from models import db


# db.engine.pool.use_threadlocal = True # This may be needless

cache.init_app(app)

app.register_blueprint(settings_page)
app.register_blueprint(manage_page)
app.register_blueprint(edit_page)
app.register_blueprint(view_page)
app.register_blueprint(slide_page)
app.register_blueprint(login_page)
app.register_blueprint(logger_bp)
app.register_blueprint(answers)
app.register_blueprint(groups)
app.register_blueprint(Blueprint('bower',
                                 __name__,
                                 static_folder='static/scripts/bower_components',
                                 static_url_path='/static/scripts/bower_components'))

app.wsgi_app = ReverseProxied(app.wsgi_app)

print('Debug mode: {}'.format(app.config['DEBUG']))
print('Profiling: {}'.format(app.config['PROFILE']))

KNOWN_TAGS = ['difficult', 'unclear']


def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


DOC_EXTENSIONS = ['txt', 'md', 'markdown']
PIC_EXTENSIONS = ['png', 'jpg', 'jpeg', 'gif']
ALLOWED_EXTENSIONS = set(PIC_EXTENSIONS + DOC_EXTENSIONS)
STATIC_PATH = "./static/"
DATA_PATH = "./static/data/"


def error_generic(error, code):
    if 'text/html' in request.headers.get("Accept", ""):
        return render_template(str(code) + '.html', message=error.description), code
    else:
        return jsonResponse({'error': error.description}, code)


@app.errorhandler(400)
def bad_request(error):
    return error_generic(error, 400)


@app.errorhandler(403)
def forbidden(error):
    return error_generic(error, 403)


@app.errorhandler(404)
def notFound(error):
    return error_generic(error, 404)


@app.route('/diff/<int:doc_id>/<doc_hash>')
def document_diff(doc_id, doc_hash):
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id):
        abort(404)
    verify_edit_access(doc_id, "Sorry, you don't have permission to download this document.")
    try:
        doc_diff = timdb.documents.getDifferenceToPrevious(DocIdentifier(doc_id, doc_hash))
        return render_template('diff.html', diff_html=doc_diff)
    except TimDbException as e:
        abort(404, str(e))


@app.route('/download/<int:doc_id>/<doc_hash>')
def document_history(doc_id, doc_hash):
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id):
        abort(404)
    verify_edit_access(doc_id, "Sorry, you don't have permission to download this document.")
    try:
        doc_data = timdb.documents.getDocumentMarkdown(DocIdentifier(doc_id, doc_hash))
        return Response(doc_data, mimetype="text/plain")
    except TimDbException as e:
        abort(404, str(e))


@app.route('/download/<int:doc_id>')
def download_document(doc_id):
    return document_history(doc_id, get_newest_document(doc_id).hash)


@app.route('/upload/', methods=['POST'])
def upload_file():
    if not logged_in():
        abort(403, 'You have to be logged in to upload a file.')
    timdb = getTimDb()

    file = request.files.get('file')
    if file is None:
        abort(400, 'Missing file')
    folder = request.form.get('folder')
    if folder is None:
        return upload_image_or_file(file)
    filename = posixpath.join(folder, secure_filename(file.filename))

    user_name = getCurrentUserName()
    if not timdb.users.has_admin_access(getCurrentUserId()) \
            and re.match('^users/' + user_name + '/', filename) is None:
        abort(403, "You're not authorized to write here.")

    if not allowed_file(file.filename):
        abort(403, 'The file format is not allowed.')

    if filename.endswith(tuple(DOC_EXTENSIONS)):
        content = UnicodeDammit(file.read()).unicode_markup
        if not content:
            abort(400, 'Failed to convert the file to UTF-8.')
        timdb.documents.import_document(content, filename, getCurrentUserGroup())
        return "Successfully uploaded document"
    else:
        abort(400, 'Invalid document extension')


def try_upload_image(image_file):
    content = image_file.read()
    imgtype = imghdr.what(None, h=content)
    if imgtype is not None:
        timdb = getTimDb()
        img_id, img_filename = timdb.images.saveImage(content,
                                                      secure_filename(image_file.filename),
                                                      getCurrentUserGroup())
        timdb.users.grantViewAccess(0, img_id)  # So far everyone can see all images
        return jsonResponse({"file": str(img_id) + '/' + img_filename})
    else:
        abort(400, 'Invalid image type')


def upload_image_or_file(image_file):
    content = image_file.read()
    imgtype = imghdr.what(None, h=content)
    timdb = getTimDb()
    if imgtype is not None:
        img_id, img_filename = timdb.images.saveImage(content,
                                                      secure_filename(image_file.filename),
                                                      getCurrentUserGroup())
        timdb.users.grantViewAccess(timdb.users.getUserGroupByName(ANONYMOUS_GROUPNAME), img_id)  # So far everyone can see all images
        return jsonResponse({"image": str(img_id) + '/' + img_filename})
    else:
        file_id, file_filename = timdb.files.saveFile(content,
                                                      secure_filename(image_file.filename),
                                                      getCurrentUserGroup())
        timdb.users.grantViewAccess(timdb.users.getUserGroupByName(ANONYMOUS_GROUPNAME), file_id)  # So far everyone can see all files
        return jsonResponse({"file": str(file_id) + '/' + file_filename})


@app.route('/images/<int:image_id>/<image_filename>')
def get_image(image_id, image_filename):
    timdb = getTimDb()
    if not timdb.images.imageExists(image_id, image_filename):
        abort(404)
    verify_view_access(image_id)
    img_data = timdb.images.getImage(image_id, image_filename)
    imgtype = imghdr.what(None, h=img_data)
    f = io.BytesIO(img_data)
    return send_file(f, mimetype='image/' + imgtype)


@app.route('/files/<int:file_id>/<file_filename>')
def get_file(file_id, file_filename):
    timdb = getTimDb()
    if not timdb.files.fileExists(file_id, file_filename):
        abort(404)
    verify_view_access(file_id)
    img_data = timdb.files.getFile(file_id, file_filename)
    f = io.BytesIO(img_data)
    return send_file(f)


@app.route('/images')
def get_all_images():
    timdb = getTimDb()
    images = timdb.images.getImages()
    allowedImages = [image for image in images if timdb.users.has_view_access(getCurrentUserId(), image['id'])]
    return jsonResponse(allowedImages)


# Route to get info from lectures. Gives answers, and messages and other necessary info.
@app.route('/getLectureInfo')
def get_lecture_info():
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


# Route to get all the messages from some lecture.
# Tulisi hakea myös kaikki aukiolevat kysymykset, joihin käyttäjä ei ole vielä vastannut.
@app.route('/getAllMessages')
def get_all_messages(param_lecture_id=-1):
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


# Gets updates from some lecture. Checks updates in 1 second frequently and answers if there is updates.
@app.route('/getUpdates')
def get_updates():
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


# Route to use to get question manually (instead of getting question in /getUpdates)
@app.route('/getQuestionManually')
def get_question_manually():
    if not request.args.get('lecture_id'):
        abort(400, "Bad request")
    lecture_id = int(request.args.get('lecture_id'))
    new_question = get_new_question(lecture_id, None, None, True)
    return jsonResponse(new_question)


def get_new_question(lecture_id, current_question_id=None, current_points_id=None, force=False):
    """
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


# Checks if the lecture is about to end. 1 -> ends in 1 min. 5 -> ends in 5 min. 100 -> goes on atleast for 5 mins.
def check_if_lecture_is_ending(current_user, timdb, lecture_id):
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


# Route to add message to database.
@app.route('/sendMessage', methods=['POST'])
def send_message():
    timdb = getTimDb()
    new_message = request.args.get("message")
    lecture_id = int(request.args.get("lecture_id"))

    new_timestamp = str(datetime.datetime.now())
    msg_id = timdb.messages.add_message(getCurrentUserId(), lecture_id, new_message, new_timestamp, True)
    return jsonResponse(msg_id)


# Route to get specific question
@app.route('/getQuestion')
def get_question():
    doc_id = request.args.get('doc_id')
    par_index = request.args.get('par_index')
    timdb = getTimDb()
    question = timdb.questions.get_paragraphs_question(doc_id, par_index)
    return jsonResponse(question)


# Route to get all questions
@app.route('/getQuestions', methods=['GET'])
def get_all_questions():
    timdb = getTimDb()
    questions = timdb.questions.get_questions()
    return jsonResponse(questions)


# Route to get add question to database
@app.route('/addQuestion/', methods=['POST'])
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


# Route to check if the current user is in some lecture in specific document
@app.route('/checkLecture', methods=['GET'])
def check_lecture():
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


# Route to start lecture that's start time is in future
@app.route("/startFutureLecture", methods=['POST'])
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


# Route to get all the lectures from document
@app.route('/getAllLecturesFromDocument', methods=['GET'])
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


# Route to get show lecture info of some specific lecture
@app.route('/showLectureInfo/<int:lecture_id>', methods=['GET'])
def show_lecture_info(lecture_id):
    timdb = getTimDb()
    lecture = timdb.lectures.get_lecture(lecture_id)
    if len(lecture) <= 0:
        abort(400)

    lecture = lecture[0]
    doc = timdb.documents.get_document(lecture.get('doc_id'))
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
                           rights=get_rights(doc['id']))


# Route to get show lecture info of some specific lecture
@app.route('/showLectureInfoGivenName/', methods=['GET'])
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


# Route to check if certain lecture needs password
@app.route('/lectureNeedsPassword/', methods=['GET'])
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


# Gets users from specific lecture
# returns 2 lists of dictionaries.
# TODO: Think if it would be better to return only one
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


# Checks if some lecture is running or not.
def check_if_lecture_is_running(lecture_id):
    timdb = getTimDb()
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    return timdb.lectures.check_if_lecture_is_running(lecture_id, time_now)


# Checks if some lecture is full.
def check_if_lecture_is_full(lecture_id):
    timdb = getTimDb()
    return timdb.lectures.check_if_lecture_is_full(lecture_id)


# Gets all lectures that are currently running. Also gives the ones that are in the future
def get_running_lectures(doc_id=None):
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


# Route to create lecture.
@app.route('/createLecture', methods=['POST'])
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


# Route to end lecture
@app.route('/endLecture', methods=['POST'])
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


# Cleans dictionaries from lecture that isn't running anymore
def clean_dictionaries_by_lecture(lecture_id):
    tempdb = getTempDb()
    tempdb.runningquestions.delete_lectures_running_questions(lecture_id)
    tempdb.usersshown.delete_all_from_lecture(lecture_id)
    tempdb.usersextended.delete_all_from_lecture(lecture_id)
    tempdb.useractivity.delete_lecture_activity(lecture_id)
    tempdb.newanswers.delete_lecture_answers(lecture_id)
    tempdb.showpoints.stop_showing_points(lecture_id)
    tempdb.pointsshown.delete_all_from_lecture(lecture_id)


# Route to extend lecture
@app.route('/extendLecture', methods=['POST'])
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


# Route to delete lecture.
@app.route('/deleteLecture', methods=['POST'])
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


# Route to join lecture. Checks that the given password is correct.
@app.route('/joinLecture', methods=['POST'])
def join_lecture():
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


# Route to leave lecture
@app.route('/leaveLecture', methods=['POST'])
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


@app.route('/uploads/<filename>')
def uploaded_file(filename):
    return send_from_directory(app.config['UPLOAD_FOLDER'], filename)


@app.route("/getDocuments")
def get_documents():
    timdb = getTimDb()
    docs = timdb.documents.get_documents()
    allowed_docs = [doc for doc in docs if timdb.users.has_view_access(getCurrentUserId(), doc['id'])]

    req_folder = request.args.get('folder')
    if req_folder is not None and len(req_folder) == 0:
        req_folder = None
    final_docs = []

    for doc in allowed_docs:
        fullname = doc['name']

        if req_folder:
            if not fullname.startswith(req_folder + '/'):
                continue
            docname = fullname[len(req_folder) + 1:]
        else:
            docname = fullname

        if '/' in docname:
            continue

        uid = getCurrentUserId()
        doc['name'] = docname
        doc['fullname'] = fullname
        doc['canEdit'] = timdb.users.has_edit_access(uid, doc['id'])
        doc['isOwner'] = timdb.users.userIsOwner(getCurrentUserId(), doc['id']) or timdb.users.has_admin_access(uid)
        doc['owner'] = timdb.users.getOwnerGroup(doc['id'])
        final_docs.append(doc)

    final_docs.sort(key=lambda d: d['name'].lower())
    return jsonResponse(final_docs)


@app.route("/getFolders")
def get_folders():
    root_path = request.args.get('root_path')
    timdb = getTimDb()
    folders = timdb.folders.get_folders(root_path)
    allowed_folders = [f for f in folders if timdb.users.has_view_access(getCurrentUserId(), f['id'])]
    uid = getCurrentUserId()

    for f in allowed_folders:
        f['isOwner'] = timdb.users.userIsOwner(uid, f['id']) or timdb.users.has_admin_access(uid)
        f['owner'] = timdb.users.getOwnerGroup(f['id'])

    allowed_folders.sort(key=lambda f: f['name'].lower())
    return jsonResponse(allowed_folders)


def create_item(item_name, item_type, create_function, owner_group_id):
    if not logged_in():
        abort(403, 'You have to be logged in to create a {}.'.format(item_type))

    if item_name is not None:
        if item_name.startswith('/') or item_name.endswith('/'):
            abort(400, 'The {} name cannot start or end with /.'.format(item_type))

        if re.match('^(\d)*$', item_name) is not None:
            abort(400, 'The {} name can not be a number to avoid confusion with document id.'.format(item_type))

    timdb = getTimDb()
    username = getCurrentUserName()

    if item_name is not None:
        if timdb.documents.get_document_id(item_name) is not None or timdb.folders.get_folder_id(item_name) is not None:
            abort(403, 'Item with a same name already exists.')

        if not canWriteToFolder(item_name):
            abort(403, 'You cannot create {}s in this folder. Try users/{} instead.'.format(item_type, username))

        item_path, _ = timdb.folders.split_location(item_name)
        folder_id = timdb.folders.create(item_path, owner_group_id)

    item_id = create_function(item_name, owner_group_id)
    return jsonResponse({'id': item_id, 'name': item_name})


@app.route("/createDocument", methods=["POST"])
def create_document():
    jsondata = request.get_json()
    doc_name = jsondata['doc_name']

    timdb = getTimDb()
    return create_item(doc_name, 'document', lambda name, group: timdb.documents.create(name, group).doc_id,
                       getCurrentUserGroup())


@app.route("/translations/<int:doc_id>", methods=["GET"])
def get_translations(doc_id):
    timdb = getTimDb()

    if not timdb.documents.exists(doc_id):
        abort(404, 'Document not found')
    if not has_view_access(doc_id):
        abort(403, 'Permission denied')

    trlist = timdb.documents.get_translations(doc_id)
    for tr in trlist:
        tr['owner'] = timdb.users.get_user_group_name(tr['owner_id']) if tr['owner_id'] else None

    return jsonResponse(trlist)


def valid_language_id(lang_id):
    return re.match('^\w+$', lang_id) is not None


@app.route("/translate/<int:tr_doc_id>/<language>", methods=["POST"])
def create_translation(tr_doc_id, language):
    title = request.get_json().get('doc_title', None)
    timdb = getTimDb()

    doc_id = timdb.documents.get_translation_source(tr_doc_id)

    if not timdb.documents.exists(doc_id):
        abort(404, 'Document not found')

    if not has_view_access(doc_id):
        abort(403, 'Permission denied')
    if not valid_language_id(language):
        abort(404, 'Invalid language identifier')
    if timdb.documents.translation_exists(doc_id, lang_id=language):
        abort(403, 'Translation already exists')
    if not logged_in():
        # todo: check for translation right
        abort(403, 'You have to be logged in to create a translation')

    src_doc = Document(doc_id)
    doc = timdb.documents.create_translation(src_doc, None, getCurrentUserGroup())
    timdb.documents.add_translation(doc.doc_id, src_doc.doc_id, language, title)

    src_doc_name = timdb.documents.get_first_document_name(src_doc.doc_id)
    doc_name = timdb.documents.get_translation_path(doc_id, src_doc_name, language)

    return jsonResponse({'id': doc.doc_id, 'title': title, 'name': doc_name})


@app.route("/translation/<int:doc_id>", methods=["POST"])
def update_translation(doc_id):
    (lang_id, doc_title) = verify_json_params('new_langid', 'new_title', require=True)
    timdb = getTimDb()

    src_doc_id = None
    translations = timdb.documents.get_translations(doc_id)
    for tr in translations:
        if tr['id'] == doc_id:
            src_doc_id = tr['src_docid']
        if tr['lang_id'] == lang_id and tr['id'] != doc_id:
            abort(403, 'Translation ' + lang_id + ' already exists')

    if src_doc_id is None or not timdb.documents.exists(src_doc_id):
        abort(404)

    if not valid_language_id(lang_id):
        abort(403, 'Invalid language identifier')

    if not has_ownership(src_doc_id) and not has_ownership(doc_id):
        abort(403, "You need ownership of either this or the translated document")

    # Remove and add because we might be adding a language identifier for the source document
    # In that case there may be nothing to update!
    timdb.documents.remove_translation(doc_id, commit=False)
    timdb.documents.add_translation(doc_id, src_doc_id, lang_id, doc_title)
    return okJsonResponse()


@app.route("/cite/<int:docid>/<path:newname>", methods=["GET"])
def create_citation_doc(docid, newname):
    params = request.get_json()

    # Filter for allowed reference parameters
    if params is not None:
        params = {k: params[k] for k in params if k in ('r', 'r_docid')}
        params['r'] = 'c'
    else:
        params = {'r': 'c'}

    timdb = getTimDb()
    if not has_view_access(docid):
        abort(403)

    src_doc = Document(docid)
    factory = lambda name, group: timdb.documents.create_translation(src_doc, name, group, params).doc_id
    return create_item(newname, 'document', factory, getCurrentUserGroup())


@app.route("/createFolder", methods=["POST"])
def create_folder():
    jsondata = request.get_json()
    folder_name = jsondata['name']
    owner_id = jsondata['owner']
    timdb = getTimDb()
    return create_item(folder_name, 'folder', timdb.folders.create, owner_id)


@app.route("/getBlock/<int:doc_id>/<par_id>")
def get_block(doc_id, par_id):
    verify_edit_access(doc_id)
    area_start = request.args.get('area_start')
    area_end = request.args.get('area_end')
    if area_start and area_end:
        return jsonResponse({"text": Document(doc_id).export_section(area_start, area_end)})
    else:
        par = Document(doc_id).get_paragraph(par_id)
        return jsonResponse({"text": par.get_exported_markdown()})


@app.route("/<plugin>/<filename>")
def plugin_call(plugin, filename):
    try:
        req = containerLink.call_plugin_resource(plugin, filename)
        return Response(stream_with_context(req.iter_content()), content_type=req.headers['content-type'])
    except PluginException:
        abort(404)


@app.route("/index/<int:doc_id>")
def get_index(doc_id):
    verify_view_access(doc_id)
    index = Document(doc_id).get_index()
    if not index:
        return jsonResponse({'empty': True})
    else:
        return render_template('content.html',
                               headers=index)


@app.route("/<plugin>/template/<template>/<index>")
def view_template(plugin, template, index):
    try:
        req = containerLink.call_plugin_resource(plugin, "template?file=" + template + "&idx=" + index)
        return Response(stream_with_context(req.iter_content()), content_type=req.headers['content-type'])
    except PluginException:
        abort(404)


@app.route("/sessionsetting/<setting>/<value>", methods=['POST'])
def set_session_setting(setting, value):
    try:
        if 'settings' not in session:
            session['settings'] = {}
        session['settings'][setting] = value
        session.modified = True
        return jsonResponse(session['settings'])
    except (NameError, KeyError):
        abort(404)


def get_document(doc_id, doc_ver):
    """

    :type doc_ver: str
    :type doc_id: int
    :rtype: Document
    """
    return Document(doc_id=doc_id, modifier_group_id=getCurrentUserGroup())


@app.route("/postNote", methods=['POST'])
def post_note():
    jsondata = request.get_json()
    note_text = jsondata['text']
    access = jsondata['access']
    sent_tags = jsondata.get('tags', {})
    tags = []
    for tag in KNOWN_TAGS:
        if sent_tags.get(tag):
            tags.append(tag)
    doc_id = jsondata['docId']
    doc_ver = request.headers.get('Version')
    par_id = jsondata['par']
    verify_comment_right(doc_id)
    # verify_document_version(doc_id, doc_ver)
    doc = get_document(doc_id, doc_ver)
    par = doc.get_paragraph(par_id)
    if par is None:
        abort(400, 'Non-existent paragraph')
    timdb = getTimDb()
    group_id = getCurrentUserGroup()

    if par.get_attr('r') != 'tr':
        par = get_referenced_pars_from_req(par)[0]

    timdb.notes.addNote(group_id, Document(par.get_doc_id()), par, note_text, access, tags)
    doc = Document(doc_id)
    return par_response([doc.get_paragraph(par_id)],
                        doc)


@app.route("/editNote", methods=['POST'])
def edit_note():
    verifyLoggedIn()
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['docId'])
    verify_view_access(doc_id, getCurrentUserGroup())
    note_text = jsondata['text']
    access = jsondata['access']
    par_id = jsondata['par']
    note_id = int(jsondata['id'])
    sent_tags = jsondata.get('tags', {})
    tags = []
    for tag in KNOWN_TAGS:
        if sent_tags[tag]:
            tags.append(tag)
    timdb = getTimDb()
    if not (timdb.notes.hasEditAccess(group_id, note_id)
            or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to edit this note.")
    timdb.notes.modifyNote(note_id, note_text, access, tags)
    doc = Document(doc_id)
    return par_response([doc.get_paragraph(par_id)],
                        doc)


@app.route("/deleteNote", methods=['POST'])
def delete_note():
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['docId'])
    note_id = int(jsondata['id'])
    paragraph_id = jsondata['par']
    timdb = getTimDb()
    if not (timdb.notes.hasEditAccess(group_id, note_id)
            or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to remove this note.")
    timdb.notes.deleteNote(note_id)
    doc = Document(doc_id)
    return par_response([doc.get_paragraph(paragraph_id)],
                        doc)


@app.route("/getServerTime", methods=['GET'])
def get_server_time():
    t2 = int(time.time() * 1000)
    t1 = int(request.args.get('t1'))
    return jsonResponse({'t1': t1, 't2': t2, 't3': int(time.time() * 1000)})


@app.route("/questions/<int:doc_id>")
def get_questions(doc_id):
    verify_ownership(doc_id)
    timdb = getTimDb()
    questions = timdb.questions.get_doc_questions(doc_id)
    return jsonResponse(questions)


@app.route("/getLectureWithName", methods=['POST'])
def get_lecture_with_name(lecture_code, doc_id):
    verify_ownership(doc_id)
    timdb = getTimDb()
    lecture = timdb.lectures.get_lecture_by_code(lecture_code, doc_id)
    return jsonResponse(lecture)


@app.route("/extendQuestion", methods=['POST'])
def extend_question():
    lecture_id = int(request.args.get('lecture_id'))
    asked_id = int(request.args.get('asked_id'))
    extend = int(request.args.get('extend'))

    tempdb = getTempDb()
    tempdb.runningquestions.extend_question(asked_id, extend * 1000)

    return jsonResponse('Extended')


@app.route("/askQuestion", methods=['POST'])
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
    tempdb = getTempDb()
    tempdb.runningquestions.delete_lectures_running_questions(lecture_id)
    tempdb.usersshown.delete_all_from_question(asked_id)
    tempdb.usersextended.delete_all_from_question(asked_id)
    tempdb.newanswers.delete_question_answers(asked_id)
    tempdb.showpoints.stop_showing_points(lecture_id)
    tempdb.pointsshown.delete_all_from_lecture(lecture_id)
    tempdb.pointsclosed.delete_all_from_lecture(lecture_id)


@app.route('/showAnswerPoints', methods=['POST'])
def show_points():
    if 'asked_id' not in request.args or 'lecture_id' not in request.args:
        abort("400")
    asked_id = int(request.args.get('asked_id'))
    lecture_id = int(request.args.get('lecture_id'))

    tempdb = getTempDb()
    tempdb.showpoints.stop_showing_points(lecture_id)
    tempdb.showpoints.add_show_points(lecture_id, asked_id)

    return jsonResponse("")


# Route to get add question to database
@app.route('/updatePoints/', methods=['POST'])
def update_question_points():
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
    with app.app_context():
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


@app.route("/getQuestionById", methods=['GET'])
def get_question_by_id():
    if not request.args.get("question_id"):
        abort("400")
    # doc_id = int(request.args.get('doc_id'))
    question_id = int(request.args.get('question_id'))

    # verifyOwnership(doc_id)
    timdb = getTimDb()
    question = timdb.questions.get_question(question_id)
    return jsonResponse(question[0])


@app.route("/getAskedQuestionById", methods=['GET'])
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


# Route to stop question from running
@app.route("/stopQuestion", methods=['POST'])
def stop_question():
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


@app.route("/deleteQuestion", methods=['POST'])
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


# Tämän muuttaminen long polliksi vaatii threadien poistamisen
@app.route("/getLectureAnswers", methods=['GET'])
def get_lecture_answers():
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


@app.route("/answerToQuestion", methods=['PUT'])
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


@app.route("/closePoints", methods=['PUT'])
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


@app.route("/note/<int:note_id>")
def get_note(note_id):
    timdb = getTimDb()
    note = timdb.notes.get_note(note_id)
    if not (timdb.notes.hasEditAccess(getCurrentUserGroup(), note_id)
            or timdb.users.userIsOwner(getCurrentUserId(), note['doc_id'])):
        abort(403)
    note.pop('UserGroup_id')
    tags = note['tags']
    note['tags'] = {}
    for tag in KNOWN_TAGS:
        note['tags'][tag] = tag in tags
    return jsonResponse({'text': note['content'], 'extraData': note})


@app.route("/read/<int:doc_id>", methods=['GET'])
def get_read_paragraphs(doc_id):
    verify_read_marking_right(doc_id)
    timdb = getTimDb()
    doc = Document(doc_id)
    readings = timdb.readings.getReadings(getCurrentUserGroup(), doc)
    return jsonResponse(readings)


@app.route("/read/<int:doc_id>/<specifier>", methods=['PUT'])
def set_read_paragraph(doc_id, specifier):
    verify_read_marking_right(doc_id)
    timdb = getTimDb()
    group_id = getCurrentUserGroup()

    # todo: document versions
    # version = request.headers.get('Version', 'latest')
    # verify_document_version(doc_id, version)
    doc = Document(doc_id)
    par = doc.get_paragraph(specifier)
    if par is None:
        return abort(400, 'Non-existent paragraph')

    for par in get_referenced_pars_from_req(par):
        timdb.readings.setAsRead(group_id, Document(par.get_doc_id()), par)

    return okJsonResponse()


@app.route("/read/<int:doc_id>", methods=['PUT'])
def mark_all_read(doc_id):
    verify_read_marking_right(doc_id)
    timdb = getTimDb()
    # todo: document versions
    # version = request.headers.get('Version', 'latest')
    # verify_document_version(doc_id, version)
    doc = Document(doc_id, modifier_group_id=getCurrentUserGroup())
    timdb.readings.setAllAsRead(getCurrentUserGroup(), doc)
    return okJsonResponse()


@app.route("/")
def start_page():
    timdb = getTimDb()
    in_lecture = user_in_lecture()
    settings = get_user_settings()
    return render_template('start.html',
                           in_lecture=in_lecture,
                           settings=settings)


@app.route("/view/")
def index_page():
    timdb = getTimDb()
    current_user = getCurrentUserId()
    in_lecture = user_in_lecture()
    possible_groups = timdb.users.getUserGroupsPrintable(current_user)
    settings = get_user_settings()
    return render_template('index.html',
                           userName=getCurrentUserName(),
                           userId=current_user,
                           userGroups=possible_groups,
                           in_lecture=in_lecture,
                           settings=settings)


@app.route("/getslidestatus/")
def getslidestatus():
    if 'doc_id' not in request.args:
        abort(404, "Missing doc id")
    doc_id = int(request.args['doc_id'])
    tempdb = getTempDb()
    status = tempdb.slidestatuses.get_status(doc_id)
    if status:
        status = status.status
    else:
        status = None
    return jsonResponse(status)


@app.route("/setslidestatus")
def setslidestatus():
    print(request.args)
    if 'doc_id' not in request.args or 'status' not in request.args:
        abort(404, "Missing doc id or status")
    doc_id = int(request.args['doc_id'])
    verify_ownership(doc_id)
    status = request.args['status']
    tempdb = getTempDb()
    tempdb.slidestatuses.update_or_add_status(doc_id, status)
    return jsonResponse("")


def user_in_lecture():
    timdb = getTimDb()
    current_user = getCurrentUserId()
    in_lecture, lecture_id, = timdb.lectures.check_if_in_any_lecture(current_user)
    if in_lecture:
        in_lecture = check_if_lecture_is_running(lecture_id)
    return in_lecture


def get_user_settings():
    if 'settings' in session:
        return session['settings']
    else:
        return {}


def getTempDb():
    return models.tempdb


@app.before_request
def make_session_permanent():
    session.permanent = True


def start_app():
    if app.config['PROFILE']:
        app.wsgi_app = ProfilerMiddleware(app.wsgi_app, sort_by=('cumtime',), restrictions=[100])
    app.run(host='0.0.0.0',
            port=5000,
            use_evalex=False,
            use_reloader=False,
            threaded=not (app.config['DEBUG'] and app.config['PROFILE']))
