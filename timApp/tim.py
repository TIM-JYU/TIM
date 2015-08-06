# -*- coding: utf-8 -*-

import logging
import os
import imghdr
import io
import re
import time
import datetime
from time import mktime
import posixpath
import threading

from flask import Flask, Blueprint
from flask import stream_with_context
from flask import render_template
from flask import send_from_directory
from werkzeug.utils import secure_filename
from flask.helpers import send_file
from bs4 import UnicodeDammit

from ReverseProxied import ReverseProxied
import containerLink
from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from routes.cache import cache
from routes.answer import answers
from routes.edit import edit_page
from routes.manage import manage_page
from routes.view import view_page
from routes.slide import slide_page
from routes.login import login_page
from routes.logger import logger_bp
from timdb.timdbbase import TimDbException
from containerLink import PluginException
from routes.settings import settings_page
from routes.common import *
from documentmodel.randutils import hashfunc

app = Flask(__name__)
app.config.from_pyfile('defaultconfig.py', silent=False)
app.config.from_envvar('TIM_SETTINGS', silent=True)
default_secret = app.config['SECRET_KEY']
if not app.config.from_pyfile(app.config['SECRET_FILE_PATH'], silent=True):
    print('WARNING: secret file not found, using default values - do not run in production!')
else:
    assert default_secret != app.config['SECRET_KEY']
# Compress(app)


cache.init_app(app)

app.register_blueprint(settings_page)
app.register_blueprint(manage_page)
app.register_blueprint(edit_page)
app.register_blueprint(view_page)
app.register_blueprint(slide_page)
app.register_blueprint(login_page)
app.register_blueprint(logger_bp)
app.register_blueprint(answers)
app.register_blueprint(Blueprint('bower',
                                 __name__,
                                 static_folder='static/scripts/bower_components',
                                 static_url_path='/static/scripts/bower_components'))

print('Debug mode: {}'.format(app.config['DEBUG']))

KNOWN_TAGS = ['difficult', 'unclear']


def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


DOC_EXTENSIONS = ['txt', 'md', 'markdown']
PIC_EXTENSIONS = ['png', 'jpg', 'jpeg', 'gif']
ALLOWED_EXTENSIONS = set(PIC_EXTENSIONS + DOC_EXTENSIONS)
STATIC_PATH = "./static/"
DATA_PATH = "./static/data/"


# List to question that are being asked
__question_to_be_asked = []

# Dictionary for pull request to answers
__pull_answer = {}
# Dictionary to activities of different users
__user_activity = {}
# Dictionary for extending question time
__extend_question = {}


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
    verifyEditAccess(doc_id, "Sorry, you don't have permission to download this document.")
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
    verifyEditAccess(doc_id, "Sorry, you don't have permission to download this document.")
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
    if not loggedIn():
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
    if not timdb.users.userHasAdminAccess(getCurrentUserId()) \
            and not timdb.users.isUserInGroup(user_name, "Timppa-projektiryhmä") \
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
        timdb.users.grantViewAccess(0, img_id)  # So far everyone can see all images
        return jsonResponse({"image": str(img_id) + '/' + img_filename})
    else:
        file_id, file_filename = timdb.files.saveFile(content,
                                                      secure_filename(image_file.filename),
                                                      getCurrentUserGroup())
        timdb.users.grantViewAccess(0, file_id)  # So far everyone can see all files
        return jsonResponse({"file": str(file_id) + '/' + file_filename})


@app.route('/images/<int:image_id>/<image_filename>')
def get_image(image_id, image_filename):
    timdb = getTimDb()
    if not timdb.images.imageExists(image_id, image_filename):
        abort(404)
    verifyViewAccess(image_id)
    img_data = timdb.images.getImage(image_id, image_filename)
    imgtype = imghdr.what(None, h=img_data)
    f = io.BytesIO(img_data)
    return send_file(f, mimetype='image/' + imgtype)


@app.route('/files/<int:file_id>/<file_filename>')
def get_file(file_id, file_filename):
    timdb = getTimDb()
    if not timdb.files.fileExists(file_id, file_filename):
        abort(404)
    verifyViewAccess(file_id)
    img_data = timdb.files.getFile(file_id, file_filename)
    f = io.BytesIO(img_data)
    return send_file(f)


@app.route('/images')
def get_all_images():
    timdb = getTimDb()
    images = timdb.images.getImages()
    allowedImages = [image for image in images if timdb.users.userHasViewAccess(getCurrentUserId(), image['id'])]
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

    use_wall = False
    use_quesitions = False
    if request.args.get('get_messages') == "true":
        session['use_wall'] = True
        use_wall = True
    else:
        session['use_wall'] = False

    if request.args.get('get_questions') == "true":
        session['use_questions'] = True
        use_quesitions = True
    else:
        session['use_questions'] = False

    helper = request.args.get("lecture_id")
    if len(helper) > 0:
        lecture_id = int(float(helper))
    else:
        lecture_id = -1

    timdb = getTimDb()
    step = 0

    if not check_if_lecture_is_running(lecture_id):
        timdb.lectures.delete_users_from_lecture(lecture_id)
        clean_dictionaries_by_lecture(lecture_id)
        return get_running_lectures()

    list_of_new_messages = []
    last_message_id = -1

    lecturers = []
    students = []

    time_now = str(datetime.datetime.now().strftime("%H:%M:%S"))
    __user_activity[getCurrentUserId(), lecture_id] = time_now

    lecture = timdb.lectures.get_lecture(lecture_id)

    current_user = getCurrentUserId()
    lecture_ending = 100

    # Jos poistaa tämän while loopin, muuttuu long pollista perinteiseksi polliksi
    while step <= 10:
        lecturers, students = get_lecture_users(timdb, lecture_id)
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

        # Gets new questions if the questions are in use.
        if use_quesitions:
            for pair in __question_to_be_asked:
                if pair[0] == lecture_id and current_user not in pair[2]:
                    question = timdb.questions.get_asked_question(pair[1])[0]
                    question_json = timdb.questions.get_asked_json_by_id(question["asked_json_id"])[0]["json"]
                    pair[2].append(current_user)
                    lecture_ending = check_if_lecture_is_ending(current_user, timdb, lecture_id)
                    return jsonResponse(
                        {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                         "lectureId": lecture_id, "question": True, "askedId": pair[1], "asked": pair[3],
                         "questionJson": question_json,
                         "isLecture": True, "lecturers": lecturers, "students": students,
                         "lectureEnding": lecture_ending})

        if len(list_of_new_messages) > 0:
            if len(lecture) > 0 and lecture[0].get("lecturer") == current_user:
                lecture_ending = check_if_lecture_is_ending(current_user, timdb, lecture_id)
                lecturers, students = get_lecture_users(timdb, lecture_id)
            return jsonResponse(
                {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                 "lectureId": lecture_id, "isLecture": True, "lecturers": lecturers, "students": students,
                 "lectureEnding": lecture_ending})

        # Myös tämä sleep kannattaa poistaa.
        time.sleep(1)
        step += 1

    if len(lecture) > 0 and lecture[0].get("lecturer") == current_user:
        lecture_ending = check_if_lecture_is_ending(current_user, timdb, lecture_id)

    return jsonResponse(
        {"status": "no-results", "data": ["No new messages"], "lastid": client_last_id, "lectureId": lecture_id,
         "isLecture": True, "lecturers": lecturers, "students": students, "lectureEnding": lecture_ending})


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
    # verifyOwnership(doc_id)
    question_id = None
    if request.args.get('question_id'):
        question_id = int(request.args.get('question_id'))
    question_title = request.args.get('question_title')
    answer = request.args.get('answer')
    doc_id = int(request.args.get('doc_id'))
    par_id = request.args.get('par_id')
    points = request.args.get('points')
    question_json = request.args.get('questionJson')
    timdb = getTimDb()
    questions = None
    if not question_id:
        questions = timdb.questions.add_questions(doc_id, par_id, question_title, answer, question_json, points)
    else:
        questions = timdb.questions.update_question(question_id, doc_id, par_id, question_title, answer, question_json,
                                                    points)
    return jsonResponse(timdb.questions.get_question(questions)[0])


# Route to check if the current user is in some lecture in specific document
@app.route('/checkLecture', methods=['GET'])
def check_lecture():
    timdb = getTimDb()
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
                lecturers, students = get_lecture_users(timdb, lecture_id)
            else:
                is_lecturer = False

            if "use_wall" in session:
                use_wall = session['use_wall']
            else:
                use_wall = False

            if "use_questions" in session:
                use_question = session['use_questions']
            else:
                use_question = False

            return jsonResponse({"isInLecture": is_in_lecture, "lectureId": lecture_id, "lectureCode": lecture_code,
                                 "isLecturer": is_lecturer, "startTime": lecture[0].get("start_time"),
                                 "endTime": lecture[0].get("end_time"), "lecturers": lecturers, "students": students,
                                 "useWall": use_wall, "useQuestions": use_question})
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
    lecture_code = request.args.get('lecture_code')
    doc_id = int(request.args.get("doc_id"))
    verifyOwnership(doc_id)
    lecture = timdb.lectures.get_lecture_by_code(lecture_code, doc_id)
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    lecture = timdb.lectures.update_lecture_starting_time(lecture, time_now)
    timdb.lectures.join_lecture(lecture.get("lecture_id"), getCurrentUserId(), True)
    students, lecturers = get_lecture_users(timdb, lecture.get("lecture_id"))
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
    return render_template("lectureInfo.html",
                           doc=doc,
                           docId=lecture.get("doc_id"),
                           lectureId=lecture_id,
                           lectureCode=lecture.get("lecture_code"),
                           lectureStartTime=lecture.get("start_time"),
                           lectureEndTime=lecture.get("end_time"),
                           in_lecture=in_lecture)


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


# Gets users from specific lecture
# returns 2 lists of dictionaries.
# TODO: Think if it would be better to return only one
def get_lecture_users(timdb, lecture_id):
    lecture = timdb.lectures.get_lecture(lecture_id)
    lecturers = []
    students = []
    users = timdb.lectures.get_users_from_leture(lecture_id)

    if len(__user_activity) <= 0:
        for user in users:
            __user_activity[user.get("user_id"), lecture_id] = ""

    for user in users:
        if lecture[0].get("lecturer") == user.get("user_id"):
            if (user.get("user_id"), lecture_id) in __user_activity:
                lecturer = {"name": timdb.users.getUser(user.get('user_id')).get("name"),
                            "active": __user_activity[user.get("user_id"), lecture_id]}
            else:
                lecturer = {"name": timdb.users.getUser(user.get('user_id')).get("name"), "active": ""}
            lecturers.append(lecturer)

        else:
            if (user.get("user_id"), lecture_id) in __user_activity:
                student = {"name": timdb.users.getUser(user.get('user_id')).get("name"),
                           "active": __user_activity[user.get("user_id"), lecture_id]}
            else:
                student = {"name": timdb.users.getUser(user.get('user_id')).get("name"), "active": ""}
            students.append(student)

    return lecturers, students


# Checks if some lecture is running or not.
def check_if_lecture_is_running(lecture_id):
    timdb = getTimDb()
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    return timdb.lectures.check_if_lecture_is_running(lecture_id, time_now)


# Gets all lectures that are currently running. Also gives the ones that are in the future
def get_running_lectures(doc_id):
    timdb = getTimDb()
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    lecture_code = "Not running"
    list_of_lectures = timdb.lectures.get_document_lectures(doc_id, time_now)
    current_lecture_codes = []
    future_lectures = []
    is_lecturer = hasOwnership(doc_id)
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
    verifyOwnership(doc_id)
    timdb = getTimDb()
    start_time = request.args.get("start_date")
    end_time = request.args.get("end_date")
    lecture_code = request.args.get("lecture_code")
    password = request.args.get("password")
    if not password:
        password = ""
    current_user = getCurrentUserId()
    if not timdb.lectures.check_if_correct_name(doc_id, lecture_code, lecture_id):
        abort(400, "Can't create two or more lectures with the same name to the same document.")
    if lecture_id < 0:
        lecture_id = timdb.lectures.create_lecture(doc_id, current_user, start_time, end_time, lecture_code, password,
                                                   True)
    else:
        timdb.lectures.update_lecture(lecture_id, doc_id, current_user, start_time, end_time, lecture_code, password)

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
    verifyOwnership(doc_id)
    timdb = getTimDb()
    timdb.lectures.delete_users_from_lecture(lecture_id)

    now = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    timdb.lectures.set_end_for_lecture(lecture_id, str(now))

    clean_dictionaries_by_lecture(lecture_id)

    return get_running_lectures(doc_id)


# Cleans dictionaries from lecture that isn't running anymore
def clean_dictionaries_by_lecture(lecture_id):
    entries_to_remove = []
    pulls_to_remove = []
    for pair in __question_to_be_asked:
        if pair[0] == lecture_id:
            __question_to_be_asked.remove(pair)

    for actitivty in __user_activity:
        if actitivty[1] == lecture_id:
            entries_to_remove.append(actitivty)

    for actitivty in entries_to_remove:
        del __user_activity[actitivty]

    for pullRequest in __pull_answer:
        if pullRequest[1] == lecture_id:
            pulls_to_remove.append(pullRequest)

    for pullRequest in pulls_to_remove:
        del __pull_answer[pullRequest]


# Route to extend lecture
@app.route('/extendLecture', methods=['POST'])
def extend_lecture():
    if not request.args.get("doc_id") or not request.args.get("lecture_id") or not request.args.get("new_end_time"):
        abort(400)
    doc_id = int(request.args.get("doc_id"))
    lecture_id = int(request.args.get("lecture_id"))
    new_end_time = request.args.get("new_end_time")
    verifyOwnership(doc_id)
    timdb = getTimDb()
    timdb.lectures.extend_lecture(lecture_id, new_end_time)
    return jsonResponse("")


# Route to delete lecture.
@app.route('/deleteLecture', methods=['POST'])
def delete_lecture():
    if not request.args.get("doc_id") or not request.args.get("lecture_id"):
        abort(400)
    doc_id = int(request.args.get("doc_id"))
    verifyOwnership(doc_id)
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
    doc_id = int(request.args.get("doc_id"))
    lecture_code = request.args.get("lecture_code")
    password_quess = request.args.get("password_quess")
    lecture_id = timdb.lectures.get_lecture_by_code(lecture_code, doc_id)
    current_user = getCurrentUserId()

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
    timdb.lectures.join_lecture(lecture_id, current_user, True)

    time_now = str(datetime.datetime.now().strftime("%H:%M:%S"))
    __user_activity[getCurrentUserId(), lecture_id] = time_now

    session['in_lecture'] = [lecture_id]

    lecturers = []
    students = []
    if lecture[0].get("lecturer") == current_user:
        is_lecturer = True
        lecturers, students = get_lecture_users(timdb, lecture_id)
    else:
        is_lecturer = False
    return jsonResponse(
        {"correctPassword": True, "inLecture": True, "lectureId": lecture_id, "isLecturer": is_lecturer,
         "lectureCode": lecture_code, "startTime": lecture[0].get("start_time"),
         "endTime": lecture[0].get("end_time"), "lecturers": lecturers, "students": students})


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
    if __user_activity[current_user, lecture_id]:
        del __user_activity[current_user, lecture_id]


@app.route('/uploads/<filename>')
def uploaded_file(filename):
    return send_from_directory(app.config['UPLOAD_FOLDER'], filename)


@app.route("/getDocuments")
def get_documents():
    timdb = getTimDb()
    docs = timdb.documents.get_documents()
    allowed_docs = [doc for doc in docs if timdb.users.userHasViewAccess(getCurrentUserId(), doc['id'])]

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
        doc['canEdit'] = timdb.users.userHasEditAccess(uid, doc['id'])
        doc['isOwner'] = timdb.users.userIsOwner(getCurrentUserId(), doc['id']) or timdb.users.userHasAdminAccess(uid)
        doc['owner'] = timdb.users.getOwnerGroup(doc['id'])
        final_docs.append(doc)

    return jsonResponse(final_docs)


@app.route("/getFolders")
def get_folders():
    root_path = request.args.get('root_path')
    timdb = getTimDb()
    folders = timdb.folders.get_folders(root_path)
    allowed_folders = [f for f in folders if timdb.users.userHasViewAccess(getCurrentUserId(), f['id'])]
    uid = getCurrentUserId()

    for f in allowed_folders:
        f['isOwner'] = timdb.users.userIsOwner(uid, f['id']) or timdb.users.userHasAdminAccess(uid)
        f['owner'] = timdb.users.getOwnerGroup(f['id'])

    return jsonResponse(allowed_folders)


def create_item(item_name, item_type, create_function, owner_group_id):
    if not loggedIn():
        abort(403, 'You have to be logged in to create a {}.'.format(item_type))

    if item_name.startswith('/') or item_name.endswith('/'):
        abort(400, 'The {} name cannot start or end with /.'.format(item_type))

    if re.match('^(\d)*$', item_name) is not None:
        abort(400, 'The {} name can not be a number to avoid confusion with document id.'.format(item_type))

    timdb = getTimDb()
    username = getCurrentUserName()

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
    return create_item(doc_name, 'document', timdb.documents.create, getCurrentUserGroup())


@app.route("/createFolder", methods=["POST"])
def create_folder():
    jsondata = request.get_json()
    folder_name = jsondata['name']
    owner_id = jsondata['owner']
    timdb = getTimDb()
    return create_item(folder_name, 'folder', timdb.folders.create, owner_id)


@app.route("/getBlock/<int:doc_id>/<par_id>")
def get_block(doc_id, par_id):
    verifyEditAccess(doc_id)
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
    verifyViewAccess(doc_id)
    index = Document(doc_id).get_index()
    return jsonResponse(index)


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


def get_paragraph(doc, par_id):
    """

    :type doc: Document
    :type par_id: str
    :rtype: DocParagraph
    """
    if not doc.has_paragraph(par_id):
        return None
    return DocParagraph(doc_id=doc.doc_id, par_id=par_id)


@app.route("/postNote", methods=['POST'])
def post_note():
    jsondata = request.get_json()
    note_text = jsondata['text']
    access = jsondata['access']
    sent_tags = jsondata.get('tags', {})
    tags = []
    for tag in KNOWN_TAGS:
        if sent_tags[tag]:
            tags.append(tag)
    doc_id = jsondata['docId']
    doc_ver = request.headers.get('Version')
    paragraph_id = jsondata['par']
    verifyCommentRight(doc_id)
    # verify_document_version(doc_id, doc_ver)
    doc = get_document(doc_id, doc_ver)
    par = get_paragraph(doc, paragraph_id)
    if par is None:
        abort(400, 'Non-existent paragraph')
    timdb = getTimDb()
    group_id = getCurrentUserGroup()
    timdb.notes.addNote(group_id, doc, par, note_text, access, tags)
    return okJsonResponse()


@app.route("/editNote", methods=['POST'])
def edit_note():
    verifyLoggedIn()
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['docId'])
    note_text = jsondata['text']
    access = jsondata['access']
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
    return okJsonResponse()


@app.route("/deleteNote", methods=['POST'])
def delete_note():
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['docId'])  # TODO: maybe not needed
    note_id = int(jsondata['id'])
    timdb = getTimDb()
    if not (timdb.notes.hasEditAccess(group_id, note_id)
            or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to remove this note.")
    timdb.notes.deleteNote(note_id)
    return okJsonResponse()


@app.route("/getServerTime", methods=['GET'])
def get_server_time():
    return jsonResponse(int(time.time() * 1000))


@app.route("/questions/<int:doc_id>")
def get_questions(doc_id):
    verifyOwnership(doc_id)
    timdb = getTimDb()
    questions = timdb.questions.get_doc_questions(doc_id)
    return jsonResponse(questions)


@app.route("/getLectureWithName", methods=['POST'])
def get_lecture_with_name(lecture_code, doc_id):
    verifyOwnership(doc_id)
    timdb = getTimDb()
    lecture = timdb.lectures.get_lecture_by_code(lecture_code, doc_id)
    return jsonResponse(lecture)


@app.route("/extendQuestion", methods=['POST'])
def extend_question():
    lecture_id = int(request.args.get('lecture_id'))
    asked_id = int(request.args.get('asked_id'))
    extend = int(request.args.get('extend'))
    for q in __question_to_be_asked:
        if q[0] == lecture_id and q[1] == asked_id:
            question = (q[0], q[1], q[2], q[3], q[4] + extend * 1000)
            __question_to_be_asked.append(question)
            __question_to_be_asked.remove(q)
            __extend_question[question[0], question[1]].set()
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

    verifyOwnership(doc_id)

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
                                                       asked_json_id)
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

    verifyOwnership(int(doc_id))
    __question_to_be_asked.append((lecture_id, asked_id, [], ask_time, end_time))

    return jsonResponse(asked_id)


# Route to get add question to database
@app.route('/updatePoints/', methods=['POST'])
def update_question_points():
    # TODO: Only lecturers should be able to create questions.
    # verifyOwnership(doc_id)
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
    if question_timelimit == 0:
        return

    # Adding extra time to limit so when people gets question a bit later than others they still get to answer
    # TODO: If current implementation changes the way that the question last 10 seconds and after that you can't
    # TODO: answer. Remove this part
    extra_time = 3
    end_time += extra_time * 1000
    while int(time.time() * 1000) < end_time:
        time.sleep(1)
        stopped = True
        for question in __question_to_be_asked:
            if question[0] == lecture_id and question[1] == asked_id:
                end_time = extra_time * 1000 + question[4]
                stopped = False
                break
        if stopped:
            return

    for question in __question_to_be_asked:
        if question[0] == lecture_id and question[1] == asked_id:
            __question_to_be_asked.remove(question)


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


@app.route("/stopQuestion", methods=['POST'])
def stop_question():
    if not request.args.get("asked_id") or not request.args.get("lecture_id"):
        abort("400")
    asked_id = int(request.args.get('asked_id'))
    lecture_id = int(request.args.get('lecture_id'))
    timdb = getTimDb()
    current_user = getCurrentUserId()
    lecture = timdb.lectures.get_lecture(lecture_id)
    if lecture:
        if lecture[0].get("lecturer") != current_user:
            abort("400", "You cannot stop questions on someone elses lecturer.")
        for question in __question_to_be_asked:
            if question[0] == lecture_id and question[1] == asked_id:
                __question_to_be_asked.remove(question)
    return jsonResponse("")


@app.route("/deleteQuestion", methods=['POST'])
def delete_question():
    if not request.args.get("question_id") or not request.args.get('doc_id'):
        abort("400")

    doc_id = int(request.args.get('doc_id'))
    question_id = int(request.args.get('question_id'))

    verifyOwnership(doc_id)
    timdb = getTimDb()
    timdb.questions.delete_question(question_id)
    timdb.lecture_answers.delete_answers_from_question(question_id)

    return jsonResponse("")


# Tämän muuttaminen long polliksi vaatii threadien poistamisen
@app.route("/getExtendQuestion", methods=['GET'])
def get_extend_question():
    if not request.args.get('asked_id') or not request.args.get('lecture_id'):
        abort(400, "Bad request")

    asked_id = int(request.args.get('asked_id'))
    lecture_id = int(request.args.get('lecture_id'))

    __extend_question[lecture_id, asked_id] = threading.Event()

    for extend in __extend_question:
        lecture, question = extend
        if lecture == lecture_id and question != asked_id:
            __extend_question[extend].set()

    """
    if not request.args.get("time"):
        time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f"))
    else:
        time_now = request.args.get('time')
    """
    __extend_question[lecture_id, asked_id].wait(5)
    endtime = None
    for q in __question_to_be_asked:
        if q[0] == lecture_id and q[1] == asked_id:
            endtime = q[4]
            break
    return jsonResponse(endtime)


# Tämän muuttaminen long polliksi vaatii threadien poistamisen
@app.route("/getLectureAnswers", methods=['GET'])
def get_lecture_answers():
    if not request.args.get('asked_id') or not request.args.get('doc_id') or not request.args.get('lecture_id'):
        abort(400, "Bad request")

    verifyOwnership(int(request.args.get('doc_id')))
    asked_id = int(request.args.get('asked_id'))
    lecture_id = int(request.args.get('lecture_id'))

    __pull_answer[asked_id, lecture_id] = threading.Event()

    for pull in __pull_answer:
        question, lecture = pull
        if lecture == lecture_id and question != asked_id:
            __pull_answer[pull].set()

    if not request.args.get("time"):
        time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f"))
    else:
        time_now = request.args.get('time')

    __pull_answer[asked_id, lecture_id].wait()

    timdb = getTimDb()
    lecture_answers = timdb.lecture_answers.get_answers_to_question(asked_id, time_now)
    if len(lecture_answers) <= 0:
        return jsonResponse({"noAnswer": True})

    latest_answer = lecture_answers[-1].get("answered_on")

    return jsonResponse({"answers": lecture_answers, "askedId": asked_id, "latestAnswer": latest_answer})


@app.route("/answerToQuestion", methods=['PUT'])
def answer_to_question():
    if not request.args.get("asked_id") or not request.args.get('answers') or not request.args.get('lecture_id'):
        abort(400, "Bad request")

    timdb = getTimDb()

    asked_id = int(request.args.get("asked_id"))
    answer = request.args.get("answers")
    whole_answer = answer
    lecture_id = int(request.args.get("lecture_id"))

    lecture_answer = timdb.lecture_answers.get_user_answer_to_question(asked_id, getCurrentUserId())

    question_ended = True
    for question in __question_to_be_asked:
        if question[0] == lecture_id and question[1] == asked_id:
            question_ended = False

    if question_ended:
        return jsonResponse({"questionLate": "The question has already finished. Your answer was not saved."})

    if (not lecture_answer) or (lecture_answer and answer != lecture_answer[0]["answer"]):
        time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f"))
        question_points = timdb.questions.get_asked_question(asked_id)[0].get("points")
        points_table = create_points_table(question_points)
        points = calculate_points(answer, points_table)
        current_user = getCurrentUserId()
        if lecture_answer and current_user != 0:
            timdb.lecture_answers.update_answer(lecture_answer[0]["answer_id"], current_user, asked_id,
                                                lecture_id, whole_answer, time_now, points)
        else:
            timdb.lecture_answers.add_answer(current_user, asked_id, lecture_id, whole_answer, time_now, points)
        __pull_answer[asked_id, lecture_id].set()

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


@app.route("/notes/<int:doc_id>")
def get_notes(doc_id):
    verifyViewAccess(doc_id)
    timdb = getTimDb()
    group_id = getCurrentUserGroup()
    doc = Document(doc_id)
    notes = [note for note in timdb.notes.getNotes(group_id, doc)]
    for note in notes:
        note['editable'] = note['UserGroup_id'] == group_id or timdb.users.userIsOwner(getCurrentUserId(), doc_id)
        note.pop('UserGroup_id')
        note['private'] = note['access'] == 'justme'
        tags = note['tags']
        note['tags'] = {}
        for tag in KNOWN_TAGS:
            note['tags'][tag] = tag in tags
    return jsonResponse(notes)


@app.route("/read/<int:doc_id>", methods=['GET'])
def get_read_paragraphs(doc_id):
    verifyReadMarkingRight(doc_id)
    timdb = getTimDb()
    doc = Document(doc_id)
    readings = timdb.readings.getReadings(getCurrentUserGroup(), doc)
    return jsonResponse(readings)


@app.route("/read/<int:doc_id>/<specifier>", methods=['PUT'])
def set_read_paragraph(doc_id, specifier):
    verifyReadMarkingRight(doc_id)
    timdb = getTimDb()
    # todo: document versions
    # version = request.headers.get('Version', 'latest')
    # verify_document_version(doc_id, version)
    doc = Document(doc_id)
    par = doc.get_paragraph(specifier)
    if par is None:
        return abort(400, 'Non-existent paragraph')
    timdb.readings.setAsRead(getCurrentUserGroup(), doc, par)
    return okJsonResponse()


@app.route("/read/<int:doc_id>", methods=['PUT'])
def mark_all_read(doc_id):
    verifyReadMarkingRight(doc_id)
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
    return render_template('start.html',
                           in_lecture=in_lecture)


@app.route("/view/")
def index_page():
    timdb = getTimDb()
    current_user = getCurrentUserId()
    in_lecture = user_in_lecture()
    possible_groups = timdb.users.getUserGroupsPrintable(current_user)
    return render_template('tempindex.html',
                           userName=getCurrentUserName(),
                           userId=current_user,
                           userGroups=possible_groups,
                           in_lecture=in_lecture)


def user_in_lecture():
    timdb = getTimDb()
    current_user = getCurrentUserId()
    in_lecture, lecture_id, = timdb.lectures.check_if_in_any_lecture(current_user)
    if in_lecture:
        in_lecture = check_if_lecture_is_running(lecture_id)
    return in_lecture


@app.before_request
def make_session_permanent():
    session.permanent = True


def start_app():
    # TODO: Think if it is truly necessary to have threaded=True here
    app.wsgi_app = ReverseProxied(app.wsgi_app)
    app.run(host='0.0.0.0', port=5000, use_reloader=False, threaded=True)
