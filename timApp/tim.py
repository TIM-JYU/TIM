# -*- coding: utf-8 -*-
# Modified hajoviin
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

from flask import Flask, redirect, url_for, Blueprint
from flask import stream_with_context
from flask import render_template
from flask import send_from_directory
from werkzeug.utils import secure_filename
from flask.helpers import send_file
from bs4 import UnicodeDammit
from ReverseProxied import ReverseProxied

import containerLink
from routes.edit import edit_page
from routes.manage import manage_page
from routes.view import view_page
from routes.login import login_page
from timdb.timdbbase import TimDbException
import pluginControl
from containerLink import PluginException
from routes.settings import settings_page
from routes.common import *


app = Flask(__name__)
app.config.from_pyfile('defaultconfig.py', silent=False)
app.config.from_envvar('TIM_SETTINGS', silent=True)
# Compress(app)

app.register_blueprint(settings_page)
app.register_blueprint(manage_page)
app.register_blueprint(edit_page)
app.register_blueprint(view_page)
app.register_blueprint(login_page)
app.register_blueprint(Blueprint('bower',
                                 __name__,
                                 static_folder='static/scripts/bower_components',
                                 static_url_path='/static/scripts/bower_components'))

print('Debug mode: {}'.format(app.config['DEBUG']))

KNOWN_TAGS = ['difficult', 'unclear']

# current_app.logging.basicConfig(filename='timLog.log',level=logging.DEBUG, format='%(asctime)s %(message)s')
formatter = logging.Formatter(
    "{\"time\":%(asctime)s, \"file\": %(pathname)s, \"line\" :%(lineno)d, \"messageLevel\":  %(levelname)s, \"message\": %(message)s}")
if not os.path.exists(app.config['LOG_DIR']):
    os.mkdir(app.config['LOG_DIR'])
handler = logging.FileHandler(app.config['LOG_PATH'])
handler.setLevel(logging.DEBUG)
handler.setFormatter(formatter)
app.logger.addHandler(handler)


def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


DOC_EXTENSIONS = ['txt', 'md', 'markdown']
PIC_EXTENSIONS = ['png', 'jpg', 'jpeg', 'gif']
ALLOWED_EXTENSIONS = set(PIC_EXTENSIONS + DOC_EXTENSIONS)
STATIC_PATH = "./static/"
DATA_PATH = "./static/data/"

LOG_LEVELS = {"CRITICAL": app.logger.critical,
              "ERROR": app.logger.error,
              "WARNING": app.logger.warning,
              "INFO": app.logger.info,
              "DEBUG": app.logger.debug}

# List to question that are being asked
__question_to_be_asked = []

# Dictionary for pull request to answers
__pull_answer = {}
# Dictionary to activities of different users
__user_activity = {}

# Logger call
@app.route("/log/", methods=["POST"])
def logMessage():
    try:
        message = request.get_json()['message']
        level = request.get_json()['level']
        LOG_LEVELS[level](message)
    except KeyError:
        app.logger.error("Failed logging call: " + str(request.get_data()))


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
def documentDiff(doc_id, doc_hash):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    verifyEditAccess(doc_id, "Sorry, you don't have permission to download this document.")
    try:
        doc_diff = timdb.documents.getDifferenceToPrevious(DocIdentifier(doc_id, doc_hash))
        return render_template('diff.html', diff_html=doc_diff)
    except TimDbException as e:
        abort(404, str(e))


@app.route('/download/<int:doc_id>/<doc_hash>')
def documentHistory(doc_id, doc_hash):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    verifyEditAccess(doc_id, "Sorry, you don't have permission to download this document.")
    try:
        doc_data = timdb.documents.getDocumentMarkdown(DocIdentifier(doc_id, doc_hash))
        return Response(doc_data, mimetype="text/plain")
    except TimDbException as e:
        abort(404, str(e))


@app.route('/download/<int:doc_id>')
def downloadDocument(doc_id):
    return documentHistory(doc_id, getNewest(doc_id).hash)


@app.route('/upload/', methods=['POST'])
def upload_file():
    if request.method != 'POST':
        return jsonResponse({'message': 'Only POST method is supported.'}, 405)
    if not loggedIn():
        return jsonResponse({'message': 'You have to be logged in to upload a file.'}, 403)
    timdb = getTimDb()

    doc = request.files['file']
    folder = request.form['folder']
    filename = posixpath.join(folder, secure_filename(doc.filename))

    userName = getCurrentUserName()
    if not timdb.users.userHasAdminAccess(getCurrentUserId()) and not timdb.users.isUserInGroup(userName,
                                                                                                "Timppa-projektiryhmä") and re.match(
                            '^' + userName + '\/', filename) is None:
        return jsonResponse({'message': "You're not authorized to write here."}, 403)

    if not allowed_file(doc.filename):
        return jsonResponse({'message': 'The file format is not allowed.'}, 403)

    if (filename.endswith(tuple(DOC_EXTENSIONS))):
        content = UnicodeDammit(doc.read()).unicode_markup
        if not content:
            return jsonResponse({'message': 'Failed to convert the file to UTF-8.'}, 400)
        timdb.documents.importDocument(content, filename, getCurrentUserGroup())
        return "Successfully uploaded document"
    else:
        content = doc.read()
        imgtype = imghdr.what(None, h=content)
        if imgtype is not None:
            img_id, img_filename = timdb.images.saveImage(content, doc.filename, getCurrentUserGroup())
            timdb.users.grantViewAccess(0, img_id)  # So far everyone can see all images
            return jsonResponse({"file": str(img_id) + '/' + img_filename})
        else:
            doc.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
            return redirect(url_for('uploaded_file', filename=filename))


@app.route('/images/<int:image_id>/<image_filename>')
def getImage(image_id, image_filename):
    timdb = getTimDb()
    if not timdb.images.imageExists(image_id, image_filename):
        abort(404)
    verifyViewAccess(image_id)
    img_data = timdb.images.getImage(image_id, image_filename)
    imgtype = imghdr.what(None, h=img_data)
    f = io.BytesIO(img_data)
    return send_file(f, mimetype='image/' + imgtype)


@app.route('/images')
def getAllImages():
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
    answer_dicts = timdb.lecture_answers.get_answers_to_questions_from_lecture(lecture_id)
    question_ids = []
    answerers = []
    for singleDict in answer_dicts:
        singleDict['user_name'] = timdb.users.getUser(singleDict['user_id']).get("name")
        if singleDict['question_id'] not in question_ids:
            question_ids.append(singleDict['question_id'])
        if singleDict['user_name'] not in answerers:
            answerers.append(singleDict['user_name'])

    lecture_questions = timdb.questions.get_multiple_questions(question_ids)

    is_lecturer = False
    current_user = getCurrentUserId()
    if timdb.lectures.get_lecture(lecture_id)[0].get("lecturer") == current_user:
        is_lecturer = True

    user_name = timdb.users.getUser(current_user).get("name")

    return jsonResponse(
        {"messages": messages, "answerers": answerers, "answers": answer_dicts, "questions": lecture_questions,
         "isLecturer": is_lecturer,
         "userName": user_name})


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
    current_user = getCurrentUserId()
    for triple in __question_to_be_asked:
        if triple[0] == lecture_id and current_user not in triple[2]:
            triple[2].append(current_user)

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
            'doc_id') or not request.args.get('is_lecturer'):
        abort(400, "Bad requst")
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

    doc_id = int(request.args.get('doc_id'))

    if not check_if_lecture_is_running(lecture_id):
        timdb.lectures.delete_users_from_lecture(lecture_id)
        clean_dictionaries_by_lecture(lecture_id)
        return get_running_lectures(doc_id)

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
                    question_json = timdb.questions.get_question(pair[1])[0].get("questionJson")
                    pair[2].append(getCurrentUserId())
                    lecture_ending = check_if_lecture_is_ending(current_user, timdb, lecture_id)
                    return jsonResponse(
                        {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                         "lectureId": lecture_id, "question": True, "questionId": pair[1],
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


# Route to render question
@app.route('/lecture/question')
def show_question():
    return render_template('question.html')


# Route to render question
@app.route('/question')
def show_question_without_view():
    return render_template('question.html')


# Route to get specific question
@app.route('/getQuestion')
def get_quesition():
    doc_id = request.args.get('doc_id')
    par_index = request.args.get('par_index')
    timdb = getTimDb()
    question = timdb.questions.get_paragraphs_question(doc_id, par_index)
    return jsonResponse(question)


# Route to get all questions
@app.route('/getQuestions', methods=['GET'])
def get_questions():
    timdb = getTimDb()
    questions = timdb.questions.get_questions()
    return jsonResponse(questions)


# Route to get add question to database
@app.route('/addQuestion/', methods=['POST'])
def add_question():
    # TODO: Only lecturers should be able to create questions.
    # verifyOwnership(doc_id)
    question_id = None
    if (request.args.get('question_id')):
        question_id = int(request.args.get('question_id'))
    question_title = request.args.get('question_title')
    answer = request.args.get('answer')
    doc_id = int(request.args.get('doc_id'))
    par_index = int(request.args.get('par_index'))
    questionJson = request.args.get('questionJson')
    timdb = getTimDb()
    if not question_id:
        questions = timdb.questions.add_questions(doc_id, par_index, question_title, answer, questionJson)
    else:
        questions = timdb.questions.update_question(question_id, doc_id, par_index, question_title, answer, questionJson)
    return jsonResponse(questions)


# Route to check if the current user is in some lecture in specific document
@app.route('/checkLecture', methods=['GET'])
def check_lecture():
    if not request.args.get('doc_id'):
        abort(400)

    doc_id = int(request.args.get('doc_id'))
    timdb = getTimDb()
    current_user = getCurrentUserId()
    is_in_lecture, lecture_id, = timdb.lectures.check_if_in_lecture(doc_id, current_user)
    lecture = timdb.lectures.get_lecture(lecture_id)
    lecturers = []
    students = []
    if lecture:
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
            use_question = session['use_wall']
        else:
            use_question = False

        return jsonResponse({"isInLecture": is_in_lecture, "lectureId": lecture_id, "lectureCode": lecture_code,
                             "isLecturer": is_lecturer, "startTime": lecture[0].get("start_time"),
                             "endTime": lecture[0].get("end_time"), "lecturers": lecturers, "students": students,
                             "useWall": use_wall, "useQuestions": use_question})
    else:
        return get_running_lectures(doc_id)


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
                        "target": "/showLectureInfo/" + str(lecture.get("lecture_id"))}
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
    return render_template("lectureInfo.html",
                           docId=lecture.get("doc_id"),
                           lectureId=lecture_id,
                           lectureCode=lecture.get("lecture_code"),
                           lectureStartTime=lecture.get("start_time"),
                           lectureEndTime=lecture.get("end_time"))


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
    if lecture_id <0:
        lecture_id = timdb.lectures.create_lecture(doc_id, current_user, start_time, end_time, lecture_code, password, True)
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


# Cleans dictionaries from lecture that isn't runnin anymore
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
    lecture = timdb.lectures.get_lecture(lecture_id)
    if lecture[0].get("password") != password_quess:
        return jsonResponse({"correctPassword": False})
    timdb.lectures.join_lecture(lecture_id, current_user, True)

    time_now = str(datetime.datetime.now().strftime("%H:%M:%S"))
    __user_activity[getCurrentUserId(), lecture_id] = time_now

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


# Route to leace lecture
@app.route('/leaveLecture', methods=['POST'])
def leave_lecture():
    timdb = getTimDb()
    lecture_id = int(request.args.get("lecture_id"))
    doc_id = int(request.args.get("doc_id"))
    timdb.lectures.leave_lecture(lecture_id, getCurrentUserId(), True)
    del __user_activity[getCurrentUserId(), lecture_id]
    return get_running_lectures(doc_id)


@app.route('/uploads/<filename>')
def uploaded_file(filename):
    return send_from_directory(app.config['UPLOAD_FOLDER'], filename)


@app.route("/getDocuments")
def getDocuments():
    versions = 1
    if request.args.get('versions'):
        ver_str = request.args.get('versions')
        if re.match('^\d+$', ver_str) is None:
            return "Invalid version argument."
        else:
            ver_int = int(ver_str)
            if ver_int > 10:
                # DoS prevention
                return "Version limit is currently capped at 10."
            else:
                versions = ver_int

    timdb = getTimDb()
    docs = timdb.documents.getDocuments(historylimit=versions)
    allowedDocs = [doc for doc in docs if timdb.users.userHasViewAccess(getCurrentUserId(), doc['id'])]

    req_folder = request.args.get('folder')
    if req_folder is not None and len(req_folder) == 0:
        req_folder = None
    finalDocs = []

    for doc in allowedDocs:
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
        finalDocs.append(doc)

    return jsonResponse(finalDocs)


@app.route("/getFolders")
def getFolders():
    root_path = request.args.get('root_path')
    timdb = getTimDb()
    folders = timdb.folders.getFolders(root_path, getCurrentUserGroup())
    allowedFolders = [f for f in folders if timdb.users.userHasViewAccess(getCurrentUserId(), f['id'])]
    uid = getCurrentUserId()

    for f in allowedFolders:
        f['isOwner'] = timdb.users.userIsOwner(uid, f['id']) or timdb.users.userHasAdminAccess(uid)
        f['owner'] = timdb.users.getOwnerGroup(f['id'])

    return jsonResponse(allowedFolders)


@app.route("/getJSON/<int:doc_id>/")
def getJSON(doc_id):
    timdb = getTimDb()
    verifyViewAccess(doc_id)
    try:
        texts = timdb.documents.getDocumentBlocks(getNewest(doc_id))
        doc = timdb.documents.getDocument(doc_id.id)
        return jsonResponse({"name": doc['name'], "text": texts})
    except IOError as err:
        print(err)
        return "No data found"


@app.route("/getJSON-HTML/<int:doc_id>")
def getJSON_HTML(doc_id):
    timdb = getTimDb()
    verifyViewAccess(doc_id)
    try:
        newest = getNewest(doc_id)
        blocks = timdb.documents.getDocumentAsHtmlBlocks(newest)
        doc = timdb.documents.getDocument(doc_id)
        return jsonResponse({"name": doc['name'], "text": blocks})
    except ValueError as err:
        print(err)
        return "[]"
    except TimDbException as err:
        print(err)
        return "[]"


def createItem(itemName, itemType, createFunction):
    if not loggedIn():
        return jsonResponse({'message': 'You have to be logged in to create a {}.'.format(itemType)}, 403)

    if itemName.startswith('/') or itemName.endswith('/'):
        return jsonResponse({'message': 'The {} name cannot start or end with /.'.format(itemType)}, 400)

    if re.match('^(\d)*$', itemName) is not None:
        return jsonResponse(
            {'message': 'The {} name can not be a number to avoid confusion with document id.'.format(itemType)}, 400)

    timdb = getTimDb()

    userName = getCurrentUserName()

    if timdb.documents.getDocumentId(itemName) is not None or timdb.folders.getFolderId(itemName) is not None:
        return jsonResponse({'message': 'Item with a same name already exists.'}, 403)

    if not canWriteToFolder(itemName):
        return jsonResponse(
            {'message': 'You cannot create {}s in this folder. Try users/{} instead.'.format(itemType, userName)}, 403)

    itemId = createFunction(itemName)
    return jsonResponse({'id': itemId, 'name': itemName})


@app.route("/createDocument", methods=["POST"])
def createDocument():
    jsondata = request.get_json()
    docName = jsondata['doc_name']
    timdb = getTimDb()
    createFunc = lambda docName: timdb.documents.createDocument(docName, getCurrentUserGroup())
    return createItem(docName, 'document', createFunc)


@app.route("/createFolder", methods=["POST"])
def createFolder():
    jsondata = request.get_json()
    folderName = jsondata['name']
    ownerId = jsondata['owner']
    timdb = getTimDb()
    createFunc = lambda folderName: timdb.folders.createFolder(folderName, ownerId)
    return createItem(folderName, 'folder', createFunc)


@app.route("/getBlock/<int:docId>/<int:blockId>")
def getBlockMd(docId, blockId):
    timdb = getTimDb()
    verifyViewAccess(docId)
    block = timdb.documents.getBlock(getNewest(docId), blockId)
    return jsonResponse({"text": block})


@app.route("/getBlockHtml/<int:docId>/<int:blockId>")
def getBlockHtml(docId, blockId):
    timdb = getTimDb()
    verifyViewAccess(docId)
    block = timdb.documents.getBlockAsHtml(getNewest(docId), blockId)
    return block


@app.route("/<plugin>/<path:fileName>")
def pluginCall(plugin, fileName):
    try:
        req = containerLink.call_plugin_resource(plugin, fileName)
        return Response(stream_with_context(req.iter_content()), content_type=req.headers['content-type'])
    except PluginException:
        abort(404)


@app.route("/index/<int:docId>")
def getIndex(docId):
    timdb = getTimDb()
    verifyViewAccess(docId)
    index = timdb.documents.getIndex(getNewest(docId))
    return jsonResponse(index)


@app.route("/postNote", methods=['POST'])
def postNote():
    jsondata = request.get_json()
    noteText = jsondata['text']
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
    timdb = getTimDb()
    group_id = getCurrentUserGroup()
    timdb.notes.addNote(group_id, doc_id, doc_ver, int(paragraph_id), noteText, access, tags)
    # TODO: Handle error.
    return "Success"


@app.route("/editNote", methods=['POST'])
def editNote():
    verifyLoggedIn()
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['docId'])
    doc_ver = request.headers.get('Version')
    paragraph_id = int(jsondata['par'])
    noteText = jsondata['text']
    access = jsondata['access']
    note_index = int(jsondata['note_index'])
    sent_tags = jsondata.get('tags', {})
    tags = []
    for tag in KNOWN_TAGS:
        if sent_tags[tag]:
            tags.append(tag)
    timdb = getTimDb()

    if not (timdb.notes.hasEditAccess(group_id, doc_id, paragraph_id, note_index)
            or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to edit this note.")

    timdb.notes.modifyNote(doc_id, doc_ver, paragraph_id, note_index, noteText, access, tags)
    return "Success"


@app.route("/deleteNote", methods=['POST'])
def deleteNote():
    verifyLoggedIn()
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['docId'])
    paragraph_id = int(jsondata['par'])
    note_index = int(jsondata['note_index'])
    timdb = getTimDb()

    if not (timdb.notes.hasEditAccess(group_id, doc_id, paragraph_id, note_index)
            or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to remove this note.")

    timdb.notes.deleteNote(doc_id, paragraph_id, note_index)
    return "Success"


@app.route("/questions/<int:doc_id>")
def getQuestions(doc_id):
    verifyOwnership(doc_id)
    timdb = getTimDb()
    questions = timdb.questions.get_doc_questions(doc_id)
    return jsonResponse(questions)


@app.route("/askQuestion", methods=['POST'])
def ask_question():
    if not request.args.get('doc_id') or not request.args.get('question_id') or not request.args.get('lecture_id'):
        abort(400, "Bad request")
    doc_id = int(request.args.get('doc_id'))
    question_id = int(request.args.get('question_id'))
    lecture_id = int(request.args.get('lecture_id'))

    verifyOwnership(doc_id)

    if lecture_id < 0:
        abort(400, "Not valid lecture id")

    timdb = getTimDb()
    if not json.loads(timdb.questions.get_question(question_id)[0].get("questionJson"))["TIMELIMIT"]  :
        question_timelimit = 0
    else:
        question_timelimit = int(json.loads(timdb.questions.get_question(question_id)[0].get("questionJson"))["TIMELIMIT"])
    threadToStopQuestion = threading.Thread(target=stop_question_from_running,
                                            args=(lecture_id, question_id, question_timelimit))

    threadToStopQuestion.start()

    verifyOwnership(int(doc_id))
    __question_to_be_asked.append((lecture_id, question_id, []))

    return jsonResponse("")


def stop_question_from_running(lecture_id, question_id, question_timelimit):
    if question_timelimit == 0:
        return

    # Adding extra time to limit so when people gets question a bit later than others they still get to answer
    # TODO: If current implementation changes the way that the question last 10 seconds and after that you can't
    # TODO: answer. Remove this part
    extra_time = 3
    time.sleep(question_timelimit + extra_time)

    for question in __question_to_be_asked:
        if question[0] == lecture_id and question[1] == question_id:
            __question_to_be_asked.remove(question)


@app.route("/getQuestionById", methods=['GET'])
def get_question():
    if not request.args.get("question_id"):
        abort("400")
    # doc_id = int(request.args.get('doc_id'))
    question_id = int(request.args.get('question_id'))

    # verifyOwnership(doc_id)
    timdb = getTimDb()
    question = timdb.questions.get_question(question_id)
    return jsonResponse(question[0])

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
@app.route("/getLectureAnswers", methods=['GET'])
def get_lecture_answers():
    if not request.args.get('question_id') or not request.args.get('doc_id') or not request.args.get('lecture_id'):
        abort(400, "Bad request")

    verifyOwnership(int(request.args.get('doc_id')))
    question_id = int(request.args.get('question_id'))
    lecture_id = int(request.args.get('lecture_id'))

    __pull_answer[question_id, lecture_id] = threading.Event()

    for pull in __pull_answer:
        question, lecture = pull
        if lecture == lecture_id and question != question_id:
            __pull_answer[pull].set()

    if not request.args.get("time"):
        time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f"))
    else:
        time_now = request.args.get('time')

    __pull_answer[question_id, lecture_id].wait()

    timdb = getTimDb()
    answers = timdb.lecture_answers.get_answers_to_question(question_id, time_now)
    if len(answers) <= 0:
        return jsonResponse({"noAnswer": True})

    latest_answer = answers[-1].get("answered_on")

    return jsonResponse({"answers": answers, "questionId": question_id, "latestAnswer": latest_answer})


@app.route("/answerToQuestion", methods=['PUT'])
def answer_to_question():
    if not request.args.get("question_id") or not request.args.get('answers') or not request.args.get('lecture_id'):
        abort(400, "Bad request")

    timdb = getTimDb()

    question_id = int(request.args.get("question_id"))
    answer = request.args.get("answers")
    whole_answer = answer
    lecture_id = int(request.args.get("lecture_id"))
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f"))

    question_ended = True
    for question in __question_to_be_asked:
         if question[0] == lecture_id and question[1] == question_id:
            question_ended = False

    if question_ended:
        return jsonResponse({"questionLate": "The question has already finished. Your answer was not saved."})


    single_answers = []
    answers = answer.split('|')
    for answer in answers:
        single_answers.append(answer.split(','))

    question_json = json.loads(timdb.questions.get_question(question_id)[0].get("questionJson"))

    points = 0.0
    for oneAnswer in single_answers:
        question_number = 0
        for oneLine in oneAnswer:
            header_number = 0
            if question_json['TYPE'] == "matrix" or question_json['TYPE'] == "true-false":
                for header in question_json['DATA']['HEADERS']:
                    if header['text'] == oneLine:
                        try:
                            points += float(
                                question_json['DATA']['ROWS'][question_number]['COLUMNS'][header_number]['points'])
                            break
                        except ValueError:
                            points += 0
                    header_number += 1
            else:
                for row in question_json['DATA']['ROWS']:
                    if row['text'] == oneLine:
                        try:
                            points += float(row['COLUMNS'][0]['points'])
                            break
                        except ValueError:
                            points += 0

        question_number += 1

    timdb.lecture_answers.add_answer(getCurrentUserId(), question_id, lecture_id, whole_answer, time_now, points)

    __pull_answer[question_id, lecture_id].set()

    return jsonResponse("")


@app.route("/notes/<int:doc_id>")
def getNotes(doc_id):
    verifyViewAccess(doc_id)
    timdb = getTimDb()
    group_id = getCurrentUserGroup()
    doc_ver = timdb.documents.getNewestVersionHash(doc_id)
    notes = [note for note in timdb.notes.getNotes(group_id, doc_id, doc_ver)]
    for note in notes:
        note['editable'] = note['UserGroup_id'] == group_id or timdb.users.userIsOwner(getCurrentUserId(), doc_id)
        note['private'] = note['access'] == 'justme'
        tags = note['tags']
        note['tags'] = {}
        for tag in KNOWN_TAGS:
            note['tags'][tag] = tag in tags
    return jsonResponse(notes)


@app.route("/read/<int:doc_id>", methods=['GET'])
def getReadParagraphs(doc_id):
    verifyReadMarkingRight(doc_id)
    timdb = getTimDb()
    doc_ver = timdb.documents.getNewestVersionHash(doc_id)
    readings = timdb.readings.getReadings(getCurrentUserGroup(), doc_id, doc_ver)
    for r in readings:
        r.pop('doc_ver', None)
    return jsonResponse(readings)


@app.route("/read/<int:doc_id>/<int:specifier>", methods=['PUT'])
def setReadParagraph(doc_id, specifier):
    verifyReadMarkingRight(doc_id)
    timdb = getTimDb()
    blocks = timdb.documents.getDocumentAsBlocks(getNewest(doc_id))
    doc_ver = timdb.documents.getNewestVersionHash(doc_id)
    if len(blocks) <= specifier:
        return jsonResponse({'error': 'Invalid paragraph specifier.'}, 400)
    timdb.readings.setAsRead(getCurrentUserGroup(), doc_id, doc_ver, specifier)
    return "Success"


def parse_task_id(task_id):
    # Assuming task_id is of the form "22.palindrome"
    pieces = task_id.split('.')
    if len(pieces) != 2:
        abort(400, 'The format of task_id is invalid. Expected exactly one dot character.')
    doc_id = int(pieces[0])
    task_id_name = pieces[1]
    return doc_id, task_id_name


@app.route("/<plugintype>/<task_id>/answer/", methods=['PUT'])
def saveAnswer(plugintype, task_id):
    timdb = getTimDb()

    doc_id, task_id_name = parse_task_id(task_id)
    verifyViewAccess(doc_id)
    if not 'input' in request.get_json():
        return jsonResponse({'error': 'The key "input" was not found from the request.'}, 400)
    answerdata = request.get_json()['input']

    answer_browser_data = request.get_json().get('abData', {})
    is_teacher = answer_browser_data.get('teacher', False)
    if is_teacher:
        verifyOwnership(doc_id)

    # Load old answers
    oldAnswers = timdb.answers.getAnswers(getCurrentUserId(), task_id)

    # Get the newest answer (state). Only for logged in users.
    state = oldAnswers[0]['content'] if loggedIn() and len(oldAnswers) > 0 else None

    markup = getPluginMarkup(doc_id, plugintype, task_id_name)
    if markup is None:
        return jsonResponse(
            {'error': 'The task was not found in the document. ' + str(doc_id) + ' ' + task_id_name},
            404)
    if markup == "YAMLERROR: Malformed string":
        return jsonResponse({'error': 'Plugin markup YAML is malformed.'}, 400)

    answerCallData = {'markup': markup, 'state': state, 'input': answerdata, 'taskID': task_id}

    pluginResponse = containerLink.call_plugin_answer(plugintype, answerCallData)

    try:
        jsonresp = json.loads(pluginResponse)
    except ValueError:
        return jsonResponse(
            {'error': 'The plugin response was not a valid JSON string. The response was: ' + pluginResponse}, 400)

    if 'web' not in jsonresp:
        return jsonResponse({'error': 'The key "web" is missing in plugin response.'}, 400)

    if 'save' in jsonresp:
        saveObject = jsonresp['save']
        tags = []
        tim_info = jsonresp.get('tim_info', {})
        points = tim_info.get('points', None)

        # Save the new state
        try:
            tags = saveObject['tags']
        except (TypeError, KeyError):
            pass
        if not is_teacher:
            timdb.answers.saveAnswer([getCurrentUserId()], task_id, json.dumps(saveObject), points, tags)
        else:
            if answer_browser_data.get('saveTeacher', False):
                answer_id = answer_browser_data.get('answer_id', None)
                if answer_id is None:
                    return jsonResponse({'error': 'Missing answer_id key'}, 400)
                expected_task_id = timdb.answers.get_task_id(answer_id)
                if expected_task_id != task_id:
                    return jsonResponse({'error': 'Task ids did not match'}, 400)
                users = timdb.answers.get_users(answer_id)
                if len(users) == 0:
                    return jsonResponse({'error': 'No users found for the specified answer'}, 400)
                if not getCurrentUserId() in users:
                    users.append(getCurrentUserId())
                points = answer_browser_data.get('points', points)
                if points == "":
                    points = None
                timdb.answers.saveAnswer(users, task_id, json.dumps(saveObject), points, tags)

    return jsonResponse({'web': jsonresp['web']})


@app.route("/answers/<task_id>/<user>")
def get_answers(task_id, user):
    verifyLoggedIn()
    timdb = getTimDb()
    doc_id, task_id_name = parse_task_id(task_id)
    if not timdb.documents.documentExists(doc_id):
        abort(404, 'No such document')
    user_id = timdb.users.getUserByName(user)
    if user_id != getCurrentUserId():
        verifyOwnership(doc_id)
    if user_id is None:
        abort(400, 'Non-existent user')
    answers = timdb.answers.getAnswers(user_id, task_id)
    return jsonResponse(answers)


@app.route("/getState")
def get_state():
    timdb = getTimDb()
    doc_id, par_id, user, state = unpack_args('doc_id', 'par_id', 'user', 'state', types=[int, int, str, str])
    if not timdb.documents.documentExists(doc_id):
        abort(404, 'No such document')
    user_id = timdb.users.getUserByName(user)
    if user_id != getCurrentUserId():
        verifyOwnership(doc_id)
    if user_id is None:
        abort(400, 'Non-existent user')
    if not timdb.documents.documentExists(doc_id):
        abort(404, 'No such document')
    if not hasViewAccess(doc_id):
        abort(403, 'Permission denied')

    version = request.headers['Version']
    block = timdb.documents.getBlockAsHtml(DocIdentifier(doc_id, version), par_id)

    texts, jsPaths, cssPaths, modules = pluginControl.pluginify([block],
                                                                user,
                                                                timdb.answers,
                                                                doc_id,
                                                                user_id,
                                                                custom_state=state)
    return jsonResponse(texts[0])


def getPluginMarkup(doc_id, plugintype, task_id):
    timdb = getTimDb()
    doc_markdown = timdb.documents.getDocumentAsHtmlBlocks(getNewest(doc_id))
    for block in doc_markdown:
        if ('plugin="{}"'.format(plugintype) in block and "<pre" in block and 'id="{}"'.format(task_id) in block):
            markup = pluginControl.get_block_yaml(block)
            return markup
    return None


@app.route("/")
@app.route("/view/")
def indexPage():
    timdb = getTimDb()
    possible_groups = timdb.users.getUserGroupsPrintable(getCurrentUserId())
    return render_template('index.html',
                           userName=getCurrentUserName(),
                           userId=getCurrentUserId(),
                           userGroups=possible_groups)


def startApp():
    # TODO: Think if it is truly necessary to have threaded=True here
    app.wsgi_app = ReverseProxied(app.wsgi_app)
    app.run(host='0.0.0.0', port=5000, use_reloader=False, threaded=True)
