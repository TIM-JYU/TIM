# -*- coding: utf-8 -*-
# Modified hajoviin
import logging
import os
import imghdr
import io
import collections
import re
import sys
import time
import datetime
from time import mktime
from datetime import timezone
import posixpath
import threading

from flask import Flask, redirect, url_for, Blueprint
from flask import stream_with_context
from flask import render_template
from flask import send_from_directory
from flask.ext.compress import Compress
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

__question_to_be_asked = []

__pull_answer = {}

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


@app.route('/wall')
def get_wall():
    verifyLoggedIn()
    return render_template('wall.html')


@app.route('/getAllMessages')
def get_all_messages():
    if not request.args.get("lecture_id"):
        abort(400, "Bad request, missing lecture id")
    timdb = getTimDb()
    lecture_id = int(request.args.get("lecture_id"))

    messages = timdb.messages.get_messages(lecture_id)
    if len(messages) > 0:
        list_of_new_messages = []
        for message in messages:
            user = timdb.users.getUser(message.get('user_id'))
            time_as_time = datetime.datetime.fromtimestamp(
                mktime(time.strptime(message.get("timestamp"), "%Y-%m-%d %H:%M:%S.%f")))
            list_of_new_messages.append(
                user.get('name') + " <" + time_as_time.strftime('%H:%M:%S') + ">" + ": " + message.get('message'))
            # Prevents previously asked question to be asked from user.
            current_user = getCurrentUserId()
            for triple in __question_to_be_asked:
                if triple[0] == lecture_id and current_user not in triple[2]:
                    triple[2].append(current_user)

        return jsonResponse(
            {"status": "results", "data": list_of_new_messages, "lastid": messages[-1].get('msg_id'),
             "lectureId": lecture_id})

    return jsonResponse({"status": "no-results", "data": [], "lastid": -1, "lectureId": lecture_id})


@app.route('/getUpdates')
def get_updates():
    if not request.args.get('client_message_id') or not request.args.get("lecture_id") or not request.args.get(
            'doc_id') or not request.args.get('is_lecturer'):
        abort(400, "Bad requst")
    client_last_id = int(request.args.get('client_message_id'))

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
        for pair in __question_to_be_asked:
            if pair[0] == lecture_id:
                __question_to_be_asked.remove(pair)
        return get_running_lectures(doc_id)

    list_of_new_messages = []
    last_message_id = -1

    while step <= 10:
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
                        user.get('name') + " <" + time_as_time.strftime('%H:%M:%S') + ">" + ": " + message.get(
                            'message'))
                last_message_id = messages[-1].get('msg_id')

        current_user = getCurrentUserId()
        for pair in __question_to_be_asked:
            if pair[0] == lecture_id and current_user not in pair[2]:
                question_json = timdb.questions.get_question(pair[1])[0].get("questionJson")
                pair[2].append(getCurrentUserId())
                return jsonResponse(
                    {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                     "lectureId": lecture_id, "question": True, "questionId": pair[1], "questionJson": question_json,
                     "isLecture": True})

        if len(list_of_new_messages) > 0:
            return jsonResponse(
                {"status": "results", "data": list_of_new_messages, "lastid": last_message_id,
                 "lectureId": lecture_id, "isLecture": True})

        time.sleep(1)
        step += 1

    return jsonResponse(
        {"status": "no-results", "data": ["No new messages"], "lastid": client_last_id, "lectureId": lecture_id,
         "isLecture": True})


@app.route('/sendMessage', methods=['POST'])
def send_message():
    timdb = getTimDb()
    new_message = request.args.get("message")
    lecture_id = int(request.args.get("lecture_id"))

    new_timestamp = str(datetime.datetime.now())
    msg_id = timdb.messages.add_message(getCurrentUserId(), lecture_id, new_message, new_timestamp, True)
    return jsonResponse(msg_id)


@app.route('/view/question')
def show_question():
    return render_template('question.html')


@app.route('/question')
def show_question_without_view():
    return render_template('question.html')


@app.route('/getQuestion')
def get_quesition():
    doc_id = request.args.get('doc_id')
    par_index = request.args.get('par_index')
    timdb = getTimDb()
    question = timdb.questions.get_paragraphs_question(doc_id, par_index);
    return jsonResponse(question)


@app.route('/getQuestions', methods=['GET'])
def get_questions():
    timdb = getTimDb()
    questions = timdb.questions.get_questions()
    return jsonResponse(questions)


@app.route('/addQuestion', methods=['POST'])
def add_question():
    # TODO: Only lecturers should be able to create questions.
    # verifyOwnership(doc_id)
    question = request.args.get('question')
    answer = request.args.get('answer')
    doc_id = int(request.args.get('doc_id'))
    par_index = int(request.args.get('par_index'))
    questionJson = request.args.get('questionJson')
    timdb = getTimDb()
    questions = timdb.questions.add_questions(doc_id, par_index, question, answer, questionJson)
    return jsonResponse(questions)


@app.route('/checkLecture', methods=['GET'])
def check_lecture():
    arg_id = request.args.get('doc_id')
    if not arg_id:
        return abort(400)

    doc_id = int(arg_id)
    timdb = getTimDb()
    current_user = getCurrentUserId()
    is_in_lecture, lecture_id, = timdb.lectures.check_if_in_lecture(doc_id, current_user)
    lecture = timdb.lectures.get_lecture(lecture_id)
    if lecture:
        lecture_code = lecture[0].get("lecture_code")
        if lecture[0].get("lecturer") == current_user:
            is_lecturer = True
        else:
            is_lecturer = False
        return jsonResponse({"isInLecture": is_in_lecture, "lectureId": lecture_id, "lectureCode": lecture_code,
                             "isLecturer": is_lecturer, "startTime": lecture[0].get("start_time"),
                             "endTime": lecture[0].get("end_time")})
    else:
        return get_running_lectures(doc_id)


def check_if_lecture_is_running(lecture_id):
    timdb = getTimDb()
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    return timdb.lectures.check_if_lecture_is_running(lecture_id, time_now)


def get_running_lectures(doc_id):
    timdb = getTimDb()
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    lecture_code = "Not running"
    list_of_lectures = timdb.lectures.get_document_lectures(doc_id, time_now)
    current_lecture_codes = []
    future_lecture_codes = []
    is_lecturer = hasOwnership(doc_id)
    for lecture in list_of_lectures:
        if lecture.get("start_time") <= time_now:
            current_lecture_codes.append(lecture.get("lecture_code"))
        else:
            future_lecture_codes.append(lecture.get("lecture_code") + " [" + lecture.get("start_time") + "]")
    return jsonResponse(
        {"isLecturer": is_lecturer, "lectures": current_lecture_codes, "futureLectures": future_lecture_codes,
         "lectureCode": lecture_code})


@app.route('/createLecture', methods=['POST'])
def start_lecture():
    if not request.args.get("doc_id") or not request.args.get("start_date") or not request.args.get(
            "end_date") or not request.args.get("lecture_code"):
        abort(400, "Missing parameters")
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
    if not timdb.lectures.check_if_correct_name(doc_id, lecture_code):
        abort(400, "Can't create lecture with same code to same document")
    lecture_id = timdb.lectures.create_lecture(doc_id, current_user, start_time, end_time, lecture_code, password, True)
    current_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")

    if start_time <= current_time <= end_time:
        timdb.lectures.join_lecture(lecture_id, current_user, True)
    return jsonResponse({"lectureId": lecture_id})


@app.route('/endLecture', methods=['POST'])
def end_lecture():
    if not request.args.get("doc_id") or not request.args.get("lecture_id"):
        abort(400)

    doc_id = int(request.args.get("doc_id"))
    lecture_id = int(request.args.get("lecture_id"))
    verifyOwnership(doc_id)
    timdb = getTimDb()
    timdb.lectures.delete_users_from_lecture(lecture_id)
    for pair in __question_to_be_asked:
        if pair[0] == lecture_id:
            __question_to_be_asked.remove(pair)
    now = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    timdb.lectures.set_end_for_lecture(lecture_id, str(now))
    return jsonResponse("")


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
    for pair in __question_to_be_asked:
        if pair[0] == lecture_id:
            __question_to_be_asked.remove(pair)
    timdb.lectures.delete_lecture(lecture_id, True)
    return get_running_lectures(doc_id)


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
    if lecture[0].get("lecturer") == current_user:
        is_lecturer = True
    else:
        is_lecturer = False
    return jsonResponse(
        {"correctPassword": True, "inLecture": True, "lectureId": lecture_id, "isLecturer": is_lecturer,
         "lectureCode": lecture_code, "startTime": lecture[0].get("start_time"),
         "endTime": lecture[0].get("end_time")})


@app.route('/leaveLecture', methods=['POST'])
def leave_lecture():
    timdb = getTimDb()
    lecture_id = int(request.args.get("lecture_id"))
    doc_id = int(request.args.get("doc_id"))
    timdb.lectures.leave_lecture(lecture_id, getCurrentUserId(), True)
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
    verifyViewAccess(doc_id)
    timdb = getTimDb()
    questions = timdb.questions.get_doc_questions(doc_id)
    return jsonResponse(questions)


@app.route("/askQuestion", methods=['GET'])
def ask_question():
    if not request.args.get('doc_id') or not request.args.get('question_id') or not request.args.get('lecture_id'):
        abort(400, "Bad request")
    doc_id = int(request.args.get('doc_id'))
    question_id = int(request.args.get('question_id'))
    lecture_id = int(request.args.get('lecture_id'))

    if lecture_id < 0:
        abort(400, "Not valid lecture id")

    verifyOwnership(int(doc_id))
    __question_to_be_asked.append((lecture_id, question_id, []))

    return jsonResponse("")


@app.route("/getLectureAnswers", methods=['GET'])
def get_lecture_answers():

    if not request.args.get('question_id') or not request.args.get('doc_id'):
        abort(400, "Bad request")

    verifyOwnership(int(request.args.get('doc_id')))
    question_id = int(request.args.get('question_id'))

    __pull_answer[question_id] = threading.Event()

    if not request.args.get("time"):
        time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
    else:
        time_now = request.args.get('time')

    __pull_answer[question_id].wait()

    timdb = getTimDb()
    answers = timdb.lecture_answers.get_answers_to_question(question_id, time_now)
    latest_answer = answers[-1].get("answered_on")

    return jsonResponse({"answers": answers, "questionId": question_id, "latestAnswer": latest_answer})


@app.route("/answerToQuestion", methods=['POST'])
def answer_to_question():
    if not request.args.get("question_id") or not request.args.get('answers'):
        abort(400, "Bad request")

    timdb = getTimDb()

    question_id = int(request.args.get("question_id"))
    answer = request.args.get("answers")
    time_now = str(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
    timdb.lecture_answers.add_answer(getCurrentUserId(), question_id, answer, time_now, 0.0)
    # TODO: POINTS

    __pull_answer[question_id].set()

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
