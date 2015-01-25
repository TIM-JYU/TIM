# -*- coding: utf-8 -*-
import logging
import json
import os
import imghdr
import io
import codecs
import collections
import re
import sys

from flask import Flask, redirect, url_for, flash
from flask import stream_with_context
from flask import render_template
from flask import g
from flask import request
from flask import send_from_directory
from flask.ext.compress import Compress
import requests
from werkzeug.utils import secure_filename
from flask import Response
from flask.helpers import send_file
from bs4 import UnicodeDammit
from werkzeug.contrib.profiler import ProfilerMiddleware

from ReverseProxied import ReverseProxied
import containerLink
from routes.edit import edit_page
from routes.manage import manage_page
from routes.view import view_page
from timdb.timdb2 import TimDb
from timdb.timdbbase import TimDbException, DocIdentifier
import pluginControl
from containerLink import PluginException
from routes.settings import settings_page
from routes.common import *

app = Flask(__name__)
app.config.from_pyfile('defaultconfig.py', silent=False)
app.config.from_envvar('TIM_SETTINGS', silent=True)
Compress(app)

app.register_blueprint(settings_page)
app.register_blueprint(manage_page)
app.register_blueprint(edit_page)
app.register_blueprint(view_page)

print('Debug mode: {}'.format(app.config['DEBUG']))

KNOWN_TAGS = ['difficult', 'unclear']

# current_app.logging.basicConfig(filename='timLog.log',level=logging.DEBUG, format='%(asctime)s %(message)s')
formatter = logging.Formatter("{\"time\":%(asctime)s, \"file\": %(pathname)s, \"line\" :%(lineno)d, \"messageLevel\":  %(levelname)s, \"message\": %(message)s}")
if not os.path.exists(app.config['LOG_DIR']):
    os.mkdir(app.config['LOG_DIR'])
handler = logging.FileHandler(app.config['LOG_PATH'])
handler.setLevel(logging.DEBUG)
handler.setFormatter(formatter)
app.logger.addHandler(handler)

def allowed_file(filename):
    return '.' in filename and \
        filename.rsplit('.', 1)[1] in ALLOWED_EXTENSIONS

DOC_EXTENSIONS = ['txt', 'md', 'markdown']
PIC_EXTENSIONS = ['png', 'jpg', 'jpeg', 'gif']
ALLOWED_EXTENSIONS = set(PIC_EXTENSIONS + DOC_EXTENSIONS)
STATIC_PATH = "./static/"
DATA_PATH = "./static/data/"

LOG_LEVELS = {"CRITICAL" : app.logger.critical, 
              "ERROR" : app.logger.error,
              "WARNING" : app.logger.warning,
              "INFO": app.logger.info,
              "DEBUG" : app.logger.debug}

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
    if not loggedIn():
        return jsonResponse({'message': 'You have to be logged in to upload a file.'}, 403)
    timdb = getTimDb()
    if request.method == 'POST':
        doc = request.files['file']
        if not allowed_file(doc.filename):
            return jsonResponse({'message': 'The file format is not allowed.'}, 403)
        filename = secure_filename(doc.filename)
        if(filename.endswith(tuple(DOC_EXTENSIONS))):
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
                timdb.users.grantViewAccess(0, img_id) # So far everyone can see all images
                return jsonResponse({"file": str(img_id) + '/' + img_filename})
            else:
                doc.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
                return redirect(url_for('uploaded_file', filename=filename))
    


@app.route('/images/<int:image_id>/<image_filename>/')
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
    
@app.route('/uploads/<filename>')
def uploaded_file(filename):
    return send_from_directory(app.config['UPLOAD_FOLDER'],filename)

@app.route("/getDocuments/")
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
        
    #print('req_folder is "{}"'.format(req_folder))
    
    folders = []
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
            slash = docname.find('/')
            foldername = docname[:slash]
            
            duplicate = False
            for f in folders:
                if f['name'] == foldername:
                    duplicate = True
                    break
            if duplicate:
                continue
            
            fullfolder = foldername if req_folder is None else req_folder + '/' + foldername            
            folders.append({
                'isFolder': True,
                'name': foldername,
                'fullname' : fullfolder,
                'items': [],
                'canEdit': False,
                'isOwner': False,
                'owner': timdb.users.getOwnerGroup(doc['id'])
            })
            continue

        doc['name'] = docname
        doc['fullname'] = fullname
        doc['isFolder'] = False
        doc['canEdit'] = timdb.users.userHasEditAccess(getCurrentUserId(), doc['id'])
        doc['isOwner'] = timdb.users.userIsOwner(getCurrentUserId(), doc['id'])
        doc['owner'] = timdb.users.getOwnerGroup(doc['id'])
        finalDocs.append(doc)
        
    return jsonResponse(folders + finalDocs)

@app.route("/getJSON/<int:doc_id>/")
def getJSON(doc_id):
    timdb = getTimDb()
    verifyViewAccess(doc_id)
    try:
        texts = timdb.documents.getDocumentBlocks(getNewest(doc_id))
        doc = timdb.documents.getDocument(doc_id.id)
        return jsonResponse({"name" : doc['name'], "text" : texts})
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
        return jsonResponse({"name" : doc['name'], "text" : blocks})
    except ValueError as err:
        print(err)
        return "[]"
    except TimDbException as err:
        print(err)
        return "[]"

@app.route("/createDocument", methods=["POST"])
def createDocument():
    if not loggedIn():
        return jsonResponse({'message': 'You have to be logged in to create a document.'}, 403)
    jsondata = request.get_json()
    docName = jsondata['doc_name']
    
    if docName.startswith('/') or docName.endswith('/'):
        return jsonResponse({'message': 'Document name cannot start or end with /.'}, 400)
    
    if re.match('^(\d)*$', docName) is not None:
        return jsonResponse({'message': 'Document name can not be a number to avoid confusion with document id.'}, 400)
    
    timdb = getTimDb()
    docId = timdb.documents.createDocument(docName, getCurrentUserGroup())
    return jsonResponse({'id' : docId.id, 'name' : docName})

@app.route("/getBlock/<int:docId>/<int:blockId>")
def getBlockMd(docId, blockId):
    timdb = getTimDb()
    verifyViewAccess(docId)
    block = timdb.documents.getBlock(getNewest(docId), blockId)
    return jsonResponse({"md": block})

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
        return Response(stream_with_context(req.iter_content()), content_type = req.headers['content-type'])
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
    verifyLoggedIn()
    jsondata = request.get_json()
    noteText = jsondata['text']
    visibility = jsondata['visibility']
    tags = []
    for tag in KNOWN_TAGS:
        if jsondata[tag]:
            tags.append(tag)
    doc_id = jsondata['doc_id']
    doc_ver = request.headers.get('Version')
    paragraph_id = jsondata['par_id']

    timdb = getTimDb()
    group_id = getCurrentUserGroup()
    timdb.notes.addNote(group_id, doc_id, doc_ver, int(paragraph_id), noteText, visibility, tags)
    #TODO: Handle error.
    return "Success"

@app.route("/editNote", methods=['POST'])
def editNote():
    verifyLoggedIn()
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['doc_id'])
    doc_ver = request.headers.get('Version')
    paragraph_id = int(jsondata['par_id'])
    noteText = jsondata['text']
    visibility = jsondata['access']
    note_index = int(jsondata['note_index'])
    tags = []
    for tag in KNOWN_TAGS:
        if jsondata[tag]:
            tags.append(tag)
    timdb = getTimDb()

    if not (timdb.notes.hasEditAccess(group_id, doc_id, paragraph_id, note_index)
            or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to edit this note.")

    timdb.notes.modifyNote(doc_id, doc_ver, paragraph_id, note_index, noteText, visibility, tags)
    return "Success"

@app.route("/deleteNote", methods=['POST'])
def deleteNote():
    verifyLoggedIn()
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['doc_id'])
    paragraph_id = int(jsondata['par_id'])
    note_index = int(jsondata['note_index'])
    timdb = getTimDb()

    if not (timdb.notes.hasEditAccess(group_id, doc_id, paragraph_id, note_index)
            or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to remove this note.")

    timdb.notes.deleteNote(doc_id, paragraph_id, note_index)
    return "Success"

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
        for tag in KNOWN_TAGS:
            note[tag] = tag in note['tags']
        note.pop('tags', None)
    return jsonResponse(notes)

@app.route("/read/<int:doc_id>", methods=['GET'])
def getReadParagraphs(doc_id):
    verifyViewAccess(doc_id)
    timdb = getTimDb()
    doc_ver = timdb.documents.getNewestVersionHash(doc_id)
    readings = timdb.readings.getReadings(getCurrentUserGroup(), doc_id, doc_ver)
    return jsonResponse(readings)

@app.route("/read/<int:doc_id>/<int:specifier>", methods=['PUT'])
def setReadParagraph(doc_id, specifier):
    verifyViewAccess(doc_id)
    timdb = getTimDb()
    blocks = timdb.documents.getDocumentAsBlocks(getNewest(doc_id))
    doc_ver = timdb.documents.getNewestVersionHash(doc_id)
    if len(blocks) <= specifier:
        return jsonResponse({'error' : 'Invalid paragraph specifier.'}, 400)
    timdb.readings.setAsRead(getCurrentUserGroup(), doc_id, doc_ver, specifier)
    return "Success"

@app.route("/<plugintype>/<task_id>/answer/", methods=['PUT'])
def saveAnswer(plugintype, task_id):
    timdb = getTimDb()

    # Assuming task_id is of the form "22.palindrome"
    pieces = task_id.split('.')
    if len(pieces) != 2:
        return jsonResponse({'error' : 'The format of task_id is invalid. Expected exactly one dot character.'}, 400)
    doc_id = int(pieces[0])
    task_id_name = pieces[1]
    if not 'input' in request.get_json():
        return jsonResponse({'error' : 'The key "input" was not found from the request.'}, 400)
    answerdata = request.get_json()['input']

    # Load old answers
    oldAnswers = timdb.answers.getAnswers(getCurrentUserId(), task_id)

    # Get the newest answer (state). Only for logged in users.
    state = oldAnswers[0]['content'] if loggedIn() and len(oldAnswers) > 0 else None
    
    markup = getPluginMarkup(doc_id, plugintype, task_id_name)
    if markup is None:
        return jsonResponse({'error' : 'The task was not found in the document. ' + str(doc_id) + ' ' + task_id_name}, 404)
    if markup == "YAMLERROR: Malformed string":
        return jsonResponse({'error' : 'Plugin markup YAML is malformed.'}, 400)
 
    answerCallData = {'markup' : markup, 'state' : state, 'input' : answerdata, 'taskID': task_id}

    pluginResponse = containerLink.call_plugin_answer(plugintype, answerCallData)
    
    try:
        jsonresp = json.loads(pluginResponse)
    except ValueError:
        return jsonResponse({'error' : 'The plugin response was not a valid JSON string. The response was: ' + pluginResponse}, 400)
    
    if not 'web' in jsonresp:
        return jsonResponse({'error' : 'The key "web" is missing in plugin response.'}, 400)
    
    if 'save' in jsonresp and not request.headers.get('RefererPath', '').startswith('/teacher/'):
        saveObject = jsonresp['save']
        
        #Save the new state
        if isinstance(saveObject, collections.Iterable):
            points = jsonresp['save']['points'] if 'points' in saveObject else None
            tags = jsonresp['save']['tags'] if 'tags' in saveObject else []
        else:
            points = None
            tags = []
        timdb.answers.saveAnswer([getCurrentUserId()], task_id, json.dumps(saveObject), points, tags)

    return jsonResponse({'web':jsonresp['web']})

def getPluginMarkup(doc_id, plugintype, task_id):
    timdb = getTimDb()
    doc_markdown = timdb.documents.getDocumentAsHtmlBlocks(getNewest(doc_id))
    for block in doc_markdown:
        if('plugin="{}"'.format(plugintype) in block and "<pre" in block and 'id="{}"'.format(task_id) in block):
            markup = pluginControl.get_block_yaml(block)
            return markup
    return None
    
@app.route("/")
def indexPage():
    return render_template('index.html', userName=getCurrentUserName(), userId=getCurrentUserId())

@app.route("/logout", methods=['POST'])
def logout():
    session.pop('user_id', None)
    session.pop('appcookie', None)
    session['user_name'] = 'Anonymous'
    flash('You were successfully logged out.', 'loginmsg')
    return redirect(url_for('indexPage'))

@app.route("/login")
def loginWithKorppi():
    urlfile = request.url_root + "login"
    if request.args.get('came_from'):
        session['came_from'] = request.args.get('came_from')
    if not session.get('appcookie'):
        randomHex = codecs.encode(os.urandom(24), 'hex').decode('utf-8')
        session['appcookie'] = randomHex
    url = "https://korppi.jyu.fi/kotka/interface/allowRemoteLogin.jsp"
    try:
        r = requests.get(url, params={'request': session['appcookie']}, verify=True)
    except requests.exceptions.SSLError:
        return render_template('503.html', message='Korppi seems to be down, so login is currently not possible. '
                                                   'Try again later.'), 503
    
    if r.status_code != 200:
        return render_template('503.html', message='Korppi seems to be down, so login is currently not possible. '
                                                   'Try again later.'), 503
    korppiResponse = r.text.strip()
    #print("korppiresponse is: '{}'".format(korppiResponse))
    if not korppiResponse:
        return redirect(url+"?authorize=" + session['appcookie'] + "&returnTo=" + urlfile, code=303)
    pieces = (korppiResponse + "\n\n").split('\n')
    userName = pieces[0]
    realName = pieces[1]
    email = pieces[2]

    timdb = getTimDb()
    userId = timdb.users.getUserByName(userName)
    
    if userId is None:
        uid = timdb.users.createUser(userName, realName, email)
        gid = timdb.users.createUserGroup(userName)
        timdb.users.addUserToGroup(gid, uid)
        userId = uid
    else:
        if realName:
            timdb.users.updateUser(userId, userName, realName, email)
    session['user_id'] = userId
    session['user_name'] = userName
    session['real_name'] = realName
    session['email'] = email
    flash('You were successfully logged in.', 'loginmsg')
    return redirect(session.get('came_from', '/'))

def __getRealName(email):
    atindex = email.index('@')
    if atindex <= 0:
        return email
    parts = email[0:atindex].split('.')
    parts2 = [part.capitalize() if len(part) > 1 else part.capitalize() + '.' for part in parts]
    return ' '.join(parts2)

@app.route("/testuser/<email>")
def testLogin(email):
    if re.match('^[\w\.-]+@([\w-]+\.)+[\w-]+$', email) is None:
        return "User name must be a valid email address."

    timdb = getTimDb()
    userName = email
    userId = timdb.users.getUserByName(userName)
    realName = __getRealName(email)

    if userId is None:
        uid = timdb.users.createUser(userName, realName, email)
        gid = timdb.users.createUserGroup(userName)
        timdb.users.addUserToGroup(gid, uid)
        userId = uid
        print('New test user: ' + userName)
    session['user_id'] = userId
    session['user_name'] = userName
    session['real_name'] = realName
    session['email'] = email
    flash('You were successfully logged in.', 'loginmsg')
    return redirect(url_for('indexPage'))


def startApp():
    app.wsgi_app = ReverseProxied(app.wsgi_app)
    #app.wsgi_app = ProfilerMiddleware(app.wsgi_app, sort_by=('cumtime',))
    app.run(host='0.0.0.0', port=5000, use_reloader='pudb' not in sys.modules)
