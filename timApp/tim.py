# -*- coding: utf-8 -*-
from flask import Flask, redirect, url_for, session, abort, flash, current_app
from flask import stream_with_context
from flask import render_template
from flask import g
from flask import request
from flask import send_from_directory
import logging
from ReverseProxied import ReverseProxied
import json
import os
import containerLink
from werkzeug.utils import secure_filename
from timdb.timdb2 import TimDb
from timdb.timdbbase import TimDbException, DocIdentifier
from flask import Response
import imghdr
from flask.helpers import send_file
import io
import pluginControl
from htmlSanitize import sanitize_html
import collections
from containerLink import PluginException

app = Flask(__name__) 
app.config.from_object(__name__)

# Load default config and override config from an environment variable
app.config.update(dict(
    DATABASE=os.path.join(app.root_path, 'tim_files/tim.db'),
    DEBUG=True,
    SECRET_KEY='development key',
    USERNAME='admin',
    PASSWORD='default',
    FILES_PATH='tim_files',
    UPLOAD_FOLDER = "./media/images/",
    MAX_CONTENT_LENGTH = 16 * 1024 * 1025 
   ))

LOG_FILENAME = "../tim_logs/timLog.log"

# current_app.logging.basicConfig(filename='timLog.log',level=logging.DEBUG, format='%(asctime)s %(message)s')
formatter = logging.Formatter("{\"time\":%(asctime)s, \"file\": %(pathname)s, \"line\" :%(lineno)d, \"messageLevel\":  %(levelname)s, \"message\": %(message)s}")
handler = logging.FileHandler(LOG_FILENAME)
handler.setLevel(logging.DEBUG)
handler.setFormatter(formatter)
app.logger.addHandler(handler)

def allowed_file(filename):
    return '.' in filename and \
        filename.rsplit('.', 1)[1] in ALLOWED_EXTENSIONS

#app.config.from_envvar('TIM_SETTINGS', silent=True)

if os.path.abspath('..') == '/service':
    app.config['DEBUG'] = False

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
              #"NOTSET" : app.logger.notset}


# Logger call
@app.route("/log/", methods=["POST"])
def logMessage():
    try:
        message = request.get_json()['message']
        LOG_LEVELS[level](message)
    except KeyError:
        app.logger.error("Failed logging call: " + str(request.get_data()))
    

@app.errorhandler(403)
def forbidden(error):

    return render_template('403.html', message=error.description), 403

@app.errorhandler(404)
def notFound(error):
    return render_template('404.html'), 404

def jsonResponse(jsondata, status_code=200):
    response = Response(json.dumps(jsondata), mimetype='application/json')
    response.status_code = status_code
    return response

def verifyEditAccess(block_id, message="Sorry, you don't have permission to edit this resource."):
    timdb = getTimDb()
    if not timdb.users.userHasEditAccess(getCurrentUserId(), block_id):
        abort(403, message)

def verifyViewAccess(block_id):
    timdb = getTimDb()
    if not timdb.users.userHasViewAccess(getCurrentUserId(), block_id):
        abort(403, "Sorry, you don't have permission to view this resource.")

def verifyLoggedIn():
    if not loggedIn():
        abort(403, "You have to be logged in to perform this action.")

def loggedIn():
    return getCurrentUserId() != 0

@app.route("/manage/<int:doc_id>")
def manage(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    doc_data = timdb.documents.getDocument(DocIdentifier(doc_id, ''))
    doc_data['versions'] = timdb.documents.getDocumentVersions(doc_id)
    doc_data['owner'] = timdb.users.getOwnerGroup(doc_id)
    editors = timdb.users.getEditors(doc_id)
    viewers = timdb.users.getViewers(doc_id)
    return render_template('manage.html', doc=doc_data, editors=editors, viewers=viewers)

@app.route("/getPermissions/<int:doc_id>")
def getPermissions(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    doc_data = timdb.documents.getDocument(DocIdentifier(doc_id, ''))
    editors = timdb.users.getEditors(doc_id)
    viewers = timdb.users.getViewers(doc_id)
    return jsonResponse({'doc' : doc_data, 'editors' : editors, 'viewers' : viewers})

@app.route("/addPermission/<int:doc_id>/<group_name>/<perm_type>", methods=["PUT"])
def addPermission(doc_id, group_name, perm_type):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    
    groups = timdb.users.getUserGroupsByName(group_name)
    if len(groups) == 0:
        return jsonResponse({'message' : 'No user group with this name was found.'}, 404)
    
    group_id = groups[0]['id']
    
    if perm_type == 'edit':
        timdb.users.grantEditAccess(group_id, doc_id)
    elif perm_type == 'view':
        timdb.users.grantViewAccess(group_id, doc_id)
    else:
        abort(400)
    return "Success"

@app.route("/removePermission/<int:doc_id>/<int:group_id>/<perm_type>", methods=["PUT"])
def removePermission(doc_id, group_id, perm_type):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    if perm_type == 'edit':
        timdb.users.removeEditAccess(group_id, doc_id)
    elif perm_type == 'view':
        timdb.users.removeViewAccess(group_id, doc_id)
    else:
        abort(400)
    return "Success"

@app.route("/rename/<int:doc_id>", methods=["PUT"])
def renameDocument(doc_id):
    timdb = getTimDb()
    new_name = request.get_json()['new_name']
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    timdb.documents.renameDocument(DocIdentifier(doc_id, ''), new_name)
    return "Success"

@app.route('/download/<int:doc_id>')
def downloadDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    verifyEditAccess(doc_id, "Sorry, you don't have permission to download this document.")
    doc_data = timdb.documents.getDocumentMarkdown(getNewest(doc_id))
    return Response(doc_data, mimetype="text/plain")

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
            try:
                content = doc.read().decode('utf-8')
            except UnicodeDecodeError:
                return jsonResponse({'message': 'The file should be in UTF-8 format.'}, 400)
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
    
@app.route('/update/<int:doc_id>/<version>', methods=['POST'])
def updateDocument(doc_id, version):
    timdb = getTimDb()
    docId = DocIdentifier(doc_id, version)
    if not timdb.documents.documentExists(docId):
        abort(404)
    if not timdb.users.userHasEditAccess(getCurrentUserId(), doc_id):
        abort(403)
    doc = request.files['file']
    newId = timdb.documents.updateDocument(docId, doc.read())
    return jsonResponse({'version' : newId.hash})

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
    timdb = getTimDb()
    docs = timdb.documents.getDocuments(historylimit=1)
    allowedDocs = [doc for doc in docs if timdb.users.userHasViewAccess(getCurrentUserId(), doc['id'])]
    for doc in allowedDocs:
        doc['canEdit'] = timdb.users.userHasEditAccess(getCurrentUserId(), doc['id'])
        doc['isOwner'] = timdb.users.userIsOwner(getCurrentUserId(), doc['id'])
        doc['owner'] = timdb.users.getOwnerGroup(doc['id'])
    return jsonResponse(allowedDocs)

def getCurrentUserId():
    uid = session.get('user_id')
    return uid if uid is not None else 0

def getCurrentUserName():
    name = session.get('user_name')
    return name if name is not None else 'Anonymous'

def getCurrentUserGroup():
    timdb = getTimDb()
    return timdb.users.getUserGroups(getCurrentUserId())[0]['id']

def getTimDb():
    if not hasattr(g, 'timdb'):
        g.timdb = TimDb(db_path=app.config['DATABASE'], files_root_path=app.config['FILES_PATH'], current_user_name=getCurrentUserName())
    return g.timdb

@app.route("/getJSON/<int:doc_id>/")
def getJSON(doc_id):
    timdb = getTimDb()
    verifyViewAccess(doc_id)
    try:
        texts = timdb.documents.getDocumentBlocks(getNewest(doc_id))
        doc = timdb.documents.getDocument(doc_id)
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
        doc = timdb.documents.getDocument(newest)
        return jsonResponse({"name" : doc['name'], "text" : blocks})
    except ValueError as err:
        print(err)
        return "[]"
    except TimDbException as err:
        print(err)
        return "[]"

@app.route("/postParagraph/", methods=['POST'])
def postParagraph():
    timdb = getTimDb()
    docId = request.get_json()['docId']
    verifyEditAccess(docId)
    paragraphText = sanitize_html(request.get_json()['text'])
    parIndex = request.get_json()['par']
    app.logger.info("Editing file: {}, paragraph {}".format(docId, parIndex ))
    version = request.headers.get('Version')
    identifier = getNewest(docId)#DocIdentifier(docId, version)
    
    try:
        blocks, version = timdb.documents.modifyMarkDownBlock(identifier, int(parIndex), paragraphText)
    except IOError as err:
        print(err)
        return "Failed to modify block."
    # Replace appropriate elements with plugin content, load plugin requirements to template
    (plugins, preparedBlocks) = pluginControl.pluginify(blocks, getCurrentUserName(), timdb.answers, docId, getCurrentUserId())
    (jsPaths, cssPaths, modules) = pluginControl.getPluginDatas(plugins)

    return jsonResponse({'texts' : preparedBlocks, 'js':jsPaths,'css':cssPaths,'angularModule':modules})

@app.route("/createDocument", methods=["POST"])
def createDocument():
    if not loggedIn():
        return jsonResponse({'message': 'You have to be logged in to create a document.'}, 403)
    jsondata = request.get_json()
    docName = jsondata['doc_name']
    timdb = getTimDb()
    docId = timdb.documents.createDocument(docName, getCurrentUserGroup())
    return jsonResponse({'id' : docId.id})

@app.route("/documents/<int:doc_id>", methods=["DELETE"])
def deleteDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        return jsonResponse({'message': 'Document does not exist.'}, 404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to delete this document."}, 403)
    timdb.documents.deleteDocument(getNewest(doc_id))
    return "Success"

@app.route('/edit/<int:doc_id>')
@app.route("/documents/<int:doc_id>")
def editDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    verifyEditAccess(doc_id)
    newest = getNewest(doc_id)
    doc_metadata = timdb.documents.getDocument(newest)
    xs = timdb.documents.getDocumentAsHtmlBlocks(newest)
    (plugins,texts) = pluginControl.pluginify(xs, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
    (jsPaths, cssPaths, modules) = pluginControl.getPluginDatas(plugins)
    modules.append("ngSanitize")
    modules.append("angularFileUpload")
    return render_template('editing.html', docId=doc_metadata['id'], docName=doc_metadata['name'], text=json.dumps(texts), version={'hash' : newest.hash}, js=jsPaths, css=cssPaths, jsMods=modules)


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

def getNewest(docId):
    docId = int(docId)
    timdb = getTimDb()
    version = timdb.documents.getNewestVersion(docId)['hash']
    return DocIdentifier(docId, version)
    
@app.route("/newParagraph/", methods=["POST"])
def addBlock():
    timdb = getTimDb()
    jsondata = request.get_json()
    blockText = jsondata['text']
    docId = jsondata['docId']
    verifyEditAccess(docId)
    paragraph_id = jsondata['par']
    blocks, version = timdb.documents.addMarkdownBlock(getNewest(docId), blockText, int(paragraph_id))
    (plugins, preparedBlocks) = pluginControl.pluginify(blocks, getCurrentUserName(), timdb.answers, docId, getCurrentUserId())
    (jsPaths, cssPaths, modules) = pluginControl.getPluginDatas(plugins)
    return jsonResponse({'texts' : preparedBlocks, 'js':jsPaths,'css':cssPaths,'angularModule':modules})

@app.route("/deleteParagraph/<int:docId>/<int:blockId>")
def removeBlock(docId, blockId):
    timdb = getTimDb()
    verifyEditAccess(docId)
    timdb.documents.deleteParagraph(getNewest(docId), blockId)
    return "Successfully removed paragraph"

@app.route("/<plugin>/<path:fileName>")
def pluginCall(plugin, fileName):
    try:
        req = containerLink.callPluginResource(plugin, fileName)
        return Response(stream_with_context(req.iter_content()), content_type = req.headers['content-type'])
    except PluginException:
        abort(404)

@app.route("/view/<int:doc_id>")
def viewDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    verifyViewAccess(doc_id)
    versions = timdb.documents.getDocumentVersions(doc_id)
    xs = timdb.documents.getDocumentAsHtmlBlocks(DocIdentifier(doc_id, versions[0]['hash']))
    doc = timdb.documents.getDocument(DocIdentifier(doc_id, versions[0]['hash']))
    (plugins,texts) = pluginControl.pluginify(xs, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
    (jsPaths, cssPaths, modules) = pluginControl.getPluginDatas(plugins)
    modules.append("ngSanitize")
    modules.append("angularFileUpload")
    return render_template('view.html', docID=doc['id'], docName=doc['name'], text=json.dumps(texts), version=versions[0], js=jsPaths, css=cssPaths, jsMods=modules)


@app.route("/postNote", methods=['POST'])
def postNote():
    jsondata = request.get_json()
    noteText = jsondata['text']
    #group_id = jsondata['group_id']
    
    docId = jsondata['doc_id']
    paragraph_id = jsondata['par_id']
    timdb = getTimDb()
    group_id = timdb.users.getUserGroups(getCurrentUserId())[0]['id']
    timdb.notes.addNote(group_id, noteText, int(docId), int(paragraph_id))
    #TODO: Handle error.
    return "Success"

@app.route("/editNote", methods=['POST'])
def editNote():
    jsondata = request.get_json()
    noteText = jsondata['text']
    noteId = jsondata['note_id']
    timdb = getTimDb()
    verifyEditAccess(noteId)
    timdb.notes.modifyNote(noteId, noteText)
    return "Success"

@app.route("/deleteNote", methods=['POST'])
def deleteNote():
    jsondata = request.get_json()
    noteId = int(jsondata['note_id'])
    timdb = getTimDb()
    verifyEditAccess(noteId)
    timdb.notes.deleteNote(noteId)
    return "Success"

@app.route("/notes/<int:doc_id>")
def getNotes(doc_id):
    timdb = getTimDb()
    notes = timdb.notes.getNotes(getCurrentUserId(), doc_id)
    return jsonResponse(notes)

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

    # Get the newest answer (state)
    state = oldAnswers[0]['content'] if len(oldAnswers) > 0 else None
    
    markup = getPluginMarkup(doc_id, plugintype, task_id_name)
    if markup is None:
        return jsonResponse({'error' : 'The task was not found in the document.'}, 404)
    if markup == "YAMLERROR: Malformed string":
        return jsonResponse({'error' : 'Plugin markup YAML is malformed.'}, 400)

    answerCallData = {'markup' : markup, 'state' : state, 'input' : answerdata}

    pluginResponse = containerLink.callPluginAnswer(plugintype, answerCallData)
  
    try:
        jsonresp = json.loads(pluginResponse)
    except ValueError:
        return jsonResponse({'error' : 'The plugin response was not a valid JSON string. The response was: ' + pluginResponse}, 400)
    
    if not 'web' in jsonresp:
        return jsonResponse({'error' : 'The key "web" is missing in plugin response.'}, 400)
    
    if 'save' in jsonresp:
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
            markup = pluginControl.getBlockYaml(block)
            return markup
    return None
    
@app.route("/")
def indexPage():
    return render_template('index.html', userName=getCurrentUserName(), userId=getCurrentUserId())

@app.route("/login", methods=['POST'])
def login():
    userName = request.form['user_name']
    timdb = getTimDb()
    userId = timdb.users.getUserByName(userName)
    
    #For now we just create a user if it doesn't exist.
    if userId is None:
        uid = timdb.users.createUser(userName)
        gid = timdb.users.createUserGroup(userName)
        timdb.users.addUserToGroup(gid, uid)
        userId = uid
    session['user_id'] = userId
    session['user_name'] = userName
    flash('You were successfully logged in.')
    return redirect(url_for('indexPage'))

@app.route("/logout", methods=['POST'])
def logout():
    session.pop('user_id', None)
    session['user_name'] = 'Anonymous'
    flash('You were successfully logged out.')
    return redirect(url_for('indexPage'))
    
if __name__ == "__main__":
#    app.debug = True
#    app.run()
    app.wsgi_app = ReverseProxied(app.wsgi_app)	
    app.run(host='0.0.0.0',port=5000)

