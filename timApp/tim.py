# -*- coding: utf-8 -*-
from flask import Flask, redirect, url_for, session, abort, flash
from flask import render_template
from flask import g
from flask import request
from flask import send_from_directory
from ReverseProxied import ReverseProxied
import json
import os
from containerLink import callPlugin
from werkzeug.utils import secure_filename
from timdb.timdb2 import TimDb
from timdb.timdbbase import TimDbException, DocIdentifier
from flask import Response
import imghdr
from flask.helpers import send_file
import io
import pluginControl
from htmlSanitize import sanitize_html

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

def allowed_file(filename):
    return '.' in filename and \
        filename.rsplit('.', 1)[1] in ALLOWED_EXTENSIONS

#app.config.from_envvar('TIM_SETTINGS', silent=True)

if os.path.abspath('.') == '/service':
    app.config['DEBUG'] = False

DOC_EXTENSIONS = ['txt', 'md', 'markdown']
PIC_EXTENSIONS = ['png', 'jpg', 'jpeg', 'gif']
ALLOWED_EXTENSIONS = set(PIC_EXTENSIONS + DOC_EXTENSIONS)
STATIC_PATH = "./static/"
DATA_PATH = "./static/data/"

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

def verifyEditAccess(block_id):
    timdb = getTimDb()
    if not timdb.users.userHasEditAccess(getCurrentUserId(), block_id):
        abort(403, "Sorry, you don't have permission to edit this resource.")

def verifyViewAccess(block_id):
    timdb = getTimDb()
    if not timdb.users.userHasViewAccess(getCurrentUserId(), block_id):
        abort(403, "Sorry, you don't have permission to view this resource.")

@app.route("/manage/<int:doc_id>")
def manage(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    doc_data = timdb.documents.getDocument(DocIdentifier(doc_id, ''))
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

@app.route('/download/<int:doc_id>')
def downloadDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    verifyViewAccess(doc_id)
    doc_data = timdb.documents.getDocumentMarkdown(getNewest(doc_id))
    return Response(doc_data, mimetype="text/plain")

@app.route('/upload/', methods=['POST'])
def upload_file():
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
    docs = timdb.documents.getDocuments()
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
    version = request.headers.get('Version')
    identifier = getNewest(docId)#DocIdentifier(docId, version)
    
    try:
        blocks, version = timdb.documents.modifyMarkDownBlock(identifier, int(parIndex), paragraphText)
    except IOError as err:
        print(err)
        return "Failed to modify block."
    # Replace appropriate elements with plugin content, load plugin requirements to template
    (plugins, preparedBlocks) = pluginControl.pluginify(blocks, getCurrentUserName())
    (jsPaths, cssPaths, modules) = pluginControl.getPluginDatas(plugins)

    return jsonResponse({'texts' : preparedBlocks, 'js':jsPaths,'css':cssPaths,'angularModule':modules})

@app.route("/createDocument", methods=["POST"])
def createDocument():
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

@app.route("/edit/<int:doc_id>")
@app.route("/documents/<int:doc_id>")
def editDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    verifyEditAccess(doc_id)
    newest = getNewest(doc_id)
    doc_metadata = timdb.documents.getDocument(newest)
    xs = timdb.documents.getDocumentAsHtmlBlocks(newest)
    (plugins,texts) = pluginControl.pluginify(xs, getCurrentUserName()) 
    (jsPaths, cssPaths, modules) = pluginControl.getPluginDatas(plugins)
    modules.append("ngSanitize")
    modules.append("angularFileUpload")
    print(modules)
    return render_template('editing.html', docId=doc_metadata['id'], name=doc_metadata['name'], text=json.dumps(texts), version={'hash' : newest.hash}, js=jsPaths, css=cssPaths, jsMods=modules)


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
    (plugins, preparedBlocks) = pluginControl.pluginify(blocks, getCurrentUserName()) 
    (jsPaths, cssPaths, modules) = pluginControl.getPluginDatas(plugins)
    return jsonResponse({'texts' : preparedBlocks, 'js':jsPaths,'css':cssPaths,'angularModule':modules})

@app.route("/deleteParagraph/<int:docId>/<int:blockId>")
def removeBlock(docId, blockId):
    timdb = getTimDb()
    verifyEditAccess(docId)
    timdb.documents.deleteParagraph(getNewest(docId), blockId)
    return "Successfully removed paragraph"

@app.route("/pluginCall/<plugin>", methods=["POST"])
def pluginCall(plugin):
    info = request.get_json()
    html = callPlugin(plugin, info)
    return html

@app.route("/view/<int:doc_id>")
def viewDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    verifyViewAccess(doc_id)
    versions = timdb.documents.getDocumentVersions(doc_id)
    xs = timdb.documents.getDocumentAsHtmlBlocks(DocIdentifier(doc_id, versions[0]['hash']))
    doc = timdb.documents.getDocument(DocIdentifier(doc_id, versions[0]['hash']))
    (plugins,texts) = pluginControl.pluginify(xs, getCurrentUserName()) 
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

@app.route("/<int:doc_id>/<plugintype>/<task_id>/answer", methods=['PUT'])
def saveAnswer(doc_id, plugintype, task_id):
    timdb = getTimDb()
    
    answerdata = request.get_json()['answer']
    #answerdata = request.form['answer']
    
    # Load old state
    oldAnswers = timdb.answers.getAnswers(getCurrentUserId(), task_id)
    
    # Get the newest
    state = oldAnswers[0]['content']
    
    markup = getPluginMarkup(doc_id, plugintype, task_id)
    if markup is None:
        return jsonResponse({'error' : 'The task was not found in the document.'}, 404)
    
    # TODO: Call plugin's answer route
    pluginResponse = callPlugin(plugintype, {'markup' : markup, 'state' : state, 'input' : answerdata})
    
    # Assuming the JSON is in a string
    jsonresp = json.loads(pluginResponse)
    
    #Save the new state
    timdb.answers.saveAnswer([getCurrentUserId()], "{}.{}".format(doc_id, task_id), json.dumps(jsonresp['state']), jsonresp['state']['points'])
    return jsonResponse(jsonresp['web'])

def getPluginMarkup(doc_id, plugintype, task_id):
    timdb = getTimDb()
    doc_markdown = timdb.documents.getDocumentAsHtmlBlocks(getNewest(doc_id))
    for block in doc_markdown:
        if('plugin="{}"'.format(plugintype) in block and "<code>" in block and '<pre id="{}"'.format(task_id) in block):
            return pluginControl.prepPluginCall(block)
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
        gid = timdb.users.createUserGroup('group of user %s' % userName)
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