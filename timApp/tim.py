# -*- coding: utf-8 -*-
import logging
import os
import imghdr
import io
import re
import posixpath

from flask import Flask, Blueprint
from flask import stream_with_context
from flask import render_template
from flask import send_from_directory
from werkzeug.utils import secure_filename
from flask.helpers import send_file
from bs4 import UnicodeDammit

from ReverseProxied import ReverseProxied
import containerLink
from routes.cache import cache
from routes.answer import answers
from routes.edit import edit_page
from routes.manage import manage_page
from routes.view import view_page
from routes.login import login_page
from timdb.timdbbase import TimDbException
from containerLink import PluginException
from routes.settings import settings_page
from routes.common import *


app = Flask(__name__)
app.config.from_pyfile('defaultconfig.py', silent=False)
app.config.from_envvar('TIM_SETTINGS', silent=True)
default_secret = app.config['SECRET_KEY']
if not app.config.from_pyfile(app.config['SECRET_FILE_PATH'], silent=True):
    print('WARNING: secret file not found, using default values - do not run in production!')
else:
    assert default_secret != app.config['SECRET_KEY']
#Compress(app)

cache.init_app(app)

app.register_blueprint(settings_page)
app.register_blueprint(manage_page)
app.register_blueprint(edit_page)
app.register_blueprint(view_page)
app.register_blueprint(login_page)
app.register_blueprint(answers)
app.register_blueprint(Blueprint('bower',
                                 __name__,
                                 static_folder='static/scripts/bower_components',
                                 static_url_path='/static/scripts/bower_components'))

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
        filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

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
        abort(403, 'You have to be logged in to upload a file.')
    timdb = getTimDb()

    file = request.files.get('file')
    if file is None:
        abort(400, 'Missing file')
    folder = request.form.get('folder')
    if folder is None:
        return try_upload_image(file)
    filename = posixpath.join(folder, secure_filename(file.filename))

    user_name = getCurrentUserName()
    if not timdb.users.userHasAdminAccess(getCurrentUserId())\
            and not timdb.users.isUserInGroup(user_name, "Timppa-projektiryhmä")\
            and re.match('^users/' + user_name + '/', filename) is None:
        abort(403, "You're not authorized to write here.")

    if not allowed_file(file.filename):
        abort(403, 'The file format is not allowed.')

    if filename.endswith(tuple(DOC_EXTENSIONS)):
        content = UnicodeDammit(file.read()).unicode_markup
        if not content:
            abort(400, 'Failed to convert the file to UTF-8.')
        timdb.documents.importDocument(content, filename, getCurrentUserGroup())
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
    
@app.route('/uploads/<filename>')
def uploaded_file(filename):
    return send_from_directory(app.config['UPLOAD_FOLDER'],filename)

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
        
    #print('req_folder is "{}"'.format(req_folder))
    
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

def createItem(itemName, itemType, createFunction):
    if not loggedIn():
        abort(403, 'You have to be logged in to create a {}.'.format(itemType))

    if itemName.startswith('/') or itemName.endswith('/'):
        abort(400, 'The {} name cannot start or end with /.'.format(itemType))

    if re.match('^(\d)*$', itemName) is not None:
        abort(400, 'The {} name can not be a number to avoid confusion with document id.'.format(itemType))

    timdb = getTimDb()

    userName = getCurrentUserName()

    if timdb.documents.getDocumentId(itemName) is not None or timdb.folders.getFolderId(itemName) is not None:
        abort(403, 'Item with a same name already exists.')

    if not canWriteToFolder(itemName):
        abort(403, 'You cannot create {}s in this folder. Try users/{} instead.'.format(itemType, userName))

    itemId = createFunction(itemName)
    return jsonResponse({'id' : itemId, 'name' : itemName})


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
    #TODO: Handle error.
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
        return jsonResponse({'error' : 'Invalid paragraph specifier.'}, 400)
    timdb.readings.setAsRead(getCurrentUserGroup(), doc_id, doc_ver, specifier)
    return "Success"


@app.route("/read/<int:doc_id>", methods=['PUT'])
def setAllAsRead(doc_id):
    verifyReadMarkingRight(doc_id)
    timdb = getTimDb()
    version = request.headers.get('Version', '')
    verify_document_version(doc_id, version)
    blocks = timdb.documents.getDocumentAsBlocks(getNewest(doc_id))
    doc_ver = timdb.documents.getNewestVersionHash(doc_id)
    timdb.readings.setAllAsRead(getCurrentUserGroup(), doc_id, doc_ver, len(blocks))
    return "Success"


@app.route("/")
def startPage():
    return render_template('start.html')

@app.route("/view/")
def indexPage():
    timdb = getTimDb()
    possible_groups = timdb.users.getUserGroupsPrintable(getCurrentUserId())
    return render_template('index.html',
                           userName=getCurrentUserName(),
                           userId=getCurrentUserId(),
                           userGroups=possible_groups)


def startApp():
    app.wsgi_app = ReverseProxied(app.wsgi_app)
    #app.wsgi_app = ProfilerMiddleware(app.wsgi_app, sort_by=('cumtime',))
    app.run(host='0.0.0.0', port=5000, use_reloader=False)
