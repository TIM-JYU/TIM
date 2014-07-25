# -*- coding: utf-8 -*-
from flask import jsonify, Flask, redirect, url_for, session, abort, flash
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

ALLOWED_EXTENSIONS = set(['png', 'jpg', 'jpeg', 'gif', 'txt'])
STATIC_PATH = "./static/"
DATA_PATH = "./static/data/"

@app.errorhandler(403)
def forbidden(error):
    return render_template('403.html', message=error.description), 403

@app.route('/upload/', methods=['POST'])
def upload_file():
    timdb = getTimDb()
    if request.method == 'POST':
        doc = request.files['file']
        if allowed_file(doc.filename):
            filename = secure_filename(doc.filename)
            if(".txt" in filename):
                doc.save("./uploadedDocs/" + filename)
                print("saved file")
                timdb.documents.importDocument("./uploadedDocs/" + filename, filename, 0)
                return "Succesfully uploaded document"
            else:
                doc.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
                print("File contents safe, saving.")
                return redirect(url_for('uploaded_file', filename=filename))

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
        doc['owner'] = timdb.users.userIsOwner(getCurrentUserId(), doc['id'])
    return Response(json.dumps(allowedDocs), mimetype='application/json')

def getCurrentUserId():
    uid = session.get('user_id')
    return uid if uid is not None else 0

def getTimDb():
    if not hasattr(g, 'timdb'):
        g.timdb = TimDb(db_path=app.config['DATABASE'], files_root_path=app.config['FILES_PATH'])
    return g.timdb


@app.route("/getJSON/<int:doc_id>/")
def getJSON(doc_id):
    timdb = getTimDb()
    try:
        texts = timdb.documents.getDocumentBlocks(getNewest(doc_id))
        doc = timdb.documents.getDocument(doc_id)
        return jsonify({"name" : doc['name'], "text" : texts})
    except IOError as err:
        print(err)
        return "No data found"

@app.route("/getJSON-HTML/<int:doc_id>")
def getJSON_HTML(doc_id):
    timdb = getTimDb()
    try:
        newest = getNewest(doc_id)
        blocks = timdb.documents.getDocumentAsHtmlBlocks(newest)
        doc = timdb.documents.getDocument(newest)
        return jsonify({"name" : doc['name'], "text" : blocks})
    except ValueError as err:
        print(err)
        return "[]"
    except TimDbException as err:
        print(err)
        return "[]"

@app.route("/postParagraph/", methods=['POST'])
def postParagraph():
    timdb = getTimDb()
    docId = request.get_json()['docName']
    paragraphText = request.get_json()['text']
    parIndex = request.get_json()['par']
    version = request.headers.get('Version')
    
    try:
        blocks, version = timdb.documents.modifyMarkDownBlock(DocIdentifier(docId, version), int(parIndex), paragraphText)
    except IOError as err:
        print(err)
        return "Failed to modify block."
    return json.dumps(blocks)

@app.route("/createDocument", methods=["POST"])
def createDocument():
    jsondata = request.get_json()
    docName = jsondata['doc_name']
    timdb = getTimDb()
    docId = timdb.documents.createDocument(docName, getCurrentUserGroup())
    return jsonify({'id' : docId.id})

@app.route("/documents/<int:doc_id>")
def getDocument(doc_id):
    timdb = getTimDb()
    if not timdb.users.userHasEditAccess(getCurrentUserId(), doc_id):
        abort(403, "You don't have permission to edit this document.")
    try:
        newest = getNewest(doc_id)
        doc_metadata = timdb.documents.getDocument(newest)
        texts = timdb.documents.getDocumentAsHtmlBlocks(newest)
        return render_template('editing.html', docId=doc_metadata['id'], name=doc_metadata['name'], text=json.dumps(texts), version={'hash' : newest.hash})
    except ValueError:
        return redirect(url_for('goat'))

@app.route("/getBlock/<int:docId>/<int:blockId>")
def getBlockMd(docId, blockId):
    timdb = getTimDb()
    block = timdb.documents.getBlock(getNewest(docId), blockId)
    return block

@app.route("/getBlockHtml/<int:docId>/<int:blockId>")
def getBlockHtml(docId, blockId):
    timdb = getTimDb()
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
    docId = jsondata['docName']
    paragraph_id = jsondata['par']
    blocks = timdb.documents.addMarkdownBlock(getNewest(docId), blockText, int(paragraph_id))
    return json.dumps(blocks)

@app.route("/deleteParagraph/<int:docId>/<int:blockId>")
def removeBlock(docId,blockId):
    timdb = getTimDb()
    timdb.documents.deleteParagraph(getNewest(docId), blockId)
    return "Successfully removed paragraph"

@app.route("/pluginCall/<plugin>/")
def pluginCall(plugin):
    params = request.args.get('param')
    html = callPlugin(plugin, params).decode('utf-8')
    return html

@app.route("/hello", methods=['POST'])
def hello():
    html = request.get_json()['html']
    
@app.route("/view/<int:doc_id>")
def viewDocument(doc_id):
    timdb = getTimDb()
    if not timdb.users.userHasViewAccess(getCurrentUserId(), doc_id):
        abort(403, "You don't have permission to view this document.")
    try:
        #texts = timdb.getDocumentBlocks(doc_id)
        versions = timdb.documents.getDocumentVersions(doc_id)
        fullHtml = timdb.documents.getDocumentAsHtmlBlocks(DocIdentifier(doc_id, versions[0]['hash']))
        doc = timdb.documents.getDocument(DocIdentifier(doc_id, versions[0]['hash']))
        
        return render_template('view.html', docID=doc['id'], docName=doc['name'], text=json.dumps(fullHtml), version=versions[0])
    except ValueError:
        return redirect(url_for('goat'))

def getCurrentUserGroup():
    timdb = getTimDb()
    return timdb.users.getUserGroups(getCurrentUserId())[0]['id']

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
    timdb.notes.modifyNote(noteId, noteText)
    return "Success"

@app.route("/deleteNote", methods=['POST'])
def deleteNote():
    jsondata = request.get_json()
    noteId = int(jsondata['note_id'])
    timdb = getTimDb()
    timdb.notes.deleteNote(noteId)
    return "Success"

@app.route("/notes/<int:doc_id>")
def getNotes(doc_id):
    timdb = getTimDb()
    try:
        #notes = []
        notes = timdb.notes.getNotes(getCurrentUserId(), doc_id)
        return json.dumps(notes)
    except ValueError:
        return redirect(url_for('goat'))

@app.route("/")
def indexPage():
    return render_template('index.html', userName=session.get('user_name'), userId=getCurrentUserId())

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
