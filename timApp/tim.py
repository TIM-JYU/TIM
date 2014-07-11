# -*- coding: utf-8 -*-
from flask import jsonify, Flask, redirect, url_for
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
from timdb.timdbbase import TimDbException
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

ALLOWED_EXTENSIONS = set(['png', 'jpg', 'jpeg', 'gif'])
STATIC_PATH = "./static/"
DATA_PATH = "./static/data/"

@app.route('/upload/', methods=['POST'])
def upload_file():
    print("File received, checking contents...")
    if request.method == 'POST':
        file = request.files['file']
        if file and allowed_file(file.filename):
            filename = secure_filename(file.filename)
            file.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
            print("File contents safe, saving.")
            return redirect(url_for('uploaded_file', filename=filename))

@app.route('/uploads/<filename>')
def uploaded_file(filename):
    return send_from_directory(app.config['UPLOAD_FOLDER'],filename)

@app.route("/getDocuments/")
def getDocuments():
    timdb = getTimDb()
    return Response(json.dumps(timdb.documents.getDocuments()), mimetype='application/json')

def getCurrentUserId():
    return 1

def getTimDb():
    if not hasattr(g, 'timdb'):
        g.timdb = TimDb(db_path=app.config['DATABASE'], files_root_path=app.config['FILES_PATH'])
    return g.timdb


@app.route("/getJSON/<int:doc_id>/")
def getJSON(doc_id):
    timdb = getTimDb()
    try:
        texts = timdb.documents.getDocumentBlocks(doc_id)
        doc = timdb.documents.getDocument(doc_id)
        return jsonify({"name" : doc['name'], "text" : texts})
    except IOError as err:
        print(err)
        return "No data found"

@app.route("/getJSON-HTML/<int:doc_id>")
def getJSON_HTML(doc_id):
    timdb = getTimDb()
    try:
        blocks = timdb.documents.getDocumentAsHtmlBlocks(doc_id)
        doc = timdb.documents.getDocument(doc_id)
        return jsonify({"name" : doc['name'], "text" : blocks})
    except ValueError as err:
        print(err)
        return "[]"
    except TimDbException as err:
        print(err)
        return "[]"

@app.route("/postParagraph/", methods=['POST'])
def postParagraph():
    documentName = request.get_json()['docName']
    paragraphText = request.get_json()['text']
    paragraphName = request.get_json()['par']
    
    timdb = getTimDb()
    try:
        timdb.documents.modifyMarkDownBlock(documentName,int(paragraphName), paragraphText)
    except IOError as err:
        print(err)
        return "Failed to modify block."
    return "Success"

@app.route("/createDocument", methods=["POST"])
def createDocument():
    jsondata = request.get_json()
    docName = jsondata['doc_name']
    timdb = getTimDb()
    docId = timdb.documents.createDocument(docName)
    return jsonify({'id' : docId})

@app.route("/documents/<int:doc_id>")
def getDocument(doc_id):
    timdb = getTimDb()
    try:
        texts = timdb.documents.getDocumentAsHtmlBlocks(doc_id)
        doc = timdb.documents.getDocument(doc_id)
        return render_template('editing.html', docId=doc['id'], name=doc['name'], text=json.dumps(texts))
    except ValueError:
        return redirect(url_for('goat'))

@app.route("/getBlock/<int:docId>/<int:blockId>")
def getBlockMd(docId, blockId):
    timdb = getTimDb()
    block = timdb.documents.getBlock(docId, blockId)
    return block

@app.route("/getBlockHtml/<int:docId>/<int:blockId>")
def getBlockHtml(docId, blockId):
    timdb = getTimDb()
    block = timdb.documents.getBlockAsHtml(docId, blockId)
    return block

@app.route("/postBlock/", methods=["POST"])
def postBlock():
    timdb = getTimDb()
    request.get_json()['text']
    return ""

@app.route("/pluginCall/<plugin>/<params>")
def callHello(plugin, params):
    return callPlugin(plugin, params).decode('utf-8')

@app.route("/hello", methods=['POST'])
def hello():
    html = request.get_json()['html']
    
@app.route("/view/<int:doc_id>")
def viewDocument(doc_id):
    timdb = getTimDb()
    try:
        #texts = timdb.getDocumentBlocks(doc_id)
        doc = timdb.documents.getDocument(doc_id)
        return render_template('view.html', docID=doc['id'], docName=doc['name'])
    except ValueError:
        return redirect(url_for('goat'))

@app.route("/postNote", methods=['POST'])
def postNote():
    jsondata = request.get_json()
    noteText = jsondata['text']
    group_id = jsondata['group_id']
    docId = jsondata['doc_id']
    paragraph_id = jsondata['par_id']
    timdb = getTimDb()
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
def getFile():
    return render_template('index.html')

@app.route("/login", methods=['POST'])
def login():
    jsondata = request.get_json()
    userName = jsondata['user_name']
    
    
if __name__ == "__main__":
#    app.debug = True
#    app.run()
    app.wsgi_app = ReverseProxied(app.wsgi_app)	
    app.run(host='0.0.0.0',port=5000)
