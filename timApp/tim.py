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
from timdb import TimDb


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
    return json.dumps(timdb.getDocuments())


def getTimDb():
    if not hasattr(g, 'timdb'):
        g.timdb = TimDb(db_path=app.config['DATABASE'], files_root_path=app.config['FILES_PATH'])
    return g.timdb


@app.route("/getJSON/<textFile>/")
def getJSON(textFile):
    timdb = getTimDb()
    try:
        texts = timdb.getDocumentBlocks(int(textFile))
        doc = timdb.getDocument(int(textFile))
        return jsonify({"name" : doc['name'], "text" : texts})
    except IOError as err:
        print(err)
        return "No data found"

@app.route("/postParagraph/", methods=['POST'])
def postParagraph():
    paragraphText = request.get_json()['text']
    paragraphName = request.get_json()['par']
    
    timdb = getTimDb()
    try:
        timdb.modifyMarkDownBlock(int(paragraphName), paragraphText)
    except IOError as err:
        print(err)
        return "Failed to modify block."
    return "Success"
    
#     fileName = request.get_json()['documentName']
#     if(verifyPath.verifyDataPath(fileName+"/"+paragraphName)):
#         with open(DATA_PATH + fileName + '/' + paragraphName , 'w') as f:
#             f.write(paragraphText)
#         return "Success"
#     print ("Failed to write file")
#     return "Path may be corrupt"

@app.route("/documents/<doc_id>")
def getDocument(doc_id):
    timdb = getTimDb()
    try:
        texts = timdb.getDocumentBlocks(int(doc_id))
        doc = timdb.getDocument(int(doc_id))
        return render_template('editing.html', name=doc['name'], text=json.dumps(texts))
    except ValueError:
        return redirect(url_for('goat'))


@app.route("/pluginCall/<plugin>/<params>")
def callHello(plugin, params):
    return callPlugin(plugin, params).decode('utf-8')

@app.route("/hello", methods=['POST'])
def hello():
    html = request.get_json()['html']
    
@app.route("/view/<doc_id>")
def viewDocument(doc_id):
    timdb = getTimDb()
    try:
        #texts = timdb.getDocumentBlocks(int(doc_id))
        doc = timdb.getDocument(int(doc_id))
        return render_template('view.html', docID=doc['id'], docName=doc['name'])
    except ValueError:
        return redirect(url_for('goat'))

@app.route("/")
def getFile():
    return render_template('index.html')


if __name__ == "__main__":
#    app.debug = True
#    app.run()
    app.wsgi_app = ReverseProxied(app.wsgi_app)	
    app.run(host='0.0.0.0',port=5000)
