# -*- coding: utf-8 -*-
from flask import jsonify, Flask, redirect, url_for
from flask import render_template, render_template_string
from flask import g
from flask import request
from ReverseProxied import ReverseProxied
import json
import os
import verifyPath
from os import listdir
from os.path import isfile,join
from timdb import TimDb
from containerLink import callPlugin

app = Flask(__name__) 
app.config.from_object(__name__)

# Load default config and override config from an environment variable
app.config.update(dict(
    DATABASE=os.path.join(app.root_path, 'tim.db'),
    DEBUG=True,
    SECRET_KEY='development key',
    USERNAME='admin',
    PASSWORD='default',
    FILES_PATH='tim_files'
))
#app.config.from_envvar('TIM_SETTINGS', silent=True)

if os.path.abspath('.') == '/service':
    app.config['DEBUG'] = False

STATIC_PATH = "./static/"
DATA_PATH = "./static/data/"


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
        return render_template('start.html', name=doc['name'], text=json.dumps(texts))
    except ValueError:
        return redirect(url_for('goat'))


@app.route("/pluginCall/<plugin>")
def callHello(plugin):
    print (callPlugin(plugin).decode('utf-8'))
    return callPlugin(plugin).decode('utf-8')

@app.route("/hello", methods=['POST'])
def hello():
    html = request.get_json()['html']
    


@app.route("/")
def getFile():
    return render_template('start.html')


if __name__ == "__main__":
    app.debug = True
    app.run()
#    app.wsgi_app = ReverseProxied(app.wsgi_app)	
#    app.run(host='0.0.0.0',port=5000)
