# -*- coding: utf-8 -*-
from flask import Flask, redirect, url_for, Markup
from flask import render_template, render_template_string
from flask import g
from ReverseProxied import ReverseProxied
import markdown
import parseParagraphs as p
from verifyPath import verifyPath
import json
import os
import sqlite3
from enum import Enum

BlockType = Enum('BlockType', 'DocumentBlock Comment Note Answer')
app = Flask(__name__) 
app.config.from_object(__name__)

# Load default config and override config from an environment variable
app.config.update(dict(
    DATABASE=os.path.join(app.root_path, 'tim.db'),
    DEBUG=True,
    SECRET_KEY='development key',
    USERNAME='admin',
    PASSWORD='default'
))
app.config.from_envvar('TIM_SETTINGS', silent=True)
STATIC_PATH = "./static/"

def initDb():
    with app.app_context():
        db = getDb()
        with app.open_resource('schema.sql', mode='r') as f:
            db.cursor().executescript(f.read())
        db.commit()

def connectDb():
    """Connects to the specific database."""
    rv = sqlite3.connect(app.config['DATABASE'])
    rv.row_factory = sqlite3.Row
    return rv

def getDb():
    """Opens a new database connection if there is none yet for the
    current application context.
    """
    if not hasattr(g, 'sqlite_db'):
        g.sqlite_db = connectDb()
    return g.sqlite_db

def createUser(name):
    """Creates a new user with the specified name."""
    db = getDb()
    db.execute('insert into User (name) values (?)', [name])
    db.commit()

def createDocument(name):
    """Creates a new document with the specified name."""
    db = getDb()
    db.execute('insert into Document (name) values (?)', [name])
    db.commit()
    #TODO: Create a file for the document in file system.
    #TODO: Put the document file under version control (using a Git module maybe?).
    return

def addMarkDownBlock(document_id, content, previous_block_id):
    """Adds a new markdown block to the specified document."""
    db = getDb()
    db.execute('insert into Block (type_id) values (?)', [BlockType.DocumentBlock])
    block_id = db.last_insert_rowid()
    print(block_id)
    db.commit()
    #TODO: Create a file for the block using its id as the file name.
    #TODO: Modify the document file appropriately.
    return

def modifyMarkDownBlock(block_id, new_content):
    return

def createDocumentFromBlocks(dir, document_name):
    """
    Creates a document from existing blocks in the specified directory.
    The blocks should be ordered alphabetically.
    """
    return

@app.teardown_appcontext
def close_db(error):
    """Closes the database again at the end of the request."""
    if hasattr(g, 'sqlite_db'):
        g.sqlite_db.close()

@app.route("/getFile/<name>/<textFile>/")
def getFile(name, textFile=None):
    try:
        #Check for path injections
        if verifyPath(name, textFile):
            texts = []
            pars = p.getDocumentPars(STATIC_PATH + str(textFile))
            for par in pars:
                with open(STATIC_PATH + name + "/" + par.strip(), 'r', encoding="utf-8") as f:
                    texts.append({"par" : par, "text" : Markup(markdown.markdown(f.read()))})
        else: 
            return redirect(url_for('goat'))
        return render_template('start.html',name=name, text=json.dumps(texts))
    except IOError as err:
        print(err)
        return redirect(url_for('goat'))



#@app.route('/getMarkdown/<file>')
#def getOhj(file):
#    contents = Markup(markdown.markdown(unicode(open("./static/ohj1/" + file).read(),encoding="utf-8")))
#    contents = unicode(open(file+'HTML5', 'r').read(), "utf-8")
#    return render_template('start.html', fileCont=contents, stylesheet='stylesheet.css')

@app.route('/')
@app.route('/<path:path>')
def goat(path=None):
    return render_template('goat.html')

if __name__ == "__main__":
#    app.debug = True
#    app.run()
    app.wsgi_app = ReverseProxied(app.wsgi_app)	
    app.run(host='0.0.0.0',port=5000)