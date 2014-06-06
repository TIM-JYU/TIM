# -*- coding: utf-8 -*-
from flask import Flask, redirect, url_for
from flask import render_template, render_template_string
from flask import g
from ReverseProxied import ReverseProxied
import json
import os
from os import listdir
from os.path import isfile,join

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

@app.route("/getFile/<textFile>/")
def getFile(textFile):
    mypath = STATIC_PATH + "/data/"+textFile
    try:
        texts = []
        pars = [ f for f in listdir(mypath) if isfile(join(mypath,f)) ]
        pars.sort()
        for par in pars:
            with open(STATIC_PATH + "/data/" + textFile + "/" + par, 'r', encoding="utf-8") as f:
                texts.append({"par" : par, "text" : f.read()})
        return render_template('start.html', text=json.dumps(texts))
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
