# -*- coding: utf-8 -*-
from flask import Flask, redirect, url_for, Markup
from flask import render_template, render_template_string
from ReverseProxied import ReverseProxied
import markdown
import parseParagraphs as p
from verifyPath import verifyPath
import json

app = Flask(__name__) 
STATIC_PATH = "./static/"

@app.route("/getFile/<name>/<textFile>/")
def getFile(name, textFile=None):
    try:
        #Check for path injections
        if verifyPath(name, textFile):
            texts = []
            pars = p.getDocumentPars(STATIC_PATH + str(textFile))
            for par in pars:
                with open(STATIC_PATH + name + "/" + par.strip(), 'r') as f:
                    texts.append({"par" : par, "text" : Markup(markdown.markdown(unicode(f.read(), encoding="utf-8")))})
        else: 
            return redirect(url_for('goat'))
        return render_template('start.html',name=name, text=json.dumps(texts))
    except IOError:
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
    app.debug = True
    app.run()
#    app.wsgi_app = ReverseProxied(app.wsgi_app)	
#    app.run(host='0.0.0.0',port=5000)
