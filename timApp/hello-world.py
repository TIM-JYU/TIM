
from flask import Flask, redirect, url_for
from flask import render_template
from ReverseProxied import ReverseProxied
app = Flask(__name__) 

@app.route("/getFile/<name>/<textFile>/")
def getFile(name, textFile=None):
    if (str(name) != "samuel" or str(textFile) == ""):
        return redirect(url_for('goat'))
    path = "./" + name + "/" + textFile 
    try:
        return render_template('start.html',name=name, text= open(str(path), 'r').read())
    except IOError: 
        return redirect(url_for('goat'))
@app.route('/')
@app.route('/<path:path>')
def goat(path=None):
    return render_template('goat.html')


if __name__ == "__main__":
#    app.run()
    app.wsgi_app = ReverseProxied(app.wsgi_app)	
    app.run(host='0.0.0.0',port=5000)
