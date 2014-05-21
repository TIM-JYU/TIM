from flask import Flask
from ReverseProxied import ReverseProxied
app = Flask(__name__)

@app.route("/")
def hello():
    return "Hello World!"

if __name__ == "__main__":
     app.wsgi_app = ReverseProxied(app.wsgi_app)	
     app.run(host='0.0.0.0',port=5000)
