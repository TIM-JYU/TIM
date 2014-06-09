from shovel import task
import subprocess


@task
def lint():
    pylint()
    jslint()

@task
def pylint():
    res = ""
    try:
        res = subprocess.check_output("pylint --output-format=html timApp/*.py",shell=True)
        with open("/var/TimDev/lint.html","wb") as file:
                file.write(res)
    except subprocess.CalledProcessError as e:
        with open("/var/TimDev/lint.html","wb") as file:
                file.write(e.output)
        
@task
def jslint():
    res = ""
    try:
        res = subprocess.check_output("jshint timApp/static/scripts/*.js",shell=True)
        with open("/var/TimDev/jslint.txt","wb") as file:
                file.write(res)
    except subprocess.CalledProcessError as e:
        with open("/var/TimDev/jslint.txt","wb") as file:
                file.write(e.output)

