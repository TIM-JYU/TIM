from ubuntu
maintainer Ville Tirronen "ville.tirronen@jyu.fi"
run apt-get install -y python
run apt-get install -y python-pip
run pip install Flask
add . /wiki
expose 5000
volume /wiki/data
cmd cd /wiki/ && python hello-world.py
