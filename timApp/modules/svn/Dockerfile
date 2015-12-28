# Aloitetaan ubuntun imagesta
from ubuntu
maintainer vesal "vesal@jyu.fi"

# Asennetaan python, pip ja flask

run apt-get update
run apt-get install -y python3
run apt-get install -y python3-pip
run pip3 install Flask
run pip3 install bleach
run apt-get install -y wget

# Poistetaan pip. Sitä ei tarvitse enää ja saadaan
# hitusen pienempi image ja vähemmän ohjelmia potentiaalisen
# hyökkääjän käyttöön. (Periaate ok. käytännössä tosi marginaalinen
# toimenpide) 
# run apt-get -y --purge remove python3-pip

# pypy3:
run apt-get update
run apt-get install -y curl
RUN curl http://buildbot.pypy.org/nightly/py3k/pypy-c-jit-latest-linux64.tar.bz2 | tar -jxf - && mv pypy* /opt/pypy3
#ENV PYTHONPATH /opt/pypy3/site-packages
ENV PATH $PATH:/opt/pypy3/bin
# ENV LANG C.UTF-8
# install setuptools for pypy3
RUN curl https://bitbucket.org/pypa/setuptools/raw/bootstrap/ez_setup.py | pypy3
RUN rm *.zip
# install pip==1.4.1, because 1.5.4 segfaults for unknown reason
RUN easy_install pip==1.4.1
RUN ln -s /opt/pypy3/bin/pip /opt/pypy3/bin/pip3
run /opt/pypy3/bin/pip3 install bleach


# Lisätään alkuperäinen hakemisto polkuun /service
run mkdir /service

# Sudo jos tarvitaan testeihin
# RUN apt-get update
# RUN apt-get -y install sudo
# RUN useradd -m agent && echo "agent:agent" | chpasswd && adduser agent sudo

# Lisätään käyttäjä `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
run useradd -M agent 
run chown -R agent /service

# testejä varten
#ENV MYPASSWORD password
#RUN echo "root:$MYPASSWORD" | chpasswd

# Varmuudeksi estetään root login ja poistetaan rootin salasana,
# jos sellainen on.
# run usermod -p '!' root
# run passwd -l root

# Asetetaan container käynnistymään tunnuksella agent.
user agent

# Avataan portti 5000 ulos containerista.
expose 5000

#volume /tim_files/:/service/tim_files/

