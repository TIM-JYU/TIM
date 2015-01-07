# Aloitetaan ubuntun imagesta
from ubuntu
maintainer vesal "vesal@jyu.fi"

# Asennetaan python, pip ja flask

run apt-get update
run apt-get install -y python3
run apt-get install -y python3-pip
run pip3 install Flask
run pip3 install bleach
run apt-get install -y mono-complete fsharp
run apt-get install -y libsdl1.2debian
run apt-get install -y libsdl-mixer1.2
run apt-get install -y xvfb
# run apt-get install -y python  
run apt-get install -y imagemagick
run apt-get install -y openjdk-7-jre-headless
run apt-get install -y nunit-console
run apt-get install -y wget
run locale-gen en_US.UTF-8 
run locale-gen fi_FI.UTF-8 

# Poistetaan pip. Sitä ei tarvitse enää ja saadaan
# hitusen pienempi image ja vähemmän ohjelmia potentiaalisen
# hyökkääjän käyttöön. (Periaate ok. käytännössä tosi marginaalinen
# toimenpide)
# run apt-get -y --purge remove python3-pip

# Lisätään alkuperäinen hakemisto polkuun /service
run mkdir /service

#
# Oracle Java 8 Dockerfile
#
# https://github.com/dockerfile/java
# https://github.com/dockerfile/java/tree/master/oracle-java8
#
# Install Java.
run apt-get install -y software-properties-common 
#python-software-properties
RUN \
  echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
  add-apt-repository -y ppa:webupd8team/java && \
  apt-get update && \
  apt-get install -y oracle-java8-installer && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /var/cache/oracle-jdk8-installer

# Define working directory.
#WORKDIR /data

# Define commonly used JAVA_HOME variable
ENV JAVA_HOME /usr/lib/jvm/java-8-oracle

# Commonlisp
RUN apt-get update
# RUN apt-get install -y clisp
RUN apt-get install -y sbcl 

# Lisätään käyttäjä `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
#run useradd -M agent 
run useradd -m agent
run chown -R agent /service

# testejä varten
ENV MYPASSWORD password
RUN echo "root:$MYPASSWORD" | chpasswd

# Varmuudeksi estetään root login ja poistetaan rootin salasana,
# jos sellainen on.
# run usermod -p '!' root
# run passwd -l root

# Asetetaan container käynnistymään tunnuksella agent.
user agent

# Avataan portti 5000 ulos containerista.
expose 5000

#volume /tim_files/:/service/tim_files/
