# Aloitetaan ubuntun imagesta
from ubuntu:16.04
maintainer vesal "vesal@jyu.fi"

# Workaround PAM: system error -messagelle
run ln -fs /bin/true /usr/bin/chfn

# Aikavyöhyke ja merkistöt kuntoon
run locale-gen en_US.UTF-8
env LANG en_US.UTF-8
env LANGUAGE en_US:en
env LC_ALL en_US.UTF-8
run echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata


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
run apt-get install -y openjdk-8-jre-headless
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

# Get GPG keys for Java 8
# run sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 40976EAF437D05B5
run apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 40976EAF437D05B5

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
# see: https://github.com/bakketun/docker-sbcl/blob/master/Dockerfile
RUN apt-get install -y sbcl 

# SqLite
RUN apt-get -y install sqlite3 libsqlite3-dev

RUN apt-get install -y libxtst6

RUN apt-get install -y inotify-tools

run apt-get install -y graphviz

# Configure timezone and locale
RUN echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata
# run locale-gen C.UTF-8 
#env LC_CTYPE C.UTF-8
#env LC_ALL C.UTF-8 
env LC_CTYPE en_US.UTF-8
env LC_ALL en_US.UTF-8
env LANG en_US.UTF-8
env LANGUAGE en_US.UTF-8

RUN apt-get install -y postgresql-client    
    
RUN apt-get install -y r-base

#
# Scala 
#
run apt-get remove scala-library scala
run wget -q http://downloads.typesafe.com/scala/2.11.7/scala-2.11.7.deb
run dpkg -i scala-2.11.7.deb
run apt-get install scala
run rm scala-2.11.7.deb

run apt-get install -y doxygen

run apt-get install -y valgrind

# Install Docker client binary
run wget -q https://get.docker.com/builds/Linux/x86_64/docker-1.10.2.tgz && \
    tar -xzvf docker-1.10.2.tgz && \
    rm docker-1.10.2.tgz

# Python packages

#run pip3 install numpy
#run pip3 install scipy
#run pip3 install scikit-learn
#run pip3 install matplotlib
#run pip3 install statsmodels
#run pip3 install pandas
# http://www.scipy.org/install.html
# sudo apt-get install python-numpy python-scipy python-matplotlib ipython ipython-notebook python-pandas python-sympy python-nose
run apt-get install -y python3-numpy python3-scipy python3-matplotlib python3-pandas 
run pip3 install statsmodels

# Octave
#run apt-add-repository ppa:octave/stable
#run apt-get update

run apt-get install -y octave less   
run apt-get install -y epstool 
run apt-get install -y transfig


run apt-get install -y octave-control
run apt-get install -y octave-io

#env LD_LIBRARY_PATH /usr/lib/jvm/java-8-oracle/jre/lib/amd64/server
# run javareconf
run ln -s /usr/lib/jvm/java-8-oracle /usr/lib/jvm/default-java


run apt-get install -y octave-signal
run apt-get install -y octave-statistics

run apt-get install -y lua5.2
    
# Lisätään käyttäjä `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
# run useradd -M agent 
run useradd -m agent
ENV HOME /home/agent
run mkdir -p /service
run chown -R agent /service

# run groupadd docker
#run gpasswd -a agent docker
#run usermod -a -G sudo agent

# testejä varten
ENV MYPASSWORD password
#RUN echo root:kissa | chpasswd
#RUN echo agent:kissa | chpasswd

# Varmuudeksi estetään root login ja poistetaan rootin salasana,
# jos sellainen on.
# run usermod -p '!' root
# run passwd -l root


# ssh testaamista varten
run apt-get update
run apt-get install -y openssh-server
run mkdir /var/run/sshd
run echo 'root:test' | chpasswd
run sed -i 's/PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config
expose 22

#run chmod 777 /etc/ssh
#run chmod 777 /etc/ssh/*

# Laita seuraava kommentteihin tuotantoa varten
##run usermod -a -G sudo agent

# Asetetaan container käynnistymään tunnuksella agent.
user agent

# Avataan portti 5000 ulos containerista.
expose 5000

#volume /tim_files/:/service/tim_files/

