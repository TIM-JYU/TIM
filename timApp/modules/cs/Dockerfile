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

# Poistetaan pip. Sit‰ ei tarvitse en‰‰ ja saadaan
# hitusen pienempi image ja v‰hemm‰n ohjelmia potentiaalisen
# hyˆkk‰‰j‰n k‰yttˆˆn. (Periaate ok. k‰yt‰nnˆss‰ tosi marginaalinen
# toimenpide)
# run apt-get -y --purge remove python3-pip

# Lis‰t‰‰n alkuper‰inen hakemisto polkuun /service
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
# see: https://github.com/bakketun/docker-sbcl/blob/master/Dockerfile
RUN apt-get install -y sbcl 

# SqLite
RUN apt-get -y install sqlite3 libsqlite3-dev

RUN apt-get install -y libxtst6

RUN apt-get install -y inotify-tools

run apt-get update
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

# Let's start with some basic stuff.
RUN apt-get update -qq && apt-get install -qqy \
    apt-transport-https \
    ca-certificates \
    curl \
    lxc \
    iptables

# Install Docker from Docker Inc. repositories.
#RUN curl -sSL https://get.docker.com/ubuntu/ | sh

# Install the magic wrapper.
#ADD ./wrapdocker /usr/local/bin/wrapdocker
#RUN chmod u+s /usr/local/bin/wrapdocker
#RUN chmod +x /usr/local/bin/wrapdocker

# Define additional metadata for our image.
#VOLUME /var/lib/docker
#CMD ["sudo","wrapdocker"]
# RUN sudo /usr/local/bin/wrapdocker


# Lis‰t‰‰n k‰ytt‰j‰ `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
# run useradd -M agent 
run useradd -m agent
run chown -R agent /service

# run groupadd docker
#run gpasswd -a agent docker
#run usermod -a -G sudo agent

# testej‰ varten
ENV MYPASSWORD password
RUN echo root:kissa | chpasswd
#RUN echo agent:kissa | chpasswd

# Varmuudeksi estet‰‰n root login ja poistetaan rootin salasana,
# jos sellainen on.
# run usermod -p '!' root
# run passwd -l root

# Asetetaan container k‰ynnistym‰‰n tunnuksella agent.
user agent

# Avataan portti 5000 ulos containerista.
expose 5000

#volume /tim_files/:/service/tim_files/

