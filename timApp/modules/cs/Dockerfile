# Aloitetaan ubuntun imagesta
from ubuntu:16.04
maintainer vesal "vesal@jyu.fi"

# Workaround PAM: system error -messagelle
run ln -fs /bin/true /usr/bin/chfn

# Aikavyöhyke ja merkistöt kuntoon
run apt-get update
run apt-get install -y locales tzdata
run locale-gen en_US.UTF-8
env LANG en_US.UTF-8
env LANGUAGE en_US:en
env LC_ALL en_US.UTF-8
run echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata

# tuntuu valittavan koko ajan jos ei tätä asenna
run apt-get install -y apt-utils

run apt-get install -y acl unzip

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
run pip3 install control

# Octave
#run apt-add-repository ppa:octave/stable
#run apt-get update

# run apt-get install -y octave less
run add-apt-repository ppa:octave/stable
run apt-get update


run apt-get install -y epstool 
run apt-get install -y transfig


# run apt-get install -y octave-control
# run apt-get install -y octave-io

#env LD_LIBRARY_PATH /usr/lib/jvm/java-8-oracle/jre/lib/amd64/server
# run javareconf
run ln -s /usr/lib/jvm/java-8-oracle /usr/lib/jvm/default-java


# run apt-get install -y octave-signal
# run apt-get install -y octave-statistics
run apt-get install -y octave-control octave-image octave-io octave-optim octave-signal octave-statistics


run apt-get install -y lua5.2
    
run apt-get install -y r-cran-mvtnorm

# Swift begins see https://github.com/swiftdocker/docker-swift/blob/master/Dockerfile
# Install related packages and set LLVM 3.6 as the compiler
RUN apt-get -q update && \
    apt-get -q install -y \
    make \
    libc6-dev \
    clang-3.6 \
    curl \
    libedit-dev \
    python2.7 \
    python2.7-dev \
    libicu-dev \
    rsync \
    libxml2 \
    git \
    libcurl4-openssl-dev \
    && update-alternatives --quiet --install /usr/bin/clang clang /usr/bin/clang-3.6 100 \
    && update-alternatives --quiet --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.6 100 \
    && rm -r /var/lib/apt/lists/*

# Everything up to here should cache nicely between Swift versions, assuming dev dependencies change little
ENV SWIFT_BRANCH=swift-3.0.2-release \
    SWIFT_VERSION=swift-3.0.2-RELEASE \
    SWIFT_PLATFORM=ubuntu16.04 \
    PATH=/usr/bin:$PATH

# Download GPG keys, signature and Swift package, then unpack and cleanup
RUN SWIFT_URL=https://swift.org/builds/$SWIFT_BRANCH/$(echo "$SWIFT_PLATFORM" | tr -d .)/$SWIFT_VERSION/$SWIFT_VERSION-$SWIFT_PLATFORM.tar.gz \
    && curl -fSsL $SWIFT_URL -o swift.tar.gz \
    && curl -fSsL $SWIFT_URL.sig -o swift.tar.gz.sig \
    && export GNUPGHOME="$(mktemp -d)" \
    && set -e; \
        for key in \
      # pub   4096R/412B37AD 2015-11-19 [expires: 2017-11-18]
      #       Key fingerprint = 7463 A81A 4B2E EA1B 551F  FBCF D441 C977 412B 37AD
      # uid                  Swift Automatic Signing Key #1 <swift-infrastructure@swift.org>
          7463A81A4B2EEA1B551FFBCFD441C977412B37AD \
      # pub   4096R/21A56D5F 2015-11-28 [expires: 2017-11-27]
      #       Key fingerprint = 1BE1 E29A 084C B305 F397  D62A 9F59 7F4D 21A5 6D5F
      # uid                  Swift 2.2 Release Signing Key <swift-infrastructure@swift.org>
          1BE1E29A084CB305F397D62A9F597F4D21A56D5F \
      # pub   4096R/91D306C6 2016-05-31 [expires: 2018-05-31]
      #       Key fingerprint = A3BA FD35 56A5 9079 C068  94BD 63BC 1CFE 91D3 06C6
      # uid                  Swift 3.x Release Signing Key <swift-infrastructure@swift.org>
          A3BAFD3556A59079C06894BD63BC1CFE91D306C6 \
        ; do \
          gpg --quiet --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
        done \
    && gpg --batch --verify --quiet swift.tar.gz.sig swift.tar.gz \
    && tar -xzf swift.tar.gz --directory / --strip-components=1 \
    && rm -r "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz
run chmod -R 755 /usr/lib/swift
# Swift ends
    
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

#run chmod 777 /etc/ssh
#run chmod 777 /etc/ssh/*

# Laita seuraava kommentteihin tuotantoa varten
##run usermod -a -G sudo agent

# Asetetaan container käynnistymään tunnuksella agent.
user agent

# Avataan portti 5000 ulos containerista.
expose 5000

#volume /tim_files/:/service/tim_files/

