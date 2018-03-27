FROM ubuntu:16.04
LABEL maintainer="vesal@jyu.fi"

# Workaround for PAM: system error -message
RUN ln -fs /bin/true /usr/bin/chfn

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -r /var/lib/apt/lists/*"

# Timezone configuration
RUN bash -c "${APT_INSTALL} locales tzdata apt-utils acl unzip software-properties-common && ${APT_CLEANUP}"
RUN locale-gen en_US.UTF-8
RUN locale-gen fi_FI.UTF-8
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en \
    LC_ALL=en_US.UTF-8 \
    LC_CTYPE=en_US.UTF-8
RUN echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata

RUN bash -c "${APT_INSTALL} python3 python3-pip python3-setuptools && ${APT_CLEANUP}"
RUN pip3 install wheel
RUN pip3 install Flask bleach

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/ubuntu xenial main" > /etc/apt/sources.list.d/mono-official.list
RUN bash -c "${APT_INSTALL} mono-complete fsharp && ${APT_CLEANUP}"

RUN bash -c "${APT_INSTALL} libsdl1.2debian libsdl-mixer1.2 xvfb imagemagick nunit-console wget firejail && ${APT_CLEANUP}"

# Get GPG keys for Java 8
# RUN sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 40976EAF437D05B5
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 40976EAF437D05B5

#
# Oracle Java 8 Dockerfile
#
# https://github.com/dockerfile/java
# https://github.com/dockerfile/java/tree/master/oracle-java8
#
# Install Java.

RUN \
  echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
  add-apt-repository -y ppa:webupd8team/java && \
  bash -c "${APT_INSTALL} oracle-java8-installer && ${APT_CLEANUP}" && \
  rm -r /var/cache/oracle-jdk8-installer

# Define commonly used JAVA_HOME variable
ENV JAVA_HOME /usr/lib/jvm/java-8-oracle

# ENV LD_LIBRARY_PATH /usr/lib/jvm/java-8-oracle/jre/lib/amd64/server
# RUN javareconf
RUN ln -s /usr/lib/jvm/java-8-oracle /usr/lib/jvm/default-java

# Common Lisp
RUN bash -c "${APT_INSTALL} sbcl sqlite3 libsqlite3-dev libxtst6 inotify-tools graphviz postgresql-client r-base && ${APT_CLEANUP}"

#
# Scala
#
ENV SCALA_VERSION 2.12.3
RUN wget -q http://downloads.typesafe.com/scala/${SCALA_VERSION}/scala-${SCALA_VERSION}.deb && \
 dpkg -i scala-${SCALA_VERSION}.deb && \
 rm scala-${SCALA_VERSION}.deb

RUN bash -c "${APT_INSTALL} doxygen valgrind && ${APT_CLEANUP}"

# Docker client binary
ENV DOCKER_VERSION 17.06.2
RUN wget -q https://download.docker.com/linux/static/stable/x86_64/docker-${DOCKER_VERSION}-ce.tgz && \
    tar -xzvf docker-${DOCKER_VERSION}-ce.tgz --strip=1 -C /usr/bin docker/docker && \
    rm docker-${DOCKER_VERSION}-ce.tgz && docker --version

# Python packages

#RUN pip3 install numpy
#RUN pip3 install scipy
#RUN pip3 install scikit-learn
#RUN pip3 install matplotlib
#RUN pip3 install statsmodels
#RUN pip3 install pandas
# http://www.scipy.org/install.html
# ${APT_INSTALL} python-numpy python-scipy python-matplotlib ipython ipython-notebook python-pandas python-sympy python-nose
RUN bash -c "${APT_INSTALL} python3-numpy python3-scipy python3-matplotlib python3-pandas && ${APT_CLEANUP}"
RUN pip3 install statsmodels
RUN pip3 install control

RUN bash -c "${APT_INSTALL} epstool transfig && ${APT_CLEANUP}"

# Octave
RUN add-apt-repository ppa:octave/stable
# RUN ${APT_INSTALL} octave-statistics && ${APT_CLEANUP}
# RUN ${APT_INSTALL} octave octave-control octave-image octave-io octave-optim octave-signal octave-statistics && ${APT_CLEANUP}
RUN bash -c "${APT_INSTALL} octave octave-control octave-image octave-signal octave-symbolic gnuplot5 pstoedit liboctave-dev && ${APT_CLEANUP}"
RUN wget -q \
 http://downloads.sourceforge.net/octave/statistics-1.3.0.tar.gz \
 http://downloads.sourceforge.net/octave/struct-1.0.14.tar.gz \
 http://downloads.sourceforge.net/octave/optim-1.5.2.tar.gz \
 http://downloads.sourceforge.net/octave/io-2.4.7.tar.gz && \
 printf "pkg install io-2.4.7.tar.gz" | octave && \
 printf "pkg install statistics-1.3.0.tar.gz" | octave && \
 printf "pkg install struct-1.0.14.tar.gz" | octave && \
 printf "pkg install optim-1.5.2.tar.gz" | octave && \
 rm statistics-1.3.0.tar.gz struct-1.0.14.tar.gz optim-1.5.2.tar.gz io-2.4.7.tar.gz

RUN bash -c "${APT_INSTALL} lua5.2 && ${APT_CLEANUP}"

# Swift begins see https://github.com/swiftdocker/docker-swift/blob/master/Dockerfile
# Install related packages and set LLVM 3.6 as the compiler
RUN bash -c \
 "${APT_INSTALL} make libc6-dev clang-3.6 curl libedit-dev python2.7 python2.7-dev libicu-dev rsync libxml2 git libcurl4-openssl-dev && ${APT_CLEANUP}" \
 && update-alternatives --quiet --install /usr/bin/clang clang /usr/bin/clang-3.6 100 \
 && update-alternatives --quiet --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.6 100

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
RUN chmod -R 755 /usr/lib/swift
# Swift ends

# R packages

RUN bash -c "${APT_INSTALL} gcc g++ liblapack-dev liblapack3 libopenblas-base libopenblas-dev gfortran && ${APT_CLEANUP}"

RUN echo 'options(repos=structure(c(CRAN="https://cloud.r-project.org/")))' > ~/.Rprofile

RUN bash -c "${APT_INSTALL} cmake" && \
    git clone git://github.com/stevengj/nlopt && \
    cd nlopt && cmake -DCMAKE_CXX_FLAGS=-std=c++11 . && make && make install && cd .. && rm -r nlopt && apt-get remove -y cmake && ${APT_CLEANUP}

RUN ldconfig
RUN R -e 'install.packages(c("mvtnorm", "fICA", "JADE", "clue", "Rcpp", "RcppArmadillo", "psych", "GPArotation", "candisc", "car", "heplots", "vegan", "lavaan"))'

RUN bash -c "${APT_INSTALL} python3-tk && ${APT_CLEANUP}"
RUN pip3 install sympy

# yaml
RUN pip3 install pyaml

# snap
RUN bash -c "${APT_INSTALL} snapd"

# Kotlin
# RUN snap install --classic kotlin

#RUN curl -s https://get.sdkman.io | bash
#RUN chmod 777 "/root/.sdkman/bin/sdkman-init.sh"
#RUN bash -c  "source /root/.sdkman/bin/sdkman-init.sh && sdk install kotlin"
#RUN bash -c  "mv /root/.sdkman/candidates/kotlin/1.2.30/bin /bin/kotlin"
#RUN chmod -R 755 /root/.sdkman/
#RUN chmod 755 /bin/kotlin
#ENV PATH=/bin/kotlin:$PATH

ENV         KOTLIN_VERSION=1.2.30 \
            KOTLIN_HOME=/usr/local/kotlin

RUN         cd  /tmp && \
            wget -k "https://github.com/JetBrains/kotlin/releases/download/v${KOTLIN_VERSION}/kotlin-compiler-${KOTLIN_VERSION}.zip"  && \
            unzip "kotlin-compiler-${KOTLIN_VERSION}.zip" && \
            mkdir -p "${KOTLIN_HOME}" && \
            mv "/tmp/kotlinc/bin" "/tmp/kotlinc/lib" "${KOTLIN_HOME}" && \
            rm ${KOTLIN_HOME}/bin/*.bat && \
            chmod +x ${KOTLIN_HOME}/bin/* && \
            ln -s "${KOTLIN_HOME}/bin/"* "/usr/bin/" && \
            rm -rf /tmp/* /var/cache/apk/*


RUN useradd -m agent
ENV HOME /home/agent
RUN mkdir -p /service
RUN chown -R agent /service

# RUN groupadd docker
#RUN gpasswd -a agent docker
#RUN usermod -a -G sudo agent

# For testing
ENV MYPASSWORD password
#RUN echo root:kissa | chpasswd
#RUN echo agent:kissa | chpasswd

# Prevent root login and remove password if there is one.
# RUN usermod -p '!' root
# RUN passwd -l root

#RUN chmod 777 /etc/ssh
#RUN chmod 777 /etc/ssh/*

# Comment the following for production.
##RUN usermod -a -G sudo agent

# Run the container as agent by default.
USER agent

EXPOSE 5000
