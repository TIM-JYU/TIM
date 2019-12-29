FROM ubuntu:18.04
LABEL maintainer="vesal@jyu.fi"

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -rf /var/lib/apt/lists /usr/share/doc ~/.cache /var/cache/oracle-* /var/cache/apk /tmp/*"

# Timezone configuration
RUN bash -c "${APT_INSTALL} locales tzdata apt-utils acl unzip software-properties-common && ${APT_CLEANUP}"
RUN locale-gen en_US.UTF-8 && bash -c "${APT_CLEANUP}"
RUN locale-gen fi_FI.UTF-8 && bash -c "${APT_CLEANUP}"
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en \
    LC_ALL=en_US.UTF-8 \
    LC_CTYPE=en_US.UTF-8
RUN echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata && bash -c "${APT_CLEANUP}"

RUN bash -c "${APT_INSTALL} python3 python3-pip python3-setuptools wget dirmngr gpg-agent && ${APT_CLEANUP}"
RUN pip3 install wheel && bash -c "${APT_CLEANUP}"
RUN pip3 install Flask bleach && bash -c "${APT_CLEANUP}"

# Keep above same as SVN Dockerfile for better caching

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF && bash -c "${APT_CLEANUP}"
RUN echo "deb http://download.mono-project.com/repo/ubuntu bionic main" > /etc/apt/sources.list.d/mono-official.list
RUN bash -c "${APT_INSTALL} \
doxygen \
firejail \
fsharp \
graphviz \
imagemagick \
inotify-tools \
libsdl-mixer1.2 \
libsdl1.2debian \
libsqlite3-dev \
libxtst6 \
lua5.2 \
mono-complete \
ncdu \
nunit-console \
openjdk-11-jdk \
postgresql-client \
r-base \
sbcl \
sqlite3 \
valgrind \
xvfb \
&& ${APT_CLEANUP}"

# Scala
ENV SCALA_VERSION 2.12.7
RUN wget -q http://downloads.typesafe.com/scala/${SCALA_VERSION}/scala-${SCALA_VERSION}.deb && \
 dpkg -i scala-${SCALA_VERSION}.deb && \
 rm scala-${SCALA_VERSION}.deb

# Docker client binary
ENV DOCKER_VERSION 18.06.1
RUN wget -q https://download.docker.com/linux/static/stable/x86_64/docker-${DOCKER_VERSION}-ce.tgz && \
    tar -xzvf docker-${DOCKER_VERSION}-ce.tgz --strip=1 -C /usr/bin docker/docker && \
    rm docker-${DOCKER_VERSION}-ce.tgz && docker --version

# Python packages

# http://www.scipy.org/install.html
RUN bash -c "${APT_INSTALL} python3-numpy python3-scipy python3-matplotlib python3-pandas epstool transfig && ${APT_CLEANUP}"

RUN pip3 install statsmodels control && bash -c "${APT_CLEANUP}"

# Octave
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

# Swift requirements
RUN bash -c "${APT_INSTALL} clang libicu-dev curl && ${APT_CLEANUP}"

# Everything up to here should cache nicely between Swift versions, assuming dev dependencies change little
ENV SWIFT_BRANCH=swift-4.2-release \
    SWIFT_VERSION=swift-4.2-RELEASE \
    SWIFT_PLATFORM=ubuntu18.04 \
    PATH=/usr/bin:$PATH
ENV SWIFT_URL=https://swift.org/builds/swift-4.2.1-release/ubuntu1804/swift-4.2.1-RELEASE/swift-4.2.1-RELEASE-ubuntu18.04.tar.gz
# Download GPG keys, signature and Swift package, then unpack and cleanup
# RUN export SWIFT_URL=https://swift.org/builds/$SWIFT_BRANCH/$(echo "$SWIFT_PLATFORM" | tr -d .)/$SWIFT_VERSION/$SWIFT_VERSION-$SWIFT_PLATFORM.tar.gz
RUN curl -fSsL $SWIFT_URL -o swift.tar.gz
#    && curl -fSsL $SWIFT_URL.sig -o swift.tar.gz.sig \
RUN export GNUPGHOME="$(mktemp -d)"
#    && gpg --keyserver ha.pool.sks-keyservers.net \
#      --recv-keys \
#      '7463 A81A 4B2E EA1B 551F  FBCF D441 C977 412B 37AD' \
#      '1BE1 E29A 084C B305 F397  D62A 9F59 7F4D 21A5 6D5F' \
#      'A3BA FD35 56A5 9079 C068  94BD 63BC 1CFE 91D3 06C6' \
#      '5E4D F843 FB06 5D7F 7E24  FBA2 EF54 30F0 71E1 B235' \
#      '8513 444E 2DA3 6B7C 1659  AF4D 7638 F1FB 2B2B 08C4' \
#    && gpg --batch --verify --quiet swift.tar.gz.sig swift.tar.gz \
RUN  tar -xzf swift.tar.gz --directory / --strip-components=1
#    && pkill -9 gpg-agent \
#    && pkill -9 dirmngr \
# RUN rm -r "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz
RUN chmod -R 755 /usr/lib/swift
# Swift ends

# R packages
RUN bash -c "${APT_INSTALL} gcc g++ liblapack-dev liblapack3 libopenblas-base libopenblas-dev gfortran && ${APT_CLEANUP}"

RUN echo 'options(repos=structure(c(CRAN="https://cloud.r-project.org/")))' > ~/.Rprofile

RUN bash -c "${APT_INSTALL} cmake git" && \
    git clone git://github.com/stevengj/nlopt && \
    cd nlopt && cmake -DCMAKE_CXX_FLAGS=-std=c++11 . && make && make install && cd .. && rm -r nlopt && apt-get remove -y cmake git && ${APT_CLEANUP}

RUN ldconfig

RUN bash -c "${APT_INSTALL} libcurl4-openssl-dev && ${APT_CLEANUP}"
RUN R -e 'install.packages(c("mvtnorm", "fICA", "JADE", "clue", "Rcpp", "RcppArmadillo", "psych", "GPArotation", "candisc", "car", "heplots", "vegan", "lavaan", "ggplot2"))'

RUN bash -c "${APT_INSTALL} python3-tk bsdmainutils iputils-ping && ${APT_CLEANUP}"

RUN pip3 install sympy pyaml html5lib && bash -c "${APT_CLEANUP}"

# Kotlin
ENV         KOTLIN_VERSION=1.2.71 \
            KOTLIN_HOME=/usr/local/kotlin

RUN         cd  /tmp && \
            wget -k "https://github.com/JetBrains/kotlin/releases/download/v${KOTLIN_VERSION}/kotlin-compiler-${KOTLIN_VERSION}.zip"  && \
            unzip "kotlin-compiler-${KOTLIN_VERSION}.zip" && \
            mkdir -p "${KOTLIN_HOME}" && \
            mv "/tmp/kotlinc/bin" "/tmp/kotlinc/lib" "${KOTLIN_HOME}" && \
            rm ${KOTLIN_HOME}/bin/*.bat && \
            chmod +x ${KOTLIN_HOME}/bin/* && \
            ln -s "${KOTLIN_HOME}/bin/"* "/usr/bin/" && \
            ${APT_CLEANUP}

# JavaFX for OpenJDK
RUN wget https://download2.gluonhq.com/openjfx/11.0.1/openjfx-11.0.1_linux-x64_bin-sdk.zip && \
    unzip openjfx-11.0.1_linux-x64_bin-sdk.zip && \
    rm openjfx-11.0.1_linux-x64_bin-sdk.zip

# Rust (nightly)
ENV CARGO_HOME=/cargo
ENV RUSTUP_HOME=/rustup
RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain nightly -y && rm -r /rustup/toolchains/nightly-x86_64-unknown-linux-gnu/share/doc
RUN /cargo/bin/cargo install svgbob_cli
RUN ln -s "/cargo/bin/"* "/usr/bin/"

# Free Pascal
RUN bash -c "${APT_INSTALL} fpc && ${APT_CLEANUP}"

# Go
RUN wget -q https://dl.google.com/go/go1.11.1.linux-amd64.tar.gz && tar -C /usr/local -xzf go1.11.1.linux-amd64.tar.gz && rm go1.11.1.linux-amd64.tar.gz
ENV PATH=$PATH:/usr/local/go/bin

RUN pip3 install requests --upgrade && bash -c "${APT_CLEANUP}"

# git
RUN bash -c "${APT_INSTALL} git && ${APT_CLEANUP}"

# RUN bash -c "${APT_INSTALL} uuid-runtime && ${APT_CLEANUP}"
# RUN uuidgen > /etc/machine-id
# RUN dbus-uuidgen > /etc/machine-id
# run echo 3c99bef9df635abd860662e8594b78af >/etc/machine-id

RUN useradd -m agent
ENV HOME /home/agent
RUN mkdir -p /service
RUN chown -R agent /service

# Kokeile seuraavaa, saisiko sillä pois matplotlib:in cache varoituksen
# RUN python -c "import matplotlib.pyplot"

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
