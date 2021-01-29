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

RUN add-apt-repository ppa:deadsnakes/ppa && apt-get update

RUN bash -c "${APT_INSTALL} python3.8 wget dirmngr gpg-agent curl python3-distutils && ${APT_CLEANUP}"
RUN curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py && python3.8 get-pip.py && rm get-pip.py
RUN python3.8 -m pip install Flask bleach && bash -c "${APT_CLEANUP}"

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
postgresql-client \
r-base \
sbcl \
sqlite3 \
valgrind \
xvfb \
&& ${APT_CLEANUP}"

# Scala
RUN bash -c "${APT_INSTALL} openjdk-11-jre-headless && ${APT_CLEANUP}"
ENV SCALA_VERSION 2.13.1
RUN wget -q https://downloads.lightbend.com/scala/${SCALA_VERSION}/scala-${SCALA_VERSION}.deb && \
 dpkg -i scala-${SCALA_VERSION}.deb && \
 rm scala-${SCALA_VERSION}.deb

# Docker client binary
ENV DOCKER_VERSION 19.03.8
RUN wget -q https://download.docker.com/linux/static/stable/x86_64/docker-${DOCKER_VERSION}.tgz && \
    tar -xzvf docker-${DOCKER_VERSION}.tgz --strip=1 -C /usr/bin docker/docker && \
    rm docker-${DOCKER_VERSION}.tgz && docker --version

# Python packages

# http://www.scipy.org/install.html
RUN bash -c "${APT_INSTALL} epstool transfig && ${APT_CLEANUP}"

RUN python3.8 -m pip install numpy scipy matplotlib pandas statsmodels control && bash -c "${APT_CLEANUP}"

# Octave
RUN bash -c "${APT_INSTALL} octave octave-control octave-image octave-signal octave-symbolic gnuplot pstoedit liboctave-dev && ${APT_CLEANUP}"
RUN bash -c "${APT_INSTALL} make && ${APT_CLEANUP}"
RUN printf "pkg install -forge io" | octave && \
 printf "pkg install -forge statistics" | octave && \
 printf "pkg install -forge struct" | octave && \
 printf "pkg install -forge optim" | octave

# Swift requirements
RUN bash -c "${APT_INSTALL} libatomic1 libcurl4 curl libxml2 tzdata libpython2.7 && ${APT_CLEANUP}"

# from https://github.com/apple/swift-docker/blob/master/5.2/ubuntu/18.04/slim/Dockerfile
ARG SWIFT_SIGNING_KEY=A62AE125BBBFBB96A6E042EC925CC1CCED3D1561
ARG SWIFT_PLATFORM=ubuntu18.04
ARG SWIFT_BRANCH=swift-5.2-release
ARG SWIFT_VERSION=swift-5.2-RELEASE
ARG SWIFT_WEBROOT=https://swift.org/builds/

ENV SWIFT_SIGNING_KEY=$SWIFT_SIGNING_KEY \
    SWIFT_PLATFORM=$SWIFT_PLATFORM \
    SWIFT_BRANCH=$SWIFT_BRANCH \
    SWIFT_VERSION=$SWIFT_VERSION \
    SWIFT_WEBROOT=$SWIFT_WEBROOT

RUN set -e; \
    SWIFT_WEBDIR="$SWIFT_WEBROOT/$SWIFT_BRANCH/$(echo $SWIFT_PLATFORM | tr -d .)/" \
    && SWIFT_BIN_URL="$SWIFT_WEBDIR/$SWIFT_VERSION/$SWIFT_VERSION-$SWIFT_PLATFORM.tar.gz" \
    && SWIFT_SIG_URL="$SWIFT_BIN_URL.sig" \
    # - Grab gpg here so we cache better up above
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -q update && apt-get -q install -y gnupg && rm -rf /var/lib/apt/lists/* \
    # - Download the GPG keys, Swift toolchain, and toolchain signature, and verify.
    && export GNUPGHOME="$(mktemp -d)" \
    && curl -fsSL "$SWIFT_BIN_URL" -o swift.tar.gz "$SWIFT_SIG_URL" -o swift.tar.gz.sig \
    && gpg --batch --quiet --keyserver ha.pool.sks-keyservers.net --recv-keys "$SWIFT_SIGNING_KEY" \
    && gpg --batch --verify swift.tar.gz.sig swift.tar.gz \
    # - Unpack the toolchain, set libs permissions, and clean up.
    && tar -xzf swift.tar.gz --directory / --strip-components=1 \
    && chmod -R o+r /usr/lib/swift \
    && rm -rf "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz \
    && apt-get purge --auto-remove -y gnupg

RUN swift --version
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

RUN bash -c "${APT_INSTALL} bsdmainutils iputils-ping && ${APT_CLEANUP}"

RUN python3.8 -m pip install sympy pyaml html5lib opencv-contrib-python-headless && bash -c "${APT_CLEANUP}"

# Kotlin
ENV         KOTLIN_VERSION=1.3.70 \
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
RUN wget https://download2.gluonhq.com/openjfx/15.0.1/openjfx-15.0.1_linux-x64_bin-sdk.zip && \
    unzip openjfx-15.0.1_linux-x64_bin-sdk.zip && \
    rm openjfx-15.0.1_linux-x64_bin-sdk.zip

# Rust (nightly)
ENV CARGO_HOME=/cargo
ENV RUSTUP_HOME=/rustup
RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain nightly -y && rm -r /rustup/toolchains/nightly-x86_64-unknown-linux-gnu/share/doc
RUN /cargo/bin/cargo install svgbob_cli
RUN ln -s "/cargo/bin/"* "/usr/bin/"

# Free Pascal
RUN bash -c "${APT_INSTALL} fpc && ${APT_CLEANUP}"

# Go
RUN wget -q https://dl.google.com/go/go1.14.1.linux-amd64.tar.gz && tar -C /usr/local -xzf go1.14.1.linux-amd64.tar.gz && rm go1.14.1.linux-amd64.tar.gz
ENV PATH=$PATH:/usr/local/go/bin

RUN python3.8 -m pip install \
beautifulsoup4 \
filelock \
gunicorn \
humanize \
isodate \
marshmallow \
marshmallow-enum \
marshmallow_union \
python-dateutil \
typing-inspect \
webargs \
 && bash -c "${APT_CLEANUP}"
RUN python3.8 -m pip install requests --upgrade && bash -c "${APT_CLEANUP}"

# git
RUN bash -c "${APT_INSTALL} openssh-client git libnunit-framework2.6.3-cil && ${APT_CLEANUP}"

RUN wget -q https://github.com/AdoptOpenJDK/openjdk15-binaries/releases/download/jdk-15.0.2%2B7/OpenJDK15U-jdk_x64_linux_hotspot_15.0.2_7.tar.gz && \
 tar -xf OpenJDK15U-jdk_x64_linux_hotspot_15.0.2_7.tar.gz && \
 rm OpenJDK15U-jdk_x64_linux_hotspot_15.0.2_7.tar.gz
ENV PATH=$PWD/jdk-15.0.2+7/bin:$PATH
RUN java -version
# RUN bash -c "${APT_INSTALL} uuid-runtime && ${APT_CLEANUP}"
# RUN uuidgen > /etc/machine-id
# RUN dbus-uuidgen > /etc/machine-id
# run echo 3c99bef9df635abd860662e8594b78af >/etc/machine-id

RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.8 20

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
