# CSPlugin image contains various compilers and tools that can be run by TIM users in a sandboxed environment
# The image is comprised of three stages
# - base: The minimal setup to run the image. Contains only JDK, Python and Mono runtimes. Used in CI.
# - complete: base + various additional languages and libraries. Used in production.
# - sudo: complete + sudo access. Used in development.
#
# To build all three stages via TIM scripts, run the following commands in TIM root:
# ./tim dev build csplugin
#
# (add --push arg to also push the images automatically to Docker Hub)


FROM ubuntu:20.04 as base
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
RUN bash -c "${APT_INSTALL} python3.10 wget dirmngr gpg-agent curl python3.10-distutils && ${APT_CLEANUP}"
RUN curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py && python3.10 get-pip.py && rm get-pip.py
RUN python3.10 -m pip install Flask bleach lxml && bash -c "${APT_CLEANUP}"

# Keep above same as SVN Dockerfile for better caching

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF && bash -c "${APT_CLEANUP}"
RUN echo "deb https://download.mono-project.com/repo/ubuntu stable-focal main" > /etc/apt/sources.list.d/mono-official-stable.list
RUN bash -c "${APT_INSTALL} \
ca-certificates \
doxygen \
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
x11-utils \
libglfw3 \
libglfw3-dev \
&& ${APT_CLEANUP}"

# Copy any certificates that are needed
# Right some servers doe not share the certificate correctly,
# so we need to manually add it until references are migrated.
# See https://gitlab.com/tim-jyu/tim/-/issues/2590
COPY cert_chains/*.crt /usr/local/share/ca-certificates/
RUN update-ca-certificates
RUN ln -s /etc/ssl/certs/ca-certificates.crt /usr/lib/ssl/cert.pem && \
    chmod 775 /usr/lib/ssl/cert.pem && \
    chmod 775 /etc/ssl/certs/ca-certificates.crt

# We use Mesa drivers by default for GUI rendering
# For this, we specifically force software rendering mode and use the fast "llvmpipe" driver
# Note that llvmpipe is not "pixel stable", i.e. images can vary very slightly between runs
# Pixel-perfect mode can be achieved with "softpipe" driver
# Additionally, we disable shader cache because of read-only permissions in this container
ENV XVFB_WHD="1280x1024x24"\
    DISPLAY=":99" \
    LIBGL_ALWAYS_SOFTWARE="1" \
    GALLIUM_DRIVER="llvmpipe" \
    MESA_GLSL_CACHE_DISABLE="true"

# Docker client binary
ENV DOCKER_VERSION 20.10.3
RUN wget -q https://download.docker.com/linux/static/stable/x86_64/docker-${DOCKER_VERSION}.tgz && \
    tar -xzvf docker-${DOCKER_VERSION}.tgz --strip=1 -C /usr/bin docker/docker && \
    rm docker-${DOCKER_VERSION}.tgz && docker --version

RUN python3.10 -m pip install \
beautifulsoup4 \
filelock \
gunicorn \
html5lib \
humanize \
isodate \
marshmallow \
marshmallow-enum \
marshmallow_union \
pyaml \
python-dateutil \
typing-inspect \
webargs \
 && bash -c "${APT_CLEANUP}"
RUN python3.10 -m pip install requests urllib3 --upgrade && bash -c "${APT_CLEANUP}"

# git
RUN bash -c "${APT_INSTALL} openssh-client git libnunit-framework2.6.3-cil && ${APT_CLEANUP}"

# JDK
RUN wget -q https://download.java.net/java/GA/jdk17.0.2/dfd4a8d0985749f896bed50d7138ee7f/8/GPL/openjdk-17.0.2_linux-x64_bin.tar.gz && \
 tar -xf openjdk-17.0.2_linux-x64_bin.tar.gz && \
 rm openjdk-17.0.2_linux-x64_bin.tar.gz
ENV PATH=$PWD/jdk-17.0.2/bin:$PATH
RUN java -version
# RUN bash -c "${APT_INSTALL} uuid-runtime && ${APT_CLEANUP}"
# RUN uuidgen > /etc/machine-id
# RUN dbus-uuidgen > /etc/machine-id
# run echo 3c99bef9df635abd860662e8594b78af >/etc/machine-id

# JavaFX for OpenJDK
ENV OPENJFX_VERSION=17
RUN wget https://download2.gluonhq.com/openjfx/${OPENJFX_VERSION}/openjfx-${OPENJFX_VERSION}_linux-x64_bin-sdk.zip && \
    unzip openjfx-${OPENJFX_VERSION}_linux-x64_bin-sdk.zip && \
    rm openjfx-${OPENJFX_VERSION}_linux-x64_bin-sdk.zip

# dotnet
# Based on https://github.com/dotnet/dotnet-docker/blob/main/documentation/scenarios/installing-dotnet.md#installing-from-dotnet-install-script
ENV \
    # Do not generate certificate
    DOTNET_GENERATE_ASPNET_CERTIFICATE=false \
    # Version channel from which to install dotnet
    DOTNET_VERSION_CHANNEL=6.0 \
    # Enable correct mode for dotnet watch (only mode supported in a container)
    DOTNET_USE_POLLING_FILE_WATCHER=true \
    # Skip extraction of XML docs - generally not useful within an image/container - helps performance
    NUGET_XMLDOC_MODE=skip

RUN bash -c "${APT_INSTALL} \
    libc6 \
    libgcc1 \
    libgssapi-krb5-2 \
    libicu66 \
    libssl1.1 \
    libstdc++6 \
    zlib1g \
&& ${APT_CLEANUP}"

# Install .NET SDK and check that it works
RUN curl -sSL https://dot.net/v1/dotnet-install.sh | bash /dev/stdin --channel $DOTNET_VERSION_CHANNEL --install-dir /usr/share/dotnet \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet

# Set active Python version to 3.10
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.10 20

RUN useradd -m agent
ENV HOME /home/agent
RUN mkdir -p /service
RUN chown -R agent /service

RUN mkdir $HOME/.nuget && chown -R agent $HOME/.nuget
RUN mkdir $HOME/.local && chown -R agent $HOME/.local
# Add .local cache and NuGet package store as volumes to allow dotnet commands to restore NuGet packages
VOLUME $HOME/.local
# Add a special global package cache that will contain packages used for building directly with `csc`
VOLUME $HOME/.nuget/packages

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

# Trigger first-time scripts and folder creation
RUN dotnet help && \
    # Generate dotnet config folder
    dotnet new --list && \
    # Generate default NuGet config
    dotnet nuget list source && \
    # Clean up NuGet locks
    rm -rf /tmp/NuGetScratch

EXPOSE 5000

FROM base as complete

# Temporarily return to root to install packages
USER root

# Python packages

# OpenCV
RUN python3.10 -m pip install sympy opencv-contrib-python-headless && bash -c "${APT_CLEANUP}"

# http://www.scipy.org/install.html
RUN bash -c "${APT_INSTALL} epstool transfig && ${APT_CLEANUP}"
RUN python3.10 -m pip install numpy scipy matplotlib pandas statsmodels control scikit-learn networkx && bash -c "${APT_CLEANUP}"

# pyod
RUN python3.10 -m pip install pyod && bash -c "${APT_CLEANUP}"

# pyqula
RUN python3.10 -m pip install pyqula && bash -c "${APT_CLEANUP}"

# sktime
RUN bash -c "${APT_INSTALL} python3.10-dev build-essential && \
    python3.10 -m pip install sktime[all_extras] && \
    apt-get purge --auto-remove -y python3.10-dev build-essential && \
    ${APT_CLEANUP}"

# R packages
RUN bash -c "${APT_INSTALL} gcc g++ liblapack-dev liblapack3 libopenblas-base libopenblas-dev gfortran && ${APT_CLEANUP}"

# Scala
RUN bash -c "${APT_INSTALL} openjdk-16-jre-headless && ${APT_CLEANUP}"
ENV SCALA_VERSION 2.13.6
RUN wget -q https://downloads.lightbend.com/scala/${SCALA_VERSION}/scala-${SCALA_VERSION}.deb && \
 dpkg -i scala-${SCALA_VERSION}.deb && \
 rm scala-${SCALA_VERSION}.deb

# Octave
RUN bash -c "${APT_INSTALL} octave octave-control octave-image octave-signal octave-symbolic gnuplot pstoedit liboctave-dev fonts-freefont-otf && ${APT_CLEANUP}"
RUN bash -c "${APT_INSTALL} make && ${APT_CLEANUP}"
RUN printf "pkg install -forge io" | octave && \
 echo -n 'pkg install "https://downloads.sourceforge.net/project/octave/Octave%20Forge%20Packages/Individual%20Package%20Releases/statistics-1.4.3.tar.gz"' | octave && \
 printf "pkg install -forge struct" | octave && \
 printf "pkg install -forge optim" | octave

# Octave and gnuplot use Qt to generate plots and relevant UI (not used by us but it's a hard dependency)
# Latest gnuplot fails to run on kernel versions below 3.15 (see https://github.com/TIM-JYU/TIM/issues/2349)
# This adopts the following fix:
# https://github.com/dnschneid/crouton/wiki/Fix-error-while-loading-shared-libraries:-libQt5Core.so.5
RUN strip --remove-section=.note.ABI-tag /lib/x86_64-linux-gnu/libQt5Core.so.5

# Swift requirements
RUN bash -c "${APT_INSTALL} libcurl4 libpython3.8 libxml2 && ${APT_CLEANUP}"

# modified from https://github.com/apple/swift-docker/blob/main/5.5/ubuntu/20.04/slim/Dockerfile
ARG SWIFT_SIGNING_KEY=A62AE125BBBFBB96A6E042EC925CC1CCED3D1561
ARG SWIFT_PLATFORM=ubuntu20.04
ARG SWIFT_BRANCH=swift-5.5-release
ARG SWIFT_VERSION=swift-5.5-RELEASE
ARG SWIFT_WEBROOT=https://download.swift.org

ENV SWIFT_SIGNING_KEY=$SWIFT_SIGNING_KEY \
    SWIFT_PLATFORM=$SWIFT_PLATFORM \
    SWIFT_BRANCH=$SWIFT_BRANCH \
    SWIFT_VERSION=$SWIFT_VERSION \
    SWIFT_WEBROOT=$SWIFT_WEBROOT

RUN set -e; \
    SWIFT_WEBDIR="$SWIFT_WEBROOT/$SWIFT_BRANCH/$(echo $SWIFT_PLATFORM | tr -d .)" \
    && SWIFT_BIN_URL="$SWIFT_WEBDIR/$SWIFT_VERSION/$SWIFT_VERSION-$SWIFT_PLATFORM.tar.gz" \
    && SWIFT_SIG_URL="$SWIFT_BIN_URL.sig" \
    # - Grab gpg here so we cache better up above
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -q update && apt-get -q install -y gnupg && rm -rf /var/lib/apt/lists/* \
    # - Download the GPG keys, Swift toolchain, and toolchain signature, and verify.
    && export GNUPGHOME="$(mktemp -d)" \
    && curl -fsSL "$SWIFT_BIN_URL" -o swift.tar.gz "$SWIFT_SIG_URL" -o swift.tar.gz.sig \
    && gpg --batch --quiet --keyserver keyserver.ubuntu.com --recv-keys "$SWIFT_SIGNING_KEY" \
    && gpg --batch --verify swift.tar.gz.sig swift.tar.gz \
    # - Unpack the toolchain, set libs permissions, and clean up.
    && tar -xzf swift.tar.gz --directory / --strip-components=1 \
    && chmod -R o+r /usr/lib/swift \
    && rm -rf "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz \
    && apt-get purge --auto-remove -y gnupg

RUN swift --version
# Swift ends

RUN echo 'options(repos=structure(c(CRAN="https://cloud.r-project.org/")))' > ~/.Rprofile

# TODO: Newer versions don't compile yet (seems to be missing some deps)
RUN bash -c "${APT_INSTALL} cmake" && \
    git clone https://github.com/stevengj/nlopt.git && \
    cd nlopt && \
    git checkout b195a672e006f6d9c48bb322f6e8e1d212e4df99 && \
    cmake -DCMAKE_CXX_FLAGS=-std=c++11 . && make && make install && cd .. && rm -r nlopt && apt-get remove -y cmake && ${APT_CLEANUP}

RUN ldconfig

RUN bash -c "${APT_INSTALL} libcurl4-openssl-dev && ${APT_CLEANUP}"
RUN R -e 'install.packages(c("mvtnorm", "fICA", "JADE", "clue", "Rcpp", "RcppArmadillo", "psych", "GPArotation", "candisc", "car", "heplots", "vegan", "lavaan", "ggplot2"))'

RUN bash -c "${APT_INSTALL} bsdmainutils iputils-ping && ${APT_CLEANUP}"

# Kotlin
ENV         KOTLIN_VERSION=1.5.31 \
            KOTLIN_HOME=/usr/local/kotlin

RUN         cd  /tmp && \
            wget -k "https://github.com/JetBrains/kotlin/releases/download/v${KOTLIN_VERSION}/kotlin-compiler-${KOTLIN_VERSION}.zip"  && \
            unzip "kotlin-compiler-${KOTLIN_VERSION}.zip" && \
            rm "kotlin-compiler-${KOTLIN_VERSION}.zip" && \
            mkdir -p "${KOTLIN_HOME}" && \
            mv "/tmp/kotlinc/bin" "/tmp/kotlinc/lib" "${KOTLIN_HOME}" && \
            rm ${KOTLIN_HOME}/bin/*.bat && \
            chmod +x ${KOTLIN_HOME}/bin/* && \
            ln -s "${KOTLIN_HOME}/bin/"* "/usr/bin/" && \
            ${APT_CLEANUP}

# Rust (nightly)
ENV CARGO_HOME=/cargo
ENV RUSTUP_HOME=/rustup
RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain nightly --profile=minimal -y && rm -r /rustup/toolchains/nightly-x86_64-unknown-linux-gnu/share/doc
RUN /cargo/bin/cargo install svgbob_cli && rm -rf "$CARGO_HOME/registry"
RUN ln -s "/cargo/bin/"* "/usr/bin/"

# Free Pascal
RUN bash -c "${APT_INSTALL} fpc && ${APT_CLEANUP}"

# Go
ENV GOLANG_VERSION=1.17.6
RUN wget -q https://dl.google.com/go/go${GOLANG_VERSION}.linux-amd64.tar.gz && tar -C /usr/local -xzf go${GOLANG_VERSION}.linux-amd64.tar.gz && rm go${GOLANG_VERSION}.linux-amd64.tar.gz
ENV PATH=$PATH:/usr/local/go/bin

# NodeJS and TypeScript
RUN (curl -sL https://deb.nodesource.com/setup_16.x | bash -) && bash -c "${APT_INSTALL} nodejs && ${APT_CLEANUP}"
RUN npm i npm@latest -g && bash -c "${APT_CLEANUP}"
RUN npm i ts-node -g && bash -c "${APT_CLEANUP}"

# Install NUnit runner for .NET
ENV NUNIT_RUNNER_VERSION=3.14.0
RUN dotnet tool install --tool-path /dotnet_tools/ NUnit.ConsoleRunner.NetCore --version ${NUNIT_RUNNER_VERSION} && \
    # We need to call NUnit directly with dotnet exec in order to properly declare custom references
    ln -s /dotnet_tools/.store/nunit.consolerunner.netcore/${NUNIT_RUNNER_VERSION}/nunit.consolerunner.netcore/${NUNIT_RUNNER_VERSION}/tools/netcoreapp3.1/any/nunit3-console.dll /dotnet_tools/nunit.console.dll

# Install ffmpeg for transcoding videos
RUN bash -c "${APT_INSTALL} ffmpeg && ${APT_CLEANUP}"

# Install mongosh (adapted from https://github.com/rtsp/docker-mongosh/blob/main/Dockerfile)
ENV MONGODB_VERSION=5.0
ENV MONGOSH_VERSION=1.1.9

RUN curl -fsSL "https://www.mongodb.org/static/pgp/server-${MONGODB_VERSION}.asc" | gpg --dearmor -o /etc/apt/trusted.gpg.d/mongodb-org-${MONGODB_VERSION}.gpg && \
    echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/${MONGODB_VERSION} multiverse" > /etc/apt/sources.list.d/mongodb-org-${MONGODB_VERSION}.list && \
    apt-get update

RUN bash -c "${APT_INSTALL} mongodb-mongosh=${MONGOSH_VERSION} && ${APT_CLEANUP}"
RUN mongosh --version

# Install cqlsh from pip
RUN python3.10 -m pip install cqlsh six --upgrade && bash -c "${APT_CLEANUP}"
RUN cqlsh --version

# Install Elixir
RUN bash -c "${APT_INSTALL} gnupg" && \
    wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb && \
    dpkg -i erlang-solutions_2.0_all.deb && \
    rm erlang-solutions_2.0_all.deb && \
    apt-get update && \
    bash -c "${APT_INSTALL} esl-erlang elixir && apt-get purge --auto-remove -y gnupg && ${APT_CLEANUP}" && \
    elixir --version

RUN python3.10 -m pip install coverage && bash -c "${APT_CLEANUP}"

USER agent

FROM complete as sudo

USER root
ENV LANG en_US.UTF-8
ENV CLASSPATH .:/cs/java/junit.jar:/cs/java/hamcrest-core.jar:/cs/java/comtest.jar:/cs/java/Ali.jar:/cs/java/Graphics.jar:/cs/java/fxgui.jar:/cs/java/gui.jar

CMD /bin/bash -c "su agent"