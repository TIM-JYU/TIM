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


FROM ubuntu:24.04 AS base
LABEL org.opencontainers.image.authors="tim@jyu.fi"
LABEL org.opencontainers.image.source="https://github.com/TIM-JYU/TIM"

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

# Install gpg and gpg-agent for verifying signatures before installing python
RUN bash -c "${APT_INSTALL} gpg-agent && ${APT_CLEANUP}"

ENV PY_VERSION=3.11
ENV PYTHON=python$PY_VERSION

# Install Python and pip
# Also fix potential ModuleNotFoundError: No module named 'apt_pkg' error caused by apt_pkg
# being targeted to some other Python version
RUN bash -c "add-apt-repository ppa:deadsnakes/ppa && apt-get update && \
    ${APT_INSTALL} ${PYTHON} wget dirmngr gpg-agent curl ${PYTHON}-distutils ${PYTHON}-dev && \
    cd /usr/lib/python3/dist-packages/ && cp apt_pkg*.so apt_pkg.so && \
    wget -q https://bootstrap.pypa.io/get-pip.py -O get-pip.py && ${PYTHON} get-pip.py && rm get-pip.py && \
    ${APT_CLEANUP}"
RUN ${PYTHON} -m pip install --ignore-installed Flask bleach lxml==4.9.4 && bash -c "${APT_CLEANUP}"

RUN bash -c "gpg --homedir /tmp --no-default-keyring --keyring /usr/share/keyrings/mono-official-archive-keyring.gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
    && echo 'deb [signed-by=/usr/share/keyrings/mono-official-archive-keyring.gpg] https://download.mono-project.com/repo/ubuntu stable-focal main' | tee /etc/apt/sources.list.d/mono-official-stable.list \
    && ${APT_CLEANUP}"
RUN bash -c "${APT_INSTALL} \
build-essential \
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
x11-xkb-utils \
xkb-data \
libglfw3 \
libglfw3-dev \
&& ${APT_CLEANUP}"

# 2025-01-03: X11 shipped with Ubuntu 24.04 seems to be not synced with
# xkb-data package. This causes the following messages being logged:
#     Could not resolve keysym XF86CameraAccessEnable (and for other XF86 keys)
# The error stems from /usr/share/X11/xkb/symbols/inet containing symbols that
# xkb does not support. To suppress the message, we empty the inet symbols
# for the default keyboard layout (evdev).
# We do not use the keyboard anyways since we run headless.
# TODO: This might not be needed in future releases of Ubuntu.
RUN bash -c "echo 'xkb_symbols \"evdev\" {};' > /usr/share/X11/xkb/symbols/inet"

# Copy any certificates that are needed
# Right some servers doe not share the certificate correctly,
# so we need to manually add it until references are migrated.
# See https://gitlab.com/tim-jyu/tim/-/issues/2590
COPY cert_chains/*.crt /usr/local/share/ca-certificates/
RUN update-ca-certificates

# We use Mesa drivers by default for GUI rendering
# For this, we specifically force software rendering mode and use the fast "llvmpipe" driver
# Note that llvmpipe is not "pixel stable", i.e. images can vary very slightly between runs
# Pixel-perfect mode can be achieved with "softpipe" driver
# Additionally, we disable shader cache because of read-only permissions in this container
ENV XVFB_WHD="1280x1024x24"\
    DISPLAY=":99" \
    LIBGL_ALWAYS_SOFTWARE="1" \
    GALLIUM_DRIVER="llvmpipe" \
    MESA_SHADER_CACHE_DISABLE="true"

# Docker client binary
ENV DOCKER_VERSION 27.4.1
RUN wget -q https://download.docker.com/linux/static/stable/x86_64/docker-${DOCKER_VERSION}.tgz && \
    tar -xzvf docker-${DOCKER_VERSION}.tgz --strip=1 -C /usr/bin docker/docker && \
    rm docker-${DOCKER_VERSION}.tgz && docker --version

RUN $PYTHON -m pip install \
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
&& \
$PYTHON -m pip install requests urllib3 --upgrade && \
bash -c "${APT_CLEANUP}"

# git
RUN bash -c "${APT_INSTALL} openssh-client git libnunit-framework2.6.3-cil && ${APT_CLEANUP}"

# JDK
ENV JDK_VERSION=23.0.1
# Grab this path from download links of https://jdk.java.net/
ENV OPENJDK_DL_PATH=c28985cbf10d4e648e4004050f8781aa/11
RUN wget -q https://download.java.net/java/GA/jdk${JDK_VERSION}/${OPENJDK_DL_PATH}/GPL/openjdk-${JDK_VERSION}_linux-x64_bin.tar.gz && \
 tar -xf openjdk-${JDK_VERSION}_linux-x64_bin.tar.gz && \
 rm openjdk-${JDK_VERSION}_linux-x64_bin.tar.gz
ENV PATH=$PWD/jdk-${JDK_VERSION}/bin:$PATH
RUN java -version
# RUN bash -c "${APT_INSTALL} uuid-runtime && ${APT_CLEANUP}"
# RUN uuidgen > /etc/machine-id
# RUN dbus-uuidgen > /etc/machine-id
# run echo 3c99bef9df635abd860662e8594b78af >/etc/machine-id

# JavaFX for OpenJDK
ENV OPENJFX_VERSION=23.0.1
RUN wget -q https://download2.gluonhq.com/openjfx/${OPENJFX_VERSION}/openjfx-${OPENJFX_VERSION}_linux-x64_bin-sdk.zip && \
    unzip openjfx-${OPENJFX_VERSION}_linux-x64_bin-sdk.zip && \
    rm openjfx-${OPENJFX_VERSION}_linux-x64_bin-sdk.zip

# Rust (nightly)
ENV CARGO_HOME=/cargo
ENV RUSTUP_HOME=/rustup
RUN bash -c 'wget -qO- https://sh.rustup.rs | sh -s -- --default-toolchain nightly --profile=minimal -y && rm -r /rustup/toolchains/nightly-x86_64-unknown-linux-gnu/share/doc'
RUN /cargo/bin/cargo install svgbob_cli && rm -rf "$CARGO_HOME/registry"
RUN ln -s "/cargo/bin/"* "/usr/bin/"

# dotnet
# Based on https://github.com/dotnet/dotnet-docker/blob/main/documentation/scenarios/installing-dotnet.md#installing-from-dotnet-install-script
ENV \
    # Do not generate certificate
    DOTNET_GENERATE_ASPNET_CERTIFICATE=false \
    # Version channel from which to install dotnet
    DOTNET_VERSION_CHANNEL=8.0 \
    # Enable correct mode for dotnet watch (only mode supported in a container)
    DOTNET_USE_POLLING_FILE_WATCHER=true \
    # Skip extraction of XML docs - generally not useful within an image/container - helps performance
    NUGET_XMLDOC_MODE=skip \
    # Not setting this causes .NET apps to crash when file size limit is set
    # See https://github.com/dotnet/sdk/issues/31457
    DOTNET_EnableWriteXorExecute=0

RUN bash -c "${APT_INSTALL} \
    libc6 \
    libgcc-s1 \
    libicu74 \
    libssl3t64 \
    libstdc++6 \
    tzdata \
    zlib1g \
&& ${APT_CLEANUP}"

# Install .NET SDK and check that it works
RUN bash -c "wget -qO- https://dot.net/v1/dotnet-install.sh | bash /dev/stdin --channel $DOTNET_VERSION_CHANNEL --install-dir /usr/share/dotnet \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet"

# Set active Python version to installed version
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/$PYTHON 20

# Edit OpenSSL config to enable "legacy" providers
# OpenSSLv3 seems to disable RIPEMD160: https://github.com/openssl/openssl/issues/16994
# TODO: Edit csplugin to get rid of RIPEMD160
RUN sed -i 's/\[provider_sect\]/\0\nlegacy = legacy_sect/' /usr/lib/ssl/openssl.cnf && \
    sed -i 's/\[default_sect\]/\0\nactivate = 1\n[legacy_sect]\nactivate = 1/' /usr/lib/ssl/openssl.cnf

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

FROM base AS complete

# Temporarily return to root to install packages
USER root

# Python packages

# OpenCV
RUN $PYTHON -m pip install sympy opencv-contrib-python-headless && bash -c "${APT_CLEANUP}"

# Gurobi
RUN $PYTHON -m pip install gurobipy && bash -c "${APT_CLEANUP}"

# http://www.scipy.org/install.html
RUN bash -c "${APT_INSTALL} epstool transfig && ${APT_CLEANUP}"
RUN $PYTHON -m pip install numpy scipy matplotlib pandas statsmodels control scikit-learn scikit-image pooch networkx && bash -c "${APT_CLEANUP}"
# As an additional step, we need to initialize the default datasets of scikit-image
ENV SKIMAGE_DATADIR=/etc/skimage
RUN $PYTHON -c "import skimage; print(skimage.__version__)"

# pyod
RUN $PYTHON -m pip install pyod && bash -c "${APT_CLEANUP}"

# pyqula
RUN $PYTHON -m pip install pyqula && bash -c "${APT_CLEANUP}"

# sktime
RUN bash -c "$PYTHON -m pip install sktime[all_extras] && ${APT_CLEANUP}"

# R packages
RUN bash -c "${APT_INSTALL} gcc g++ liblapack-dev liblapack3 libopenblas-dev gfortran && ${APT_CLEANUP}"

# Scala
RUN bash -c "${APT_INSTALL} openjdk-21-jre-headless && ${APT_CLEANUP}"
ENV SCALA_VERSION 2.13.15
RUN wget -q https://downloads.lightbend.com/scala/${SCALA_VERSION}/scala-${SCALA_VERSION}.deb && \
 dpkg -i scala-${SCALA_VERSION}.deb && \
 rm scala-${SCALA_VERSION}.deb

# Octave
RUN bash -c "${APT_INSTALL} octave octave-control octave-image octave-signal gnuplot pstoedit octave-dev fonts-freefont-otf && ${APT_CLEANUP}"
RUN bash -c "${APT_INSTALL} make && ${APT_CLEANUP}"
RUN bash -c 'echo -n "pkg install -forge io" | octave && \
 echo -n '"'"'pkg install "https://github.com/gnu-octave/statistics/archive/refs/tags/release-1.5.0.tar.gz"'"'"' | octave && \
 echo -n '"'"'pkg install "https://downloads.sourceforge.net/project/octave/Octave%20Forge%20Packages/Individual%20Package%20Releases/struct-1.0.18.tar.gz"'"'"' | octave && \
 echo -n '"'"'pkg install "https://downloads.sourceforge.net/project/octave/Octave%20Forge%20Packages/Individual%20Package%20Releases/optim-1.6.2.tar.gz"'"'"' | octave '

# Swift requirements
RUN bash -c "${APT_INSTALL} libcurl4 libpython3.8 libxml2 && ${APT_CLEANUP}"

# modified from https://github.com/apple/swift-docker/blob/main/5.5/ubuntu/20.04/slim/Dockerfile
ARG SWIFT_SIGNING_KEY=A62AE125BBBFBB96A6E042EC925CC1CCED3D1561
ARG SWIFT_PLATFORM=ubuntu22.04
ARG SWIFT_BRANCH=swift-5.8.1-release
ARG SWIFT_VERSION=swift-5.8.1-RELEASE
ARG SWIFT_WEBROOT=https://download.swift.org

ENV SWIFT_SIGNING_KEY=$SWIFT_SIGNING_KEY \
    SWIFT_PLATFORM=$SWIFT_PLATFORM \
    SWIFT_BRANCH=$SWIFT_BRANCH \
    SWIFT_VERSION=$SWIFT_VERSION \
    SWIFT_WEBROOT=$SWIFT_WEBROOT

RUN bash -c 'set -e; \
    SWIFT_WEBDIR="$SWIFT_WEBROOT/$SWIFT_BRANCH/$(echo $SWIFT_PLATFORM | tr -d .)" \
    && SWIFT_BIN_URL="$SWIFT_WEBDIR/$SWIFT_VERSION/$SWIFT_VERSION-$SWIFT_PLATFORM.tar.gz" \
    && SWIFT_SIG_URL="$SWIFT_BIN_URL.sig" \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -q update && apt-get -q install -y gnupg && rm -rf /var/lib/apt/lists/* \
    && export GNUPGHOME="$(mktemp -d)" \
    && wget -q -O swift.tar.gz "$SWIFT_BIN_URL" && wget -q -O swift.tar.gz.sig "$SWIFT_SIG_URL" \
    && gpg --batch --quiet --keyserver keyserver.ubuntu.com --recv-keys "$SWIFT_SIGNING_KEY" \
    && gpg --batch --verify swift.tar.gz.sig swift.tar.gz \
    && tar -xzf swift.tar.gz --directory / --strip-components=1 \
    && chmod -R o+r /usr/lib/swift \
    && rm -rf "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz \
    && apt-get purge --auto-remove -y gnupg'

RUN swift --version
# Swift ends

RUN echo 'options(repos=structure(c(CRAN="https://cloud.r-project.org/")))' > ~/.Rprofile

RUN bash -c "${APT_INSTALL} cmake && \
    git clone https://github.com/stevengj/nlopt.git && \
    cd nlopt && \
    git checkout master && \
    cmake -DCMAKE_CXX_FLAGS=-std=c++11 . && make && make install && cd .. && rm -r nlopt && \
    apt-get remove -y cmake && ${APT_CLEANUP}"

RUN ldconfig

RUN bash -c "${APT_INSTALL} libcurl4-openssl-dev libssl-dev libxml2-dev && ${APT_CLEANUP}"
RUN R -e 'install.packages(c("mvtnorm", "fICA", "JADE", "clue", "Rcpp", "RcppArmadillo", "psych", "GPArotation", "candisc", "car", "heplots", "vegan", "lavaan", "ggplot2", "tidyverse", "ggthemes", "lme4"))'

RUN bash -c "${APT_INSTALL} bsdmainutils iputils-ping && ${APT_CLEANUP}"

# Kotlin
ENV         KOTLIN_VERSION=2.1.0 \
            KOTLIN_HOME=/usr/local/kotlin

RUN bash -c 'cd  /tmp && \
            wget -q -k "https://github.com/JetBrains/kotlin/releases/download/v${KOTLIN_VERSION}/kotlin-compiler-${KOTLIN_VERSION}.zip"  && \
            unzip "kotlin-compiler-${KOTLIN_VERSION}.zip" && \
            rm "kotlin-compiler-${KOTLIN_VERSION}.zip" && \
            mkdir -p "${KOTLIN_HOME}" && \
            mv "/tmp/kotlinc/bin" "/tmp/kotlinc/lib" "${KOTLIN_HOME}" && \
            rm ${KOTLIN_HOME}/bin/*.bat && \
            chmod +x ${KOTLIN_HOME}/bin/* && \
            ln -s "${KOTLIN_HOME}/bin/"* "/usr/bin/" && \
            ${APT_CLEANUP}'

# Free Pascal
RUN bash -c "${APT_INSTALL} fpc && ${APT_CLEANUP}"

# Go
ENV GOLANG_VERSION=1.23.4
RUN wget -q https://dl.google.com/go/go${GOLANG_VERSION}.linux-amd64.tar.gz && tar -C /usr/local -xzf go${GOLANG_VERSION}.linux-amd64.tar.gz && rm go${GOLANG_VERSION}.linux-amd64.tar.gz
ENV PATH=$PATH:/usr/local/go/bin

# NodeJS and TypeScript
RUN bash -c "(wget -qO- https://deb.nodesource.com/setup_22.x | bash -) && ${APT_INSTALL} nodejs && ${APT_CLEANUP}"
RUN npm i npm@latest -g && bash -c "${APT_CLEANUP}"
RUN npm i ts-node -g && bash -c "${APT_CLEANUP}"

# Install NUnit runner for .NET
ENV NUNIT_RUNNER_VERSION=3.19.0
RUN dotnet tool install --tool-path /dotnet_tools/ NUnit.ConsoleRunner.NetCore --version ${NUNIT_RUNNER_VERSION} && \
    # We need to call NUnit directly with dotnet exec in order to properly declare custom references
    ln -s /dotnet_tools/.store/nunit.consolerunner.netcore/${NUNIT_RUNNER_VERSION}/nunit.consolerunner.netcore/${NUNIT_RUNNER_VERSION}/tools/net8.0/any/nunit3-console.dll /dotnet_tools/nunit.console.dll

# Install ffmpeg for transcoding videos
RUN bash -c "${APT_INSTALL} ffmpeg && ${APT_CLEANUP}"

# Install mongosh (adapted from https://github.com/rtsp/docker-mongosh/blob/main/Dockerfile)
ENV MONGODB_VERSION=6.0
ENV MONGOSH_VERSION=1.9.1

RUN bash -c "wget -qO- \"https://www.mongodb.org/static/pgp/server-${MONGODB_VERSION}.asc\" | gpg --dearmor -o /etc/apt/trusted.gpg.d/mongodb-org-${MONGODB_VERSION}.gpg && \
    echo \"deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/${MONGODB_VERSION} multiverse\" > /etc/apt/sources.list.d/mongodb-org-${MONGODB_VERSION}.list && \
    apt-get update && \
    ${APT_INSTALL} mongodb-mongosh=${MONGOSH_VERSION} && ${APT_CLEANUP}"

RUN mongosh --version

# Install cqlsh from pip
RUN $PYTHON -m pip install cqlsh six --upgrade && bash -c "${APT_CLEANUP}"
RUN cqlsh --version

# Install Elixir
RUN bash -c "${APT_INSTALL} erlang elixir && apt-get purge --auto-remove -y gnupg && ${APT_CLEANUP}" && \
    elixir --version

RUN $PYTHON -m pip install coverage && bash -c "${APT_CLEANUP}"

# Install OpenSCAD

RUN bash -c "${APT_INSTALL} openscad && ${APT_CLEANUP}"

# Install Racket
RUN bash -c "${APT_INSTALL} racket && ${APT_CLEANUP}"

# Install Racket packages
RUN raco pkg install -i sicp quickcheck

# Install Guile 3
RUN bash -c "${APT_INSTALL} guile-3.0 && ${APT_CLEANUP}"

# Install SpeechRecognition for basic speech recognition
RUN $PYTHON -m pip install SpeechRecognition soundfile && bash -c "${APT_CLEANUP}"
# Install Google Translate API library
RUN $PYTHON -m pip install --pre googletrans && bash -c "${APT_CLEANUP}"

# Install OCaml
RUN bash -c "${APT_INSTALL} ocaml opam && ${APT_CLEANUP}"
# Set up custom root and switch to install opam globally
ENV OPAMROOT=/opam
ENV OPAMSWITCH=system
# Running OPAM as root is okay (e.g. in dev mode)
ENV OPAMROOTISOK=1
RUN bash -c "mkdir /opam && \
    opam init --yes --bare --disable-sandboxing && \
    opam switch create system ocaml-system && \
    eval $(opam env)"

# Install Coq
ENV COQ_VERSION=8.18.0
RUN bash -c "${APT_INSTALL} libc6-dev libgmp-dev autoconf && ${APT_CLEANUP}"
RUN bash -c "opam repo add --all-switches --set-default coq-released https://coq.inria.fr/opam/released && \
    opam pin add -y -k version -j $(nproc) coq $COQ_VERSION && \
    opam install -y -j $(nproc) \
    coq-stdlib \
    coq-bignums \
    coq-equations \
    coq-quickchick \
    coq-hott \
    coq-ext-lib \
    coq-stdpp \
    coq-math-classes && \
    opam clean -a -c -s --logs"

USER agent

FROM complete AS sudo

USER root
ENV LANG en_US.UTF-8
ENV CLASSPATH .:/cs/java/junit.jar:/cs/java/hamcrest-core.jar:/cs/java/comtest.jar:/cs/java/Ali.jar:/cs/java/Graphics.jar:/cs/java/fxgui.jar:/cs/java/gui.jar

CMD ["/bin/bash", "-c", "su agent"]