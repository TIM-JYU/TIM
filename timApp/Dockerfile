FROM ubuntu:22.04 as base
LABEL org.opencontainers.image.authors="tim@jyu.fi"
LABEL org.opencontainers.image.source="https://github.com/TIM-JYU/TIM"

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -rf /var/lib/apt/lists /usr/share/doc ~/.cache"

# Configure timezone and locale
RUN bash -c "${APT_INSTALL} locales tzdata && ${APT_CLEANUP}"
RUN locale-gen en_US.UTF-8 && bash -c "${APT_CLEANUP}"
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8
RUN echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata && bash -c "${APT_CLEANUP}"

# Install dependencies of texlive-full excluding packages that are not needed (such as documentation files).
# This almost-full installation of TeX Live is needed for the latex-pdf printing functionality, as
# TeX Live doesn't have an (MiKTeX/MacTeX-esque) auto-install functionality for missing LaTeX packages,
# i.e. the whole package archive needs to be pre-installed or the set of usable packages needs to be
# severely limited.
RUN bash -c "${APT_INSTALL} \
biber \
ca-certificates \
cm-super \
dvidvi \
dvipng \
feynmf \
fonts-texgyre \
fragmaster \
latex-cjk-all \
latexmk \
lcdf-typetools \
lmodern \
psutils \
purifyeps \
software-properties-common \
t1utils \
tex-gyre \
texlive-base \
texlive-bibtex-extra \
texlive-binaries \
texlive-extra-utils \
texlive-font-utils \
texlive-fonts-extra \
texlive-fonts-recommended \
texlive-formats-extra \
texlive-games \
texlive-humanities \
texlive-lang-arabic \
texlive-lang-chinese \
texlive-lang-cjk \
texlive-lang-cyrillic \
texlive-lang-czechslovak \
texlive-lang-english \
texlive-lang-european \
texlive-lang-french \
texlive-lang-german \
texlive-lang-greek \
texlive-lang-italian \
texlive-lang-japanese \
texlive-lang-korean \
texlive-lang-other \
texlive-lang-polish \
texlive-lang-portuguese \
texlive-lang-spanish \
texlive-latex-base \
texlive-latex-extra \
texlive-latex-recommended \
texlive-luatex \
texlive-metapost \
texlive-music \
texlive-pictures \
texlive-pstricks \
texlive-publishers \
texlive-science \
texlive-xetex \
wget \
&& ${APT_CLEANUP}"

# Install gpg and gpg-agent for verifying signatures
RUN bash -c "${APT_INSTALL} gpg-agent && ${APT_CLEANUP}"

# Update dvisvgm so that it supports converting PDFs to SVGs
ENV DVISVGM_VERSION="2.14"
RUN bash -c "${APT_INSTALL} gcc g++ libgs-dev libkpathsea-dev pkg-config libfreetype6-dev make && ${APT_CLEANUP}"
RUN FILE=`mktemp`; wget "https://github.com/mgieseki/dvisvgm/releases/download/${DVISVGM_VERSION}/dvisvgm-${DVISVGM_VERSION}.tar.gz" -qO $FILE && \
 tar -xf $FILE && \
 cd dvisvgm-$DVISVGM_VERSION && \
 ./configure --enable-bundled-libs && \
 make -j4 && \
 make install && \
 cd / && \
 ${APT_CLEANUP} && \
 rm -rf /dvisvgm-$DVISVGM_VERSION

FROM base as complete

# Install Python, pip and other necessary packages

ENV PY_VERSION=3.11
ENV PYTHON=python$PY_VERSION
RUN bash -c "add-apt-repository -y ppa:deadsnakes/ppa && ${APT_CLEANUP}"
RUN bash -c "${APT_INSTALL} ${PYTHON} ${PYTHON}-distutils && ${APT_CLEANUP}"

RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/${PYTHON} 20

# lxml dependencies
# C-parser for PyYAML
# python-magic dependency
# img2pdf w/ python lib
RUN bash -c "${APT_INSTALL} \
 git-core \
 img2pdf \
 libffi-dev \
 libmagic1 \
 libmagickwand-dev \
 libvoikko1 \
 libxml2-dev \
 libxmlsec1-dev \
 libxmlsec1-openssl \
 libxslt-dev \
 libyaml-dev \
${PYTHON}-dev \
 voikko-fi \
 zlib1g-dev \
 && ${APT_CLEANUP}"

RUN wget -q https://bootstrap.pypa.io/get-pip.py && ${PYTHON} get-pip.py && rm get-pip.py

# Install xmlsec binary which is needed for PySAML2
RUN bash -c "${APT_INSTALL} xmlsec1 && ${APT_CLEANUP}"

ENV PIP_INSTALL="${PYTHON} -m pip install"
RUN bash -c "${PIP_INSTALL} --upgrade --ignore-installed poetry==1.5.1 requests blinker && ${APT_CLEANUP}"

COPY pyproject.toml poetry.lock ./
RUN bash -c " \
poetry config virtualenvs.create false && \
poetry config virtualenvs.in-project false && \
poetry install --without dev && \
${APT_CLEANUP} && \
rm pyproject.toml poetry.lock"

# Install Pandoc
RUN FILE=`mktemp`; wget "https://github.com/jgm/pandoc/releases/download/2.19.2/pandoc-2.19.2-1-amd64.deb" -qO $FILE && \
 dpkg -i $FILE && rm $FILE && bash -c "${APT_CLEANUP}"

# Set name and email for git.
RUN git config --global user.email "agent@docker.com" && git config --global user.name "agent"
RUN git config --global --add safe.directory '*'

RUN mkdir /service

# Add user `agent` -- we don't want to run anything as root.
RUN useradd -M agent
RUN chown -R agent /service

RUN bash -c "${APT_INSTALL} curl qpdf poppler-utils && ${APT_CLEANUP} && \
    curl --version && qpdf --version"

# On current TIM production server, using latest versions of Node and NPM will cause
# ./npmi and ./js scripts to fail, causing the following symptoms:
#
# * ./npmi simply freezes right away, consuming memory until an out of memory exception is thrown.
# * ./js does not freeze, but it throws a permission error after building scripts when trying to copy them to the final
#   output directory.
#
# It has not yet been investigated whether it's because of Node or NPM update.
RUN (curl -sL https://deb.nodesource.com/setup_18.x | bash -) && bash -c "${APT_INSTALL} nodejs && ${APT_CLEANUP}"
RUN npm i npm@6 -g && bash -c "${APT_CLEANUP}"
# Fix issue with node-gyp until we upgrade NPM, see https://github.com/TIM-JYU/TIM/issues/3479 and https://github.com/nodejs/node-gyp/issues/2219
RUN sed -i "s/^\( *\)\(build_file_contents = open(build_file_path, 'rU').read().*\)$/\1build_file_contents = open(build_file_path, 'r').read()/" /usr/lib/node_modules/npm/node_modules/node-gyp/gyp/pylib/gyp/input.py

# Flask-Testing does not let us configure host, so we do it here.
RUN sed -i "s/port=port, use_reloader=False/host='0.0.0.0', port=port, use_reloader=False/" /usr/local/lib/${PYTHON}/dist-packages/flask_testing/utils.py
# Fix newest Werkzeug incompatibility
RUN sed -i "s/from werkzeug import cached_property/from werkzeug.utils import cached_property/" /usr/local/lib/${PYTHON}/dist-packages/flask_testing/utils.py

RUN wget -q https://www.texlive.info/CTAN/support/latexmk/latexmk.pl -O /usr/bin/latexmk

RUN bash -c "${APT_INSTALL} jq ripgrep unzip && ${APT_CLEANUP}"
RUN bash -c "${PIP_INSTALL} coverage && ${APT_CLEANUP}"

# Chromedriver (for running tests)
# Taken from https://github.com/SeleniumHQ/docker-selenium/blob/trunk/NodeChrome/Dockerfile
ARG CHROME_VERSION="google-chrome-stable"
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
  && echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list \
  && apt-get update -qqy \
  && apt-get -qqy install \
    ${CHROME_VERSION:-google-chrome-stable} \
  && rm /etc/apt/sources.list.d/google-chrome.list \
  && rm -rf /var/lib/apt/lists/* /var/cache/apt/*

RUN wget -q https://raw.githubusercontent.com/SeleniumHQ/docker-selenium/trunk/NodeChrome/wrap_chrome_binary -O /usr/local/bin/wrap_chrome_binary && chmod +x /usr/local/bin/wrap_chrome_binary
RUN /usr/local/bin/wrap_chrome_binary

ARG CHROME_DRIVER_VERSION
RUN if [ ! -z "$CHROME_DRIVER_VERSION" ]; \
  then CHROME_DRIVER_URL=https://edgedl.me.gvt1.com/edgedl/chrome/chrome-for-testing/$CHROME_DRIVER_VERSION/linux64/chromedriver-linux64.zip ; \
  else echo "Getting ChromeDriver binary from https://googlechromelabs.github.io/chrome-for-testing/last-known-good-versions-with-downloads.json" \
    && CFT_URL=https://googlechromelabs.github.io/chrome-for-testing/last-known-good-versions-with-downloads.json \
    && CFT_CHANNEL="Stable" \
    && if [ "$CHROME_VERSION" = "google-chrome-beta" ]; then CFT_CHANNEL="Beta" ; fi \
    && if [ "$CHROME_VERSION" = "google-chrome-unstable" ]; then CFT_CHANNEL="Dev" ; fi \
    && echo $CFT_CHANNEL \
    && CTF_VALUES=$(curl -sSL $CFT_URL | jq -r --arg CFT_CHANNEL "$CFT_CHANNEL" '.channels[] | select (.channel==$CFT_CHANNEL)') \
    && CHROME_DRIVER_VERSION=$(echo $CTF_VALUES | jq -r '.version' ) \
    && CHROME_DRIVER_URL=$(echo $CTF_VALUES | jq -r '.downloads.chromedriver[] | select(.platform=="linux64") | .url' ) ; \
  fi \
  && echo "Using ChromeDriver from: "$CHROME_DRIVER_URL \
  && echo "Using ChromeDriver version: "$CHROME_DRIVER_VERSION \
  && wget --no-verbose -O /tmp/chromedriver_linux64.zip $CHROME_DRIVER_URL \
  && rm -rf /opt/selenium/chromedriver \
  && unzip /tmp/chromedriver_linux64.zip -d /opt/selenium \
  && rm /tmp/chromedriver_linux64.zip \
  && mv /opt/selenium/chromedriver-linux64/chromedriver /opt/selenium/chromedriver-$CHROME_DRIVER_VERSION \
  && chmod 755 /opt/selenium/chromedriver-$CHROME_DRIVER_VERSION \
  && ln -fs /opt/selenium/chromedriver-$CHROME_DRIVER_VERSION /usr/bin/chromedriver


WORKDIR /service

CMD python3 launch.py

EXPOSE 5000
EXPOSE 5001
