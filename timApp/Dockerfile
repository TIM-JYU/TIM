FROM ubuntu:20.04

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -rf /var/lib/apt/lists /dvisvgm-2.4 /usr/share/doc ~/.cache"

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

# Update dvisvgm so that it supports converting PDFs to SVGs
RUN bash -c "${APT_INSTALL} gcc g++ libgs-dev libkpathsea-dev pkg-config libfreetype6-dev make && ${APT_CLEANUP}"
RUN FILE=`mktemp`; wget "https://github.com/mgieseki/dvisvgm/releases/download/2.10/dvisvgm-2.10.tar.gz" -qO $FILE && \
 tar -xf $FILE && \
 cd dvisvgm-2.10 && \
 ./configure --enable-bundled-libs && \
 make -j4 && \
 make install && \
 cd / && \
 ${APT_CLEANUP} && \
 rm -rf /dvisvgm-2.10

# Install Python, pip and other necessary packages

RUN bash -c "${APT_INSTALL} openssh-server python3.8 && ${APT_CLEANUP}"

# lxml dependencies
# C-parser for PyYAML
# python-magic dependency
RUN bash -c "${APT_INSTALL} \
 git-core \
 libffi-dev \
 libmagic1 \
 libmagickwand-dev \
 libvoikko1 \
 libxml2-dev \
 libxmlsec1-dev \
 libxmlsec1-openssl \
 libxslt-dev \
 libyaml-dev \
 python3.8-dev \
 voikko-fi \
 zlib1g-dev \
 && ${APT_CLEANUP}"

RUN wget -q https://bootstrap.pypa.io/get-pip.py && python3.8 get-pip.py && rm get-pip.py

ENV PIP_INSTALL="python3.8 -m pip install"
RUN bash -c "${PIP_INSTALL} wheel setuptools && ${APT_CLEANUP}"
RUN bash -c "${PIP_INSTALL} \
attrs \
autopep8 \
beautifulsoup4 \
bcrypt \
bleach \
celery[redis]==4.4 \
cffi \
cssselect \
docformatter \
filelock \
flask \
flask-apispec \
flask-assets \
flask-caching \
flask-compress \
flask-migrate \
flask-oidc \
flask-openid \
flask-sqlalchemy \
flask-testing \
flask-wtf \
gevent \
git+git://github.com/miracle2k/webassets.git \
gunicorn \
html5lib \
humanize \
isodate \
libsass \
lxml \
mailmanclient \
marshmallow \
marshmallow-enum \
marshmallow_union \
mmh3 \
mypy \
pandocfilters \
pillow \
psycopg2-binary \
pyaml \
pylatex \
pypandoc \
python-dateutil \
python-magic \
python3-saml \
pytz \
recommonmark \
responses \
selenium==4.0.0b1 \
sphinx \
sqlalchemy\<1.4.0 \
sqlalchemy-utils \
typing-inspect \
voikko \
wand \
webargs==5.5 \
&& ${PIP_INSTALL} requests --upgrade && ${APT_CLEANUP}"

# Install Pandoc
RUN FILE=`mktemp`; wget "https://github.com/jgm/pandoc/releases/download/2.10.1/pandoc-2.10.1-1-amd64.deb" -qO $FILE && \
 dpkg -i $FILE && rm $FILE && bash -c "${APT_CLEANUP}"

# Set name and email for git.
RUN git config --global user.email "agent@docker.com" && git config --global user.name "agent"

RUN mkdir /service

# Add user `agent` -- we don't want to run anything as root.
RUN useradd -M agent
RUN chown -R agent /service

RUN bash -c "${APT_INSTALL} curl pdftk gpg-agent poppler-utils && ${APT_CLEANUP} && pdftk --version && curl --version"

# On current TIM production server, using latest versions of Node and NPM will cause
# ./npmi and ./js scripts to fail, causing the following symptoms:
#
# * ./npmi simply freezes right away, consuming memory until an out of memory exception is thrown.
# * ./js does not freeze, but it throws a permission error after building scripts when trying to copy them to the final
#   output directory.
#
# It has not yet been investigated whether it's because of Node or NPM update.
RUN (curl -sL https://deb.nodesource.com/setup_12.x | bash -) && bash -c "${APT_INSTALL} nodejs && ${APT_CLEANUP}"
RUN npm i npm@6.14.8 -g && bash -c "${APT_CLEANUP}"

RUN mkdir /var/run/sshd
RUN echo 'root:test' | chpasswd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# Flask-Testing does not let us configure host, so we do it here.
RUN sed -i "s/port=port, use_reloader=False/host='0.0.0.0', port=port, use_reloader=False/" /usr/local/lib/python3.8/dist-packages/flask_testing/utils.py
# Fix newest Werkzeug incompatibility
RUN sed -i "s/from werkzeug import cached_property/from werkzeug.utils import cached_property/" /usr/local/lib/python3.8/dist-packages/flask_testing/utils.py

RUN wget -q http://mirrors.ctan.org/support/latexmk/latexmk.pl -O /usr/bin/latexmk

# Chromedriver (for running tests)
# Taken from https://github.com/SeleniumHQ/docker-selenium/blob/trunk/NodeChrome/Dockerfile
ARG CHROME_VERSION="google-chrome-stable"
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
  && echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list \
  && apt-get update -qqy \
  && apt-get -qqy install unzip \
    ${CHROME_VERSION:-google-chrome-stable} \
  && rm /etc/apt/sources.list.d/google-chrome.list \
  && rm -rf /var/lib/apt/lists/* /var/cache/apt/*

RUN wget -q https://raw.githubusercontent.com/SeleniumHQ/docker-selenium/trunk/NodeChrome/wrap_chrome_binary -O /usr/local/bin/wrap_chrome_binary && chmod +x /usr/local/bin/wrap_chrome_binary
RUN /usr/local/bin/wrap_chrome_binary

ARG CHROME_DRIVER_VERSION
RUN if [ -z "$CHROME_DRIVER_VERSION" ]; \
  then CHROME_MAJOR_VERSION=$(google-chrome --version | sed -E "s/.* ([0-9]+)(\.[0-9]+){3}.*/\1/") \
    && CHROME_DRIVER_VERSION=$(wget --no-verbose -O - "https://chromedriver.storage.googleapis.com/LATEST_RELEASE_${CHROME_MAJOR_VERSION}"); \
  fi \
  && echo "Using chromedriver version: "$CHROME_DRIVER_VERSION \
  && wget --no-verbose -O /tmp/chromedriver_linux64.zip https://chromedriver.storage.googleapis.com/$CHROME_DRIVER_VERSION/chromedriver_linux64.zip \
  && rm -rf /opt/selenium/chromedriver \
  && unzip /tmp/chromedriver_linux64.zip -d /opt/selenium \
  && rm /tmp/chromedriver_linux64.zip \
  && mv /opt/selenium/chromedriver /opt/selenium/chromedriver-$CHROME_DRIVER_VERSION \
  && chmod 755 /opt/selenium/chromedriver-$CHROME_DRIVER_VERSION \
  && ln -fs /opt/selenium/chromedriver-$CHROME_DRIVER_VERSION /usr/bin/chromedriver


WORKDIR /service

CMD python3 launch.py

EXPOSE 22
EXPOSE 5000
EXPOSE 5001
