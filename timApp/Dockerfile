FROM ubuntu:18.04

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
prosper \
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
texlive-generic-extra \
texlive-generic-recommended \
texlive-htmlxml \
texlive-humanities \
texlive-lang-african \
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
texlive-lang-indic \
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
texlive-omega \
texlive-pictures \
texlive-plain-extra \
texlive-pstricks \
texlive-publishers \
texlive-science \
texlive-xetex \
wget \
&& ${APT_CLEANUP}"

# Update dvisvgm so that it supports converting PDFs to SVGs
RUN bash -c "${APT_INSTALL} gcc g++ libpotrace-dev libgs-dev libkpathsea-dev && ${APT_CLEANUP}"
RUN FILE=`mktemp`; wget "https://github.com/mgieseki/dvisvgm/releases/download/2.4/dvisvgm-2.4.tar.gz" -qO $FILE && \
 tar -xf $FILE && \
 cd dvisvgm-2.4 && \
 ./configure && \
 make -j4 && \
 make install && \
 cd / && \
 ${APT_CLEANUP}

# Install Python, pip and other necessary packages

RUN bash -c "${APT_INSTALL} openssh-server python3 python3-pip && ${APT_CLEANUP}"

# lxml dependencies
# C-parser for PyYAML
# python-magic dependency
RUN bash -c "${APT_INSTALL} git-core \
 zlib1g-dev libxml2-dev libxslt-dev python3-dev \
 libyaml-dev \
 libmagic1 \
 libmagickwand-dev \
 libffi-dev \
 && ${APT_CLEANUP}"

ENV LD_LIBRARY_PATH /usr/local/lib

ENV PIP_INSTALL="python3 -m pip install"
RUN bash -c "${PIP_INSTALL} wheel setuptools && ${APT_CLEANUP}"
RUN bash -c "${APT_INSTALL} libxml2-dev libxmlsec1-dev libxmlsec1-openssl && ${APT_CLEANUP}"
RUN bash -c "${PIP_INSTALL} \
attrs \
autopep8 \
beautifulsoup4 \
bcrypt \
celery[redis] \
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
humanize \
isodate \
libsass \
lxml \
marshmallow \
marshmallow-dataclass[enum,union] \
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
selenium==3.141.0 \
sphinx \
sqlalchemy \
wand \
webargs \
&& ${PIP_INSTALL} requests --upgrade && ${APT_CLEANUP}"

# Install Pandoc
RUN FILE=`mktemp`; wget "https://github.com/jgm/pandoc/releases/download/2.7.1/pandoc-2.7.1-1-amd64.deb" -qO $FILE && \
 dpkg -i $FILE && rm $FILE && bash -c "${APT_CLEANUP}"

# Set name and email for git.
RUN git config --global user.email "agent@docker.com" && git config --global user.name "agent"

RUN mkdir /service

# Add user `agent` -- we don't want to run anything as root.
RUN useradd -M agent
RUN chown -R agent /service

RUN bash -c "${APT_INSTALL} software-properties-common && add-apt-repository ppa:malteworld/ppa && ${APT_CLEANUP}"
RUN bash -c "${APT_INSTALL} curl pdftk gpg-agent && ${APT_CLEANUP} && pdftk --version && curl --version"

RUN (curl -sL https://deb.nodesource.com/setup_12.x | bash -) && bash -c "${APT_INSTALL} nodejs && ${APT_CLEANUP}"
RUN npm i npm@latest -g && bash -c "${APT_CLEANUP}"
RUN npm install -g typescript@3.5 grunt typedoc tslint jspm typescript-formatter && npm cache clean --force && bash -c "${APT_CLEANUP}"

RUN mkdir /var/run/sshd
RUN echo 'root:test' | chpasswd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# Flask-Testing does not let us configure host, so we do it here.
RUN sed -i "s/port=port, use_reloader=False/host='0.0.0.0', port=port, use_reloader=False/" /usr/local/lib/python3.6/dist-packages/flask_testing/utils.py

RUN wget -q http://mirrors.ctan.org/support/latexmk/latexmk.pl -O /usr/bin/latexmk

WORKDIR /service
CMD python3 launch.py

EXPOSE 22
EXPOSE 5000
EXPOSE 5001
