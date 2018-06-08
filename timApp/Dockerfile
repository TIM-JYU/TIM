# Start from the Ubuntu image
FROM ubuntu:17.10

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -r /var/lib/apt/lists/*"

# Configure timezone and locale
RUN bash -c "${APT_INSTALL} locales tzdata && ${APT_CLEANUP}"
RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8
RUN echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata

# Install Python, pip and other necessary packages

RUN bash -c "${APT_INSTALL} openssh-server wget ca-certificates python3.6 && ${APT_CLEANUP}"

# Make Python 3.6 the default one
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.6 2 && \
    python3 -V

RUN wget -q https://bootstrap.pypa.io/get-pip.py && python3 get-pip.py && rm get-pip.py

# lxml dependencies
# C-parser for PyYAML
# python-magic dependency
RUN bash -c "${APT_INSTALL} git-core \
 zlib1g-dev libxml2-dev libxslt-dev python3.6-dev \
 libyaml-dev \
 libmagic1 \
 gcc g++ \
 libmagickwand-dev \
 && ${APT_CLEANUP}"

RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.6 2 && \
    python3 -V

# Install dependencies of texlive-full excluding packages that are not needed (such as documentation files).
# This almost-full installation of TeX Live is needed for the latex-pdf printing functionality, as
# TeX Live doesn't have an (MiKTeX/MacTeX-esque) auto-install functionality for missing LaTeX packages,
# i.e. the whole package archive needs to be pre-installed or the set of usable packages needs to be
# severely limited.
RUN bash -c "${APT_INSTALL} \
biber \
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
&& ${APT_CLEANUP}"

RUN bash -c "${APT_INSTALL} libffi-dev && ${APT_CLEANUP}"
ENV LD_LIBRARY_PATH /usr/local/lib

# sphinxcontrib-httpdomain does not work with 1.6
ENV PIP_INSTALL="python3 -m pip install" \
    PIP_CLEANUP="rm -r ~/.cache/pip"
RUN bash -c "${PIP_INSTALL} \
ansi2html \
autopep8 \
beautifulsoup4 \
bcrypt \
cffi \
cssselect \
cssutils \
docformatter \
filelock \
flask \
flask-assets \
flask-caching \
flask-compress \
flask-migrate \
flask-openid \
flask-sqlalchemy \
flask-testing \
gevent \
git+git://github.com/dadadel/pyment.git@baaccc191db74e88c13f0cc27951a46938249a08 \
git+git://github.com/miracle2k/webassets.git \
gunicorn \
humanize \
isodate \
libsass \
lxml \
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
pytz \
recommonmark \
responses \
selenium==3.8.1 \
sphinx==1.5.6 \
sphinxcontrib-httpdomain \
sqlalchemy \
wand \
wheel \
yubico-client \
&& ${PIP_INSTALL} requests --upgrade && ${PIP_CLEANUP}"

# Install Pandoc document converter
RUN FILE=`mktemp`; wget "https://github.com/jgm/pandoc/releases/download/2.1.2/pandoc-2.1.2-1-amd64.deb" -qO $FILE && dpkg -i $FILE && rm $FILE

# Update dvisvgm so that it supports converting PDFs to SVGs
RUN bash -c "${APT_INSTALL} libpotrace-dev libgs-dev libkpathsea-dev && ${APT_CLEANUP}"
RUN FILE=`mktemp`; wget "https://github.com/mgieseki/dvisvgm/releases/download/2.4/dvisvgm-2.4.tar.gz" -qO $FILE && \
 tar -xf $FILE && \
 cd dvisvgm-2.4 && \
 ./configure && \
 make -j4 && \
 make install && \
 find . -type f -name '*.o' -delete && \
 find . -type f -name '*.a' -delete && \
 cd .. && \
 rm $FILE

# Set name and email for git.
RUN git config --global user.email "agent@docker.com"
RUN git config --global user.name "agent"

RUN mkdir /service

# Add user `agent` -- we don't want to run anything as root.
RUN useradd -M agent
RUN chown -R agent /service

RUN bash -c "${APT_INSTALL} curl && ${APT_CLEANUP}"
RUN (curl -sL https://deb.nodesource.com/setup_10.x | bash -) && bash -c "${APT_INSTALL} nodejs && ${APT_CLEANUP}"
RUN npm i npm@latest -g
RUN npm install -g typescript@2.8 typedoc tslint jspm typescript-formatter && npm cache clean --force

RUN mkdir /var/run/sshd
RUN echo 'root:test' | chpasswd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# Flask-Testing does not let us configure host, so we do it here.
RUN sed -i "s/port=port, use_reloader=False/host='0.0.0.0', port=port, use_reloader=False/" /usr/local/lib/python3.6/dist-packages/flask_testing/utils.py

WORKDIR /service
CMD python3 launch.py

EXPOSE 22
EXPOSE 5000
EXPOSE 5001
