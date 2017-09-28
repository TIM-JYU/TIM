# Start from the Ubuntu image
FROM ubuntu:17.04

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

RUN bash -c "${APT_INSTALL} openssh-server python3.6 python3-setuptools python3-pip && pip3 install --upgrade pip && ${APT_CLEANUP}"

# Make Python 3.6 the default one
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.5 1 && \
    update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.6 2 && \
    python3 -V

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

# Install texlive
#
# Full install of texlive is needed for the latex-pdf printing functionality, as
# texlive doesn't have an (MiKTeX/MacTeX-esque) auto-install functionality for missing latex packages
# i.e. the whole package archive needs to be pre-installed or the set of usable packages needs to be
# severely limited
RUN bash -c "${APT_INSTALL} texlive-full && ${APT_CLEANUP}"

RUN pip3 install wheel
RUN pip3 install psycopg2
RUN pip3 install sqlalchemy
RUN pip3 install flask
RUN pip3 install flask-compress
RUN pip3 install flask-cache
RUN pip3 install flask-sqlalchemy
RUN pip3 install beautifulsoup4
RUN pip3 install lxml
RUN pip3 install pyaml
RUN pip3 install ansi2html
RUN pip3 install cssutils
RUN pip3 install mmh3
RUN pip3 install pandocfilters
RUN pip3 install pypandoc
RUN pip3 install pylatex

RUN bash -c "${APT_INSTALL} libffi-dev && ${APT_CLEANUP}"
ENV LD_LIBRARY_PATH /usr/local/lib
RUN pip3 install cffi

RUN pip3 install gunicorn gevent
RUN pip3 install python-magic
RUN pip3 install git+git://github.com/miracle2k/webassets.git
RUN pip3 install libsass
RUN pip3 install flask-assets
RUN pip3 install yubico-client
RUN pip3 install pytz  # Not necessarily needed after all, as Python stdlib supports UTC
RUN pip3 install python-dateutil
RUN pip3 install cssselect

# Install pandoc document converter and pypandoc, a python interface for pandoc
RUN bash -c "${APT_INSTALL} wget && ${APT_CLEANUP}"
RUN FILE=`mktemp`; wget "https://github.com/jgm/pandoc/releases/download/1.19.2.1/pandoc-1.19.2.1-1-amd64.deb" -qO $FILE && dpkg -i $FILE && rm $FILE

RUN pip3 install flask-migrate
RUN pip3 install selenium
RUN pip3 install flask-testing
RUN pip3 install isodate
RUN pip3 install humanize
RUN pip3 install autopep8
RUN pip3 install git+git://github.com/dadadel/pyment.git
RUN pip3 install docformatter
RUN pip3 install filelock
RUN pip3 install mypy
RUN pip3 install pillow
RUN pip3 install wand

# sphinxcontrib-httpdomain does not work with 1.6
RUN pip3 install sphinx==1.5.6
RUN pip3 install sphinxcontrib-httpdomain
RUN pip3 install recommonmark

# requests must be the last package to be installed!
# After this, pip3 stops working because it depends
# on older version of requests package.
RUN pip3 install requests --upgrade

# Set name and email for git.
RUN git config --global user.email "agent@docker.com"
RUN git config --global user.name "agent"

RUN mkdir /service

# Add user `agent` -- we don't want to run anything as root.
RUN useradd -M agent
RUN chown -R agent /service

RUN bash -c "${APT_INSTALL} npm && ${APT_CLEANUP}"
RUN ln -s /usr/bin/nodejs /usr/bin/node
RUN npm install -g typescript tslint jspm typescript-formatter && npm cache clean

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
