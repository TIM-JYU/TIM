# Start from the Ubuntu image
from ubuntu:17.04

# Configure timezone and locale
run apt-get update
run apt-get install -y locales tzdata
run locale-gen en_US.UTF-8
env LANG en_US.UTF-8
env LANGUAGE en_US:en
env LC_ALL en_US.UTF-8
run echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata

# Install Python, pip and other necessary packages

run apt-get install --no-install-recommends -y openssh-server
run apt-get install --no-install-recommends -y python3.6 python3-setuptools python3-pip
run pip3 install --upgrade pip

# Make Python 3.6 the default one
run rm /usr/bin/python3
run ln -s /usr/bin/python3.6 /usr/bin/python3

run apt-get install --no-install-recommends -y git-core

run apt-get install --no-install-recommends -y zlib1g-dev libxml2-dev libxslt-dev python3.6-dev # lxml dependencies
run apt-get install --no-install-recommends -y libyaml-dev # C-parser for PyYAML
run apt-get install --no-install-recommends -y libmagic1 # python-magic dependency
run apt-get install --no-install-recommends -y gcc g++
run apt-get install --no-install-recommends -y libmagickwand-dev

# Install texlive
#
# Full install of texlive is needed for the latex-pdf printing functionality, as
# texlive doesn't have an (MiKTeX/MacTeX-esque) auto-install functionality for missing latex packages
# i.e. the whole package archive needs to be pre-installed or the set of usable packages needs to be
# severely limited
run apt-get install --no-install-recommends -y texlive-full


run pip3 install wheel
run pip3 install psycopg2
run pip3 install SQLAlchemy
run pip3 install flask
run pip3 install flask-compress
run pip3 install flask-cache
run pip3 install Flask-SQLAlchemy
run pip3 install beautifulsoup4
run pip3 install hypothesis
run pip3 install lxml
run pip3 install pyaml
run pip3 install ansi2html
run pip3 install cssutils
run pip3 install mmh3
run pip3 install pandocfilters
run pip3 install pypandoc
run pip3 install pylatex

run apt-get install --no-install-recommends -y libffi-dev
env LD_LIBRARY_PATH /usr/local/lib
run pip3 install cffi

run pip3 install gunicorn gevent eventlet tornado aiohttp
run pip3 install python-magic
run pip3 install git+git://github.com/miracle2k/webassets.git
run pip3 install libsass
run pip3 install flask-assets
run pip3 install yubico-client
run pip3 install pytz  # Not necessarily needed after all, as Python's stdlib supports UTC
run pip3 install python-dateutil
run pip3 install cssselect

# Install pandoc document converter and pypandoc, a python interface for pandoc
run apt-get install --no-install-recommends -y pandoc

# Update pandoc
run apt-get install --no-install-recommends -y wget
env PANDOC_URL "https://github.com/jgm/pandoc/releases/download/1.19.2.1/pandoc-1.19.2.1-1-amd64.deb"
run FILE=`mktemp`; wget "$PANDOC_URL" -qO $FILE && dpkg -i $FILE && rm $FILE


run pip3 install Flask-Migrate
run pip3 install selenium
run pip3 install Flask-Testing
run pip3 install isodate
run pip3 install humanize
run pip3 install autopep8
run pip3 install git+git://github.com/dadadel/pyment.git
run pip3 install docformatter
run pip3 install filelock
run pip3 install mypy
run pip3 install pillow
run pip3 install wand

# sphinxcontrib-httpdomain does not work with 1.6
run pip3 install sphinx==1.5.6
run pip3 install sphinxcontrib-httpdomain
run pip3 install recommonmark

# requests must be the last package to be installed!
# After this, pip3 stops working because it depends
# on older version of requests package.
run pip3 install requests --upgrade



# Remove pip. It's no longer needed, we get slightly smaller image and less programs
# for a potential attacker to use.
# UPDATE: Don't remove pip; it causes problems with running tests.
# run apt-get -y --purge remove python3-pip

# Set name and email for git.
run git config --global user.email "agent@docker.com"
run git config --global user.name "agent"

run mkdir /service

# Add user `agent` -- we don't want to run anything as root.
run useradd -M agent
run chown -R agent /service

run apt-get install --no-install-recommends -y npm
run ln -s /usr/bin/nodejs /usr/bin/node
run npm install -g typescript tslint jspm typescript-formatter

run mkdir /var/run/sshd
run echo 'root:test' | chpasswd
run sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# Flask-Testing does not let us configure host, so we do it here.
run sed -i "s/port=port, use_reloader=False/host='0.0.0.0', port=port, use_reloader=False/" /usr/local/lib/python3.6/dist-packages/flask_testing/utils.py

# Ensure that netcat is installed.
run apt-get install --no-install-recommends -y netcat-openbsd

workdir /service
cmd python3 launch.py

expose 22
expose 5000
expose 5001
