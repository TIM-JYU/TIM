# Start from the Ubuntu image
from ubuntu:16.04
maintainer Ville Tirronen "ville.tirronen@jyu.fi"

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
run apt-get install --no-install-recommends -y python3 python3-setuptools python3-pip
run apt-get install --no-install-recommends -y git-core

run apt-get install --no-install-recommends -y zlib1g-dev libxml2-dev libxslt-dev python3-dev # lxml dependencies
run apt-get install --no-install-recommends -y libyaml-dev # C-parser for PyYAML
run apt-get install --no-install-recommends -y libmagic1 # python-magic dependency
run apt-get install --no-install-recommends -y gcc g++

run apt-get install --no-install-recommends -y python3-psycopg2

run pip3 install wheel
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
run pip3 install Flask-Migrate
run pip3 install selenium
run pip3 install Flask-Testing
run pip3 install isodate
run pip3 install humanize
run pip3 install autopep8
run pip3 install git+git://github.com/dadadel/pyment.git
run pip3 install docformatter
run pip3 install filelock

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

# angular-timer depends on humanize-duration version 2.8 but it appears to work fine with 3.10, so we force the install.
# Due to https://github.com/angular-ui/ui-grid/issues/5926, we install version 4.0.1 of angular-ui-grid and not 4.0.2.
run apt-get install --no-install-recommends -y npm \
 && npm install -g bower \
 && npm install uglify-js -g \
 && ln -s /usr/bin/nodejs /usr/bin/node \
 && bower --allow-root install angular#~1.5 \
 && bower --allow-root install angular-ui-ace#36844ff7c0e0d9445bc8e31514d7f0f59cb8b048 \
 && bower --allow-root install ng-file-upload#~12.2 \
 && bower --allow-root install jquery#~2.1 \
 && bower --allow-root install moment#~2.17 \
 && bower --allow-root install angular-sanitize#~1.5 \
 && bower --allow-root install bootstrap#~3.3 \
 && bower --allow-root install jquery-ui#~1.11 \
 && bower --allow-root install katex#~0.7 \
 && bower --allow-root install rangyinputs \
 && bower --allow-root install ngstorage#~0.3 \
 && bower --allow-root install chartjs#~1.0 \
 && bower --allow-root install oclazyload#~1.1 \
 && bower --allow-root install bootstrap-sass#~3.3 \
 && bower --allow-root install angular-bootstrap#~2.5 \
 && bower --allow-root install angular-animate#~1.5 \
 && bower --allow-root install angular-touch#~1.5 \
 && bower --allow-root install angular-ui-grid#4.0.1 \
 && bower --allow-root install angular-bootstrap-colorpicker#~3.0 \
 && bower --allow-root install eonasdan-bootstrap-datetimepicker#latest \
 && bower --allow-root install angular-eonasdan-datetimepicker#~0.3 \
 && bower --allow-root install angular-messages#~1.5 \
 && bower --allow-root install humanize-duration#~3.10 \
 && bower --allow-root --force install angular-timer#~1.3 \
 && apt-get purge -y --auto-remove npm

# Update the below date whenever Bower packages are upgraded, added or removed.
# Otherwise TIM might not re-copy the Bower libs because it only inspects the top-level directory file names
# in bower_components.
run touch /bower_components/2017-03-02.stamp

# We need to patch the ui-ace module to fix this issue: https://github.com/angular-ui/ui-ace/issues/27
run sed -i 's/var opts = angular\.extend({}, options, scope\.$eval(attrs\.uiAce));/var opts = angular.extend({}, options, scope.$eval(attrs.uiAce, scope));/' /bower_components/angular-ui-ace/ui-ace.js

# Force auto-render extension to throw exceptions instead of printing to console.
run sed -i 's/f instanceof katex\.ParseError/false/' /bower_components/katex/dist/contrib/auto-render.min.js

run mkdir /var/run/sshd
run echo 'root:test' | chpasswd
run sed -i 's/PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# Flask-Testing does not let us configure host, so we do it here.
run sed -i "s/port=port, use_reloader=False/host='0.0.0.0', port=port, use_reloader=False/" /usr/local/lib/python3.5/dist-packages/flask_testing/utils.py

# Ensure that netcat is installed.
run apt-get install --no-install-recommends -y netcat-openbsd

# Install pandoc document converter and pypandoc, a python interface for pandoc
run apt-get install --no-install-recommends -y pandoc
run pip3 install pypandoc

# Install texlive
#
# Full install of texlive is needed for the latex-pdf printing functionality, as
# texlive doesn't have an (MiKTeX/MacTeX-esque) auto-install functionality for missing latex packages
# i.e. the whole package archive needs to be pre-installed or the set of usable packages needs to be
# severely limited
run apt-get install --no-install-recommends -y texlive-full

# Install pylatex, the python lib for using latex
run pip3 install pylatex

# Update pandoc //TODO: needs to be written in a better form
run apt-get install --no-install-recommends -y wget
env path "https://github.com/jgm/pandoc/releases/download/1.19.1/pandoc-1.19.1-1-amd64.deb"
run mkdir -p "/var/tmp/timApp/installtemp"
env installfile "/var/tmp/timApp/installtemp/pandocinstaller1.19.1-1-amd64.deb"
run wget -qO $installfile $path
run dpkg -i $installfile

run apt-get install --no-install-recommends -y python-pandocfilters
run apt-get install --no-install-recommends -y python3-pandocfilters
run apt-get install -y python-pip
run pip install pandocfilters -U


# Default startup command.
cmd cd /service/timApp && source /service/scripts/_initenv.sh && python3 launch.py

expose 22
expose 5000
expose 5001
