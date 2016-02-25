# Start from the Ubuntu image
from ubuntu
maintainer Ville Tirronen "ville.tirronen@jyu.fi"

# Set locale
env LANG en_US.utf8
env LANGUAGE en_US.utf8
run locale-gen en_US.utf8

# Install Python, pip and other necessary packages

run apt-get update
run apt-get install -y python3
run apt-get install -y python3-pip
run apt-get install -y git-core

run apt-get install -y zlib1g-dev # lxml dependency
run apt-get install -y libxml2-dev libxslt-dev python3-dev # lxml dependency
run apt-get install -y libyaml-dev # C-parser for PyYAML

# Add the PostgreSQL PGP key
run apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8

# Add PostgreSQL's repository.
run echo "deb http://apt.postgresql.org/pub/repos/apt/ precise-pgdg main" > /etc/apt/sources.list.d/pgdg.list

run apt-get update
run apt-get install -y libpq-dev # Needed by PostGreSQL
# Install postgresql 9.3, version 9.4 didn't install correctly
run apt-get install -y postgresql-9.3

run pip3 install SQLAlchemy
run pip3 install flask
run pip3 install flask-compress
run pip3 install flask-cache
run pip3 install Flask-SQLAlchemy
run pip3 install beautifulsoup4
run pip3 install pycontracts==1.7.6 # latest version doesn't install on python3
run pip3 install hypothesis
run pip3 install lxml
run pip3 install pyaml
run pip3 install ansi2html
run pip3 install cssutils
run pip3 install mmh3
run pip3 install psycopg2

run apt-get install -y libffi-dev
env LD_LIBRARY_PATH /usr/local/lib
run pip3 install cffi

run pip3 install git+https://github.com/benoitc/gunicorn.git gevent==1.1rc4 eventlet tornado aiohttp
run pip3 install typing
run pip3 install python-magic

# Need to update the package "six"; otherwise there will be a strange error with Flask's reloader
# run pip3 install -U six

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

expose 5000
expose 22

run apt-get install -y npm
run npm install -g bower
run ln -s /usr/bin/nodejs /usr/bin/node
run bower --allow-root install angular#~1.4
run bower --allow-root install angular-ui-ace#36844ff7c0e0d9445bc8e31514d7f0f59cb8b048
run bower --allow-root install ng-file-upload#~11.0
run bower --allow-root install jquery#~2.1
run bower --allow-root install angular-sanitize#~1.4
run bower --allow-root install bootstrap#~3.3
run bower --allow-root install jquery-ui#~1.11
run bower --allow-root install katex#~0.5
run bower --allow-root install ng-table#~0.5
run bower --allow-root install ng-table-export
run bower --allow-root install rangyinputs
run bower --allow-root install ngstorage#~0.3
run bower --allow-root install chartjs#~1.0

# We need to patch the ui-ace module to fix this issue: https://github.com/angular-ui/ui-ace/issues/27
run sed -i 's/var opts = angular\.extend({}, options, scope\.$eval(attrs\.uiAce));/var opts = angular.extend({}, options, scope.$eval(attrs.uiAce, scope));/' /bower_components/angular-ui-ace/ui-ace.js

# Force auto-render extension to throw exceptions instead of printing to console
run sed -i 's/f instanceof katex\.ParseError/false/' /bower_components/KaTeX/dist/contrib/auto-render.min.js

run npm uninstall bower
run apt-get remove -y npm

run apt-get install -y openssh-server
run mkdir /var/run/sshd
run echo 'root:test' | chpasswd
run sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# Configure timezone and locale
run echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata

# Default startup command
cmd cd /service/timApp && source initenv.sh && python3 launch.py
