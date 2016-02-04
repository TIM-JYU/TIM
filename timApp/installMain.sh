
# Set locale
env LANG en_US.utf8
env LANGUAGE en_US.utf8
run locale-gen en_US.utf8

# Install Python, pip and other necessary packages

apt-get update
apt-get install -y python3
apt-get install -y python3-pip
apt-get install -y git-core

apt-get install -y zlib1g-dev # lxml dependency
apt-get install -y libxml2-dev libxslt-dev python3-dev # lxml dependency
apt-get install -y libyaml-dev # C-parser for PyYAML


pip3 install flask
pip3 install flask-compress
pip3 install flask-cache
pip3 install beautifulsoup4
pip3 install pycontracts
pip3 install hypothesis
pip3 install gitpylib
pip3 install lxml
pip3 install pyaml
pip3 install ansi2html
pip3 install cssutils
pip3 install mmh3

apt-get install -y wget
apt-get update
apt-get install -y cmake
wget https://github.com/libgit2/libgit2/archive/v0.23.1.tar.gz && tar xzf v0.23.1.tar.gz
cd libgit2-0.23.1/ && cmake -DPYTHON_EXECUTABLE=/usr/bin/python3 . && make install

apt-get install -y libffi-dev
env LD_LIBRARY_PATH /usr/local/lib
pip3 install pygit2

pip3 install requests --upgrade

git config --global user.email "agent@docker.com"
git config --global user.name "agent"

run mkdir /service

# Add user `agent` -- we don't want to run anything as root.
useradd -M agent
chown -R agent /service

# expose 5000
# expose 22

apt-get install -y npm
npm install -g bower
ln -s /usr/bin/nodejs /usr/bin/node
bower --allow-root install angular#~1.3
bower --allow-root install angular-ui-ace#36844ff7c0e0d9445bc8e31514d7f0f59cb8b048
bower --allow-root install ng-file-upload#~3.0
bower --allow-root install jquery#~2.1
bower --allow-root install angular-sanitize#~1.3
bower --allow-root install bootstrap#~3.3
bower --allow-root install waypoints#~3.1
bower --allow-root install jquery-ui#~1.11
bower --allow-root install katex-build#~0.2
bower --allow-root install ng-table#~0.5
bower --allow-root install ng-table-export
bower --allow-root install rangyinputs
bower --allow-root install ngstorage#~0.3

# We need to patch the ui-ace module to fix this issue: https://github.com/angular-ui/ui-ace/issues/27
sed -i 's/var opts = angular\.extend({}, options, scope\.$eval(attrs\.uiAce));/var opts = angular.extend({}, options, scope.$eval(attrs.uiAce, scope));/' /bower_components/angular-ui-ace/ui-ace.js

#run npm uninstall bower
#run apt-get remove -y npm

apt-get install -y openssh-server
# tämä ei ehkä ole turvallista pääkoneessa?
# mkdir /var/run/sshd
# echo 'root:test' | chpasswd
# sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# run apt-get autoremove -y

# Configure timezone and locale
run echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata

# Default startup command
# cmd cd /service/timApp && source initenv.sh && python3 launch.py
