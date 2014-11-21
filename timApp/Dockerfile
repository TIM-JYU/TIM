# Start from the Ubuntu image
from ubuntu
maintainer Ville Tirronen "ville.tirronen@jyu.fi"

# Set locale
env LANG en_US.utf8
env LANGUAGE en_US.utf8
run env-update en_US.utf8

# Install Python, pip and other necessary packages

run apt-get update
run apt-get install -y python3
run apt-get install -y python3-pip
run apt-get install -y git-core

run apt-get install -y zlib1g-dev # lxml dependency
run apt-get install -y libxml2-dev libxslt-dev python3-dev # lxml dependency
run apt-get install -y libyaml-dev # C-parser for PyYAML

run pip3 install flask
run pip3 install flask-compress
run pip3 install beautifulsoup4
run pip3 install pycontracts
run pip3 install hypothesis
run pip3 install gitpylib
run pip3 install lxml
run pip3 install pyaml
run pip3 install ansiconv
run pip3 install cssutils

run apt-get install -y wget
run apt-get install -y cmake

# Create symbolic link for python3; otherwise CMake won't find it.
run ln -s /usr/bin/python3 /usr/bin/python

# Install pygit2 and dependencies
run apt-get install libffi-dev
run apt-get install libgit2-dev=0.19.0-2
#env LD_LIBRARY_PATH /usr/local/lib
run pip3 install pygit2==0.19

# requests must be the last package to be installed!
# After this, pip3 stops working because it depends
# on older version of requests package.
run pip3 install requests --upgrade

# Remove pip. It's no longer needed, we get slightly smaller image and less programs
# for a potential attacker to use.
run apt-get -y --purge remove python3-pip

# Set name and email for git.
run git config --global user.email "agent@docker.com"
run git config --global user.name "agent"

run mkdir /service

# Add user `agent` -- we don't want to run anything as root.
run useradd -M agent
run chown -R agent /service

expose 5000

# Kun container käynnistyy, suoritetaan:
cmd cd /service/timApp && source initenv.sh && python3 launch.py
