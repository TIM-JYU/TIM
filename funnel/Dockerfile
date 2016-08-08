#
# Dockerfile for funnel (a traffic queuer)
#

FROM ubuntu
MAINTAINER tomi.karppinen@jyu.fi

# Configure timezone and locale
run locale-gen en_US.UTF-8
env LANG en_US.UTF-8
env LANGUAGE en_US:en
env LC_ALL en_US.UTF-8
run echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata

# Install everything we need
RUN apt-get update
RUN apt-get install -y python3
RUN apt-get install -y python3-pip

RUN pip3 install typing

# Expose http port
EXPOSE 80

