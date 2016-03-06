#
# Dockerfile for funnel (a traffic queuer)
#

FROM ubuntu
MAINTAINER tomi.karppinen@jyu.fi

# Set locale
RUN locale-gen en_US.utf8
RUN dpkg-reconfigure locales
ENV LC_ALL en_US.utf8

# Install everything we need
RUN apt-get update
RUN apt-get install -y python3
RUN apt-get install -y python3-pip

RUN pip3 install typing

# Expose http port
EXPOSE 80

