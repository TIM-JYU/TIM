FROM ubuntu:14.04
MAINTAINER jerome.petazzoni@docker.com

# Let's start with some basic stuff.
RUN apt-get update -qq && apt-get install -qqy \
    apt-transport-https \
    ca-certificates \
    curl \
    lxc \
    iptables
    
# Install Docker from Docker Inc. repositories.
RUN curl -sSL https://get.docker.com/ubuntu/ | sh


# Install the magic wrapper.
ADD ./wrapdocker /usr/local/bin/wrapdocker
RUN chmod +x /usr/local/bin/wrapdocker

# Define additional metadata for our image.
VOLUME /var/lib/docker
CMD ["wrapdocker"]  

# Tähän loppuu Docker in docker -jutut

run apt-get update
run apt-get install -y python3
run apt-get install -y python3-pip
run pip3 install bleach
run apt-get install -y imagemagick

RUN apt-get install -y libxtst6
RUN apt-get install -y inotify-tools


run locale-gen en_US.UTF-8 
run locale-gen fi_FI.UTF-8 

# Configure timezone and locale
RUN echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata
env LC_CTYPE en_US.UTF-8
env LC_ALL en_US.UTF-8
env LANG en_US.UTF-8
env LANGUAGE en_US.UTF-8

# Avataan portti 5000 ulos containerista.
expose 5000



