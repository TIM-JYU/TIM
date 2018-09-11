FROM ubuntu:18.04
LABEL maintainer="vesal@jyu.fi"

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -rf /var/lib/apt/lists /usr/share/doc ~/.cache /var/cache/oracle-* /var/cache/apk /tmp/*"

# Timezone configuration
RUN bash -c "${APT_INSTALL} locales tzdata apt-utils acl unzip software-properties-common && ${APT_CLEANUP}"
RUN locale-gen en_US.UTF-8 && bash -c "${APT_CLEANUP}"
RUN locale-gen fi_FI.UTF-8 && bash -c "${APT_CLEANUP}"
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en \
    LC_ALL=en_US.UTF-8 \
    LC_CTYPE=en_US.UTF-8
RUN echo "Europe/Helsinki" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata && bash -c "${APT_CLEANUP}"

RUN bash -c "${APT_INSTALL} python3 python3-pip python3-setuptools wget dirmngr gpg-agent && ${APT_CLEANUP}"
RUN pip3 install wheel && bash -c "${APT_CLEANUP}"
RUN pip3 install Flask bleach && bash -c "${APT_CLEANUP}"

# Keep above same as CS Dockerfile for better caching

RUN bash -c "${APT_INSTALL} curl && ${APT_CLEANUP}"

RUN useradd -M agent

USER agent

EXPOSE 5000
