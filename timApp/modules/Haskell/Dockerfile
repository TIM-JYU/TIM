FROM fpco/stack-build:lts-9
LABEL maintainer="ville.tirronen@jyu.fi"

RUN apt-get update
RUN apt-get install -y graphviz git
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

# Set name and email for git.
RUN git config --global user.email "agent@docker.com"
RUN git config --global user.name "agent"

EXPOSE 5000
