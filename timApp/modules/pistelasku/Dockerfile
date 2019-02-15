FROM ubuntu

RUN apt-get update
RUN apt-get install -y python3
RUN apt-get install -y python3-pip
RUN pip3 install bleach
RUN apt-get install -y wget

RUN mkdir /service

RUN useradd -M agent
RUN chown -R agent /service

ENV MYPASSWORD password

USER agent

EXPOSE 5000
