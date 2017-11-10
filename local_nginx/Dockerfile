FROM nginx

RUN mkdir /etc/nginx/conf

ADD plugin.conf /etc/nginx/conf/
ADD nginx.conf /etc/nginx/
ADD tim_common.conf /etc/nginx/

ARG MAIN_CONFIG=tim.conf

ADD $MAIN_CONFIG /etc/nginx/sites-enabled/

RUN rm /etc/nginx/conf.d/default.conf

EXPOSE 80
EXPOSE 81
EXPOSE 82
