from nginx

run mkdir /etc/nginx/conf

add plugin.conf /etc/nginx/conf/
add nginx.conf /etc/nginx/
add startup.sh /

arg MAIN_CONFIG=tim.conf

add $MAIN_CONFIG /etc/nginx/sites-enabled/

run rm /etc/nginx/conf.d/default.conf

expose 80
