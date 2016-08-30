# Aloitetaan ubuntun imagesta
FROM ubuntu
maintainer Pekka Kasa & Joonas Keppo "pealkasa@student.jyu.fi && josakepp@student.jyu.fi"

# Asennetaan python, pip 

run apt-get update
run apt-get install -y python3
run apt-get install -y python3-pip
run pip3 install bleach
run pip3 install xmltodict oauth2 requests_oauthlib  bs4
run apt-get install -y wget 

# Lisätään alkuperäinen hakemisto polkuun /service
run mkdir /service

# Lisätään käyttäjä `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
run useradd -M agent 
run chown -R agent /service

# testejä varten
ENV MYPASSWORD password
RUN echo "root:$MYPASSWORD" | chpasswd

# Varmuudeksi estetään root login ja poistetaan rootin salasana,
# jos sellainen on.
# run usermod -p '!' root
# run passwd -l root

# Asetetaan container käynnistymään tunnuksella agent.
user agent

# Avataan portti 5000 ulos containerista.
expose 5000
