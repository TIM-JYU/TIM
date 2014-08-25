# Aloitetaan ubuntun imagesta
from ubuntu
maintainer vesal "vesal@jyu.fi"

# Asennetaan python, pip ja flask

run apt-get install -y python3
run apt-get install -y python3-pip
run pip3 install Flask
run pip3 install bleach
run apt-get install -y wget

# Poistetaan pip. Sitä ei tarvitse enää ja saadaan
# hitusen pienempi image ja vähemmän ohjelmia potentiaalisen
# hyökkääjän käyttöön. (Periaate ok. käytännössä tosi marginaalinen
# toimenpide) 
# run apt-get -y --purge remove python3-pip

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

#volume /tim_files/:/service/tim_files/
