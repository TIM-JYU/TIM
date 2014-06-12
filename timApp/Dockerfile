# Aloitetaan ubuntun imagesta
from ubuntu
maintainer Ville Tirronen "ville.tirronen@jyu.fi"

# Asennetaan python, pip ja flask

run apt-get install -y python3
run apt-get install -y python3-pip
run pip3 install Flask
run pip3 install markdown
run pip3 install beautifulsoup4
run pip3 install pycontracts
run pip3 install hypothesis

# Poistetaan pip. Sitä ei tarvitse enää ja saadaan
# hitusen pienempi image ja vähemmän ohjelmia potentiaalisen
# hyökkääjän käyttöön. (Periaate ok. käytännössä tosi marginaalinen
# toimenpide)
run apt-get -y --purge remove python3-pip

# Lisätään alkuperäinen hakemisto polkuun /service
add . /service

# Lisätään käyttäjä `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
run useradd -M agent
run chown -R agent /service

# Varmuudeksi estetään root login ja poistetaan rootin salasana,
# jos sellainen on.
run usermod -p '!' root
run passwd -l root

# Asetetaan container käynnistymään tunnuksella agent.
user agent

# Avataan portti 5000 ulos containerista.
expose 5000

# Kun container käynnistyy, suoritetaan:
cmd cd /service/ && python3 tim.py
