# Aloitetaan ubuntun imagesta
from ubuntu
maintainer Ville Tirronen "ville.tirronen@jyu.fi"

# Asennetaan Python, pip ja muut tarvittavat packaget

run apt-get install -y python3
run apt-get install -y python3-pip
run apt-get install -y git-core
run apt-get install -y zlib1g-dev # lxml tarvitsee
run apt-get install -y libxml2-dev libxslt-dev python3-dev # lxml tarvitsee

run pip3 install Flask
run pip3 install flask-compress
run pip3 install markdown
run pip3 install beautifulsoup4
run pip3 install pycontracts
run pip3 install hypothesis
run pip3 install gitpylib
run pip3 install requests
run pip3 install lxml
run pip3 install pyaml
run pip3 install ansiconv

# Poistetaan pip. Sitä ei tarvitse enää ja saadaan
# hitusen pienempi image ja vähemmän ohjelmia potentiaalisen
# hyökkääjän käyttöön. (Periaate ok. käytännössä tosi marginaalinen
# toimenpide)
run apt-get -y --purge remove python3-pip

# Asetetaan nimi ja email gittiä varten.
run git config --global user.email "agent@docker.com"
run git config --global user.name "agent"

# Lisätään alkuperäinen hakemisto polkuun /service
run mkdir /service

# Lisätään käyttäjä `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
run useradd -M agent
run chown -R agent /service

# Varmuudeksi estetään root login ja poistetaan rootin salasana,
# jos sellainen on.
##run usermod -p '!' root
##run passwd -l root
##
### Asetetaan container käynnistymään tunnuksella agent.
##user agent

# Avataan portti 5000 ulos containerista.
expose 5000

# Kun container käynnistyy, suoritetaan:
cmd cd /service/ && source initenv.sh && python3 initdb2.py && python3 tim.py

#volume /tim_files/:/service/tim_files/
