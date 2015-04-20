# Aloitetaan ubuntun imagesta
from ubuntu
maintainer vesal "vesal@jyu.fi"

# Asennetaan python, pip 

run apt-get update
run apt-get install -y python3
run apt-get install -y python3-pip
run apt-get install -y wget

# Lis‰t‰‰n alkuper‰inen hakemisto polkuun /service
run mkdir /service

# Lis‰t‰‰n k‰ytt‰j‰ `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
run useradd -M agent 
run chown -R agent /service

# testej‰ varten
ENV MYPASSWORD password
RUN echo "root:$MYPASSWORD" | chpasswd

# Varmuudeksi estet‰‰n root login ja poistetaan rootin salasana,
# jos sellainen on.
# run usermod -p '!' root
# run passwd -l root

# Asetetaan container k‰ynnistym‰‰n tunnuksella agent.
user agent

# Avataan portti 5000 ulos containerista.
expose 5000
