# Aloitetaan ubuntun imagesta
from ubuntu
maintainer Ville Tirronen "ville.tirronen@jyu.fi"

#run apt-get update
run apt-get install -y ghc
run apt-get install -y cabal-install
run apt-get install -y zlib1g-dev
run apt-get install -y libgmp10
env LANG en_US.UTF-8

run cabal update
run cabal install happy alex
run cp .cabal/bin/happy /usr/bin
run cp .cabal/bin/alex /usr/bin

add . /Ephemeral-build
run cd /Ephemeral-build && cabal install --only-dependencies
run cd /Ephemeral-build && cabal build

# Lisätään käyttäjä `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
#run chown -R dev /service

# Varmuudeksi estetään root login ja poistetaan rootin salasana,
# jos sellainen on.
#run usermod -p '!' root
#run passwd -l root

# Avataan portti 5000 ulos containerista.
expose 5000

# Kun container käynnistyy, suoritetaan:
cmd /bin/bash
