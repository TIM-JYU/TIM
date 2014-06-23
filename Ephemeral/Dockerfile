# Aloitetaan ubuntun imagesta
from ubuntu
maintainer Ville Tirronen "ville.tirronen@jyu.fi"

# Asennetaan python, pip ja flask
run apt-get update
run apt-get install -y ghc
run apt-get install -y cabal-install
run apt-get install -y zlib1g-dev
run apt-get install -y libgmp10
env LANG en_US.UTF-8

run cabal update
run cabal install happy alex
run cp .cabal/bin/happy /usr/bin
run cp .cabal/bin/alex /usr/bin
run cabal install snap-core snap-server pandoc-1.12.3.3 aeson Diff Diff3 
# Lis‰t‰‰n k‰ytt‰j‰ `agent` -- Emme halua ajaa containerissakaan
# palveluja roottina.
#run chown -R dev /service

# Varmuudeksi estet‰‰n root login ja poistetaan rootin salasana,
# jos sellainen on.
#run usermod -p '!' root
#run passwd -l root

# Avataan portti 5000 ulos containerista.
expose 5000

# Kun container k‰ynnistyy, suoritetaan:
cmd /bin/bash
