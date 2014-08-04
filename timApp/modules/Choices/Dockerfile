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
run cabal install --only-dependencies
run cabal configure

# Avataan portti 5000 ulos containerista.
expose 5000

# Kun container käynnistyy, suoritetaan:
cmd /bin/bash
