# Start from Ubuntu image
from ubuntu
maintainer Ville Tirronen "ville.tirronen@jyu.fi"

run apt-get update
run apt-get install -y ghc
run apt-get install -y cabal-install
run apt-get install -y zlib1g-dev
run apt-get install -y libgmp10
env LANG en_US.UTF-8

run cabal update
run cabal install happy alex
run cp /root/.cabal/bin/happy /usr/bin
run cp /root/.cabal/bin/alex /usr/bin

add Ephemeral.cabal /Ephemeral/

run cd /Ephemeral && cabal install --only-dependencies

add EphemeralPrelude.hs /Ephemeral/
add Setup.hs /Ephemeral/
add Simplistic.hs /Ephemeral/

run cd /Ephemeral && cabal configure
run cd /Ephemeral && cabal build

# When the container starts up, execute:
cmd /bin/bash
