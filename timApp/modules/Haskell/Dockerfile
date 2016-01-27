# Start from Ubuntu image
from haskell:7.8
maintainer Ville Tirronen "ville.tirronen@jyu.fi"
  
run apt-get update
run apt-get install -y graphviz git
env LANG C.UTF-8
env LC_ALL C.UTF-8

run cabal update

# Set name and email for git.
run git config --global user.email "agent@docker.com"
run git config --global user.name "agent"

expose 5000
