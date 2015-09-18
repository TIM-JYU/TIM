#!/bin/sh
# A script to install everything and build Ephemeral. Run this in Ephemeral directory.

# Build Ephemeral
docker build --tag="ephemeral" .

# Create folder for logs
mkdir -p dist/build/Ephemeral/log

# Copy the built executable from the container to host
hash=$(docker run -d -t -i ephemeral)
docker cp $hash:/Ephemeral/dist/build/Ephemeral/Ephemeral dist/build/Ephemeral/

