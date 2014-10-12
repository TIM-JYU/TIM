#!/bin/sh
# A script to install everything and build Ephemeral. Run this in Ephemeral directory.

# Build Ephemeral
docker build --tag="ephemeral" .

# Copy the built executable from the container to host
hash=$(docker run -d -t -i ephemeral)
docker cp $hash:/Ephemeral/dist/build/Ephemeral/Ephemeral dist/build/Ephemeral/

# Create folder for logs
mkdir dist/build/Ephemeral/log

echo "To launch Ephemeral, run"
echo "./dist/build/Ephemeral/Ephemeral -p 8001 &"
