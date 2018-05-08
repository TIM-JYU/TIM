FROM fpco/stack-build:lts-11.7 as build
LABEL maintainer="ville.tirronen@jyu.fi"

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -r /var/lib/apt/lists/*"

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN stack update

WORKDIR /build
COPY LICENSE ./
COPY stack.yaml ./
COPY Dumbo.cabal ./
COPY AsciiMath ./AsciiMath
COPY *.hs ./

RUN stack build --only-dependencies

RUN stack build 

FROM ubuntu:17.04
ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -r /var/lib/apt/lists/*"

RUN bash -c "${APT_INSTALL} libgmp10 && ${APT_CLEANUP}"
COPY --from=build /build/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin/Dumbo /Dumbo/
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8
