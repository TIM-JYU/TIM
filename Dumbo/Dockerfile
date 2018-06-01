ARG TIM_IMAGE_TAG

FROM fpco/stack-build:lts-11.7 AS build
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

# RUN stack solver --system-ghc --update-config
RUN stack build --only-dependencies

COPY *.hs ./

RUN stack build

FROM timimages/tim:${TIM_IMAGE_TAG}
ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -r /var/lib/apt/lists/*"

COPY --from=build /build/.stack-work/install/x86_64-linux/lts-11.7/8.2.2/bin/Dumbo /Dumbo/
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8
