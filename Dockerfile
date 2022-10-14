FROM fpco/stack-build:lts-9 AS build
LABEL maintainer="ville.tirronen@jyu.fi"

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y" \
    APT_CLEANUP="rm -r /var/lib/apt/lists/*"

RUN bash -c "${APT_INSTALL} graphviz && ${APT_CLEANUP}"
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

WORKDIR /build/Choices
COPY LICENSE ./
COPY stack.yaml stack.yaml
COPY Choice.cabal Choice.cabal
COPY PluginConstructionKit/PluginConstructionKit.cabal PluginConstructionKit/PluginConstructionKit.cabal
COPY PluginConstructionKit/LICENSE PluginConstructionKit/LICENSE

RUN stack build --system-ghc --only-dependencies

COPY *.hs ./
COPY PluginConstructionKit/*.hs PluginConstructionKit/

RUN stack build --system-ghc

FROM ubuntu:20.04
ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -r /var/lib/apt/lists/*"

# graphviz is not installed because GraphVizPlugin is disabled. If you enable GraphVizPlugin, install graphviz again.
RUN bash -c "${APT_INSTALL} libgmp10 && ${APT_CLEANUP}"
COPY --from=build /build/Choices/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin/*Plugin /hbin/

WORKDIR /Choices
COPY startAll.sh ./

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

CMD ["./startAll.sh"]