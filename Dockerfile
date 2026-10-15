FROM debian:13-slim AS build

COPY . /pla-util
RUN apt-get update && \
    apt-get install -y gprbuild gnat git libpcap-dev && \
    cd pla-util && \
    gprbuild -p -P pla_util.gpr

FROM debian:13-slim

# Install all dependencies for running `pla-util`, and net-tools to have access to `ifconfig`.
RUN apt-get update && apt-get install -y \
    libgnat-14 \
    libgpg-error0 \
    liblz4-1 \
    libzstd1 \
    liblzma5 \
    libgcrypt20 \
    libcap2 \
    libsystemd0 \
    libpcap0.8t64 \
    libdbus-1-3  \
    libgcc-s1 \
    net-tools

COPY --from=build /pla-util/bin/pla-util /bin
