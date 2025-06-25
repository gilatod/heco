ARG VARIANT=bookworm-slim
FROM debian:${VARIANT}

ARG CONTAINER_USER=haskell
ARG CONTAINER_GROUP=haskell
ENV LANG=C.UTF-8

# Install dependencies
RUN apt-get update \
    && apt-get install -y build-essential ca-certificates curl gcc git gnupg \
        libffi-dev libgmp-dev libncurses-dev libtinfo-dev make pkg-config \
        sqlite3 zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Set users
RUN adduser --disabled-password --gecos "" ${CONTAINER_USER}
USER ${CONTAINER_USER}
WORKDIR /home/${CONTAINER_USER}

# Install ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=${PATH}:/home/${CONTAINER_USER}/.ghcup/bin

CMD [ "/bin/bash" ]
