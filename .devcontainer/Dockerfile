FROM haskell:slim-bookworm

ARG CONTAINER_USER=haskell
ARG CONTAINER_GROUP=haskell

# Set users
RUN adduser --disabled-password --gecos "" ${CONTAINER_USER}
USER ${CONTAINER_USER}
