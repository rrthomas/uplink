FROM haskell:8.0.2

WORKDIR /usr/src/app
COPY ./stack.yaml .
COPY ./package.yaml .

RUN apt-get update && apt-get install -y libleveldb-dev leveldb-doc openssh-client expat libexpat1-dev libpq-dev
RUN stack install --no-docker --only-dependencies -j2 --system-ghc
COPY . .
RUN stack install --no-docker --system-ghc
# delete .git after building uplink for a smaller image
RUN rm -rf .git/
