FROM haskell:7.10

WORKDIR /app

RUN apt-get update && \
    apt-get install -y --force-yes wget && \
    wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/debian/fpco.key | apt-key add - && \
    echo 'deb http://download.fpcomplete.com/debian/jessie stable main'| tee /etc/apt/sources.list.d/fpco.list && \
    apt-get update && \
    apt-get install -y --force-yes stack

ENV PATH /root/.local/bin:$PATH

COPY LICENSE Setup.hs postgresql-connector.cabal stack.yaml /app/
RUN stack build --only-dependencies

COPY main /app/main
COPY src /app/src
RUN stack build --copy-bins


