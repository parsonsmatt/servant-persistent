FROM haskell@sha256:b8349afbd4be548903521e2e3a987d530999c4e8d3c3fc1c81401a57631eeefb

RUN curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
 && echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch"-pgdg main | tee  /etc/apt/sources.list.d/pgdg.list

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    postgresql-client-11 \
    libpq-dev \
 && rm -rf /var/lib/apt/lists/*
