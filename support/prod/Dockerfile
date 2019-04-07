FROM debian:stretch@@sha256:724b0fbbda7fda6372ffed586670573c59e07a48c86d606bab05db118abe0ef5

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    libpq-dev \
 && rm -rf /var/lib/apt/lists/*

   #  libc6 \

WORKDIR /app

ADD dist /app
ADD assets /app/assets

RUN groupadd -r appuser && useradd -r -m -g appuser appuser

RUN chown -R appuser:appuser /app

USER appuser

CMD echo 'Starting..' && ./perservant
