FROM ubuntu:18.04

RUN apt-get update \
 && apt-get install -y \
    libpq-dev \
    libc6 \
 && apt-get clean

WORKDIR /app

ADD dist /app
ADD assets /app/assets

RUN groupadd -r appuser && useradd -r -m -g appuser appuser

RUN chown -R appuser:appuser /app

USER appuser

CMD echo 'Starting..' && ./perservant
