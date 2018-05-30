FROM purescript:0.12.0
MAINTAINER Li Meng Jun "lmjubuntu@gmail.com"

COPY bower.json /app/bower.json
COPY src /app/src

USER root

WORKDIR /app
RUN bower install --allow-root
RUN pulp build --to index.js
