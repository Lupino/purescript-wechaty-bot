FROM node:10

MAINTAINER Li Meng Jun

RUN npm install -g bower pulp

RUN cd /opt \
    && wget https://github.com/purescript/purescript/releases/download/v0.12.2/linux64.tar.gz \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz

ENV PATH /opt/purescript:$PATH

COPY bower.json /app/bower.json
COPY src /app/src

USER root

WORKDIR /app
RUN bower install --allow-root
RUN pulp build --to index.js

FROM buildkite/puppeteer:v1.11.0
MAINTAINER Li Meng Jun "lmjubuntu@gmail.com"

env PUPPETEER_SKIP_CHROMIUM_DOWNLOAD TRUE

COPY image/* /
RUN npm install
COPY  --from=0 /app/index.js /index.js
CMD ["/start.sh"]
