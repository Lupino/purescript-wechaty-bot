FROM alekzonder/puppeteer:1
MAINTAINER Li Meng Jun "lmjubuntu@gmail.com"

COPY package-build.json /app/package.json
COPY src /app/src

RUN npm install
