FROM alekzonder/puppeteer:1.8.0-0
MAINTAINER Li Meng Jun "lmjubuntu@gmail.com"

COPY package.json /app

RUN npm install

COPY index.js /app
COPY js /app/js
COPY start.sh /app

CMD ["./start.sh"]
