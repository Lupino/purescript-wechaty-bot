#!/usr/bin/env bash

NAME=${NAME:-"demo"}
SEARCH_HOST=${SEARCH_HOST:-"127.0.0.1:6000"}
PERIODIC_HOST=${PERIODIC_HOST:-"periodicd"}
DB_PATH=${DB_PATH:-"bot.db"}
sed -i "s#config.default.DEFAULT_PROFILE#'$NAME'#" index.js
sed -i "s#127.0.0.1:6000#$SEARCH_HOST#" index.js
sed -i "s#127.0.0.1#$PERIODIC_HOST#" index.js
sed -i "s#bot.db#$DB_PATH#" index.js
node index.js 2> stderr.log
