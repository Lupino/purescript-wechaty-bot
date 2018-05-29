#!/usr/bin/env bash

NAME=${NAME:-"demo"}
SEARCH_HOST=${SEARCH_HOST:-"127.0.0.1:6000"}
sed -i "s#config.default.DEFAULT_PROFILE#'$NAME'#" index.js
sed -i "s#127.0.0.1:6000#$SEARCH_HOST#" index.js
node index.js
