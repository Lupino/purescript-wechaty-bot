#!/usr/bin/env bash

docker build -t bot-build .

docker rm bot-build

docker run -i -t --name bot-build bot-build ls

docker cp bot-build:/app/index.js image
cp -av js image

cd image

docker build -t bot .
