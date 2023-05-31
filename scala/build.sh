#!/usr/bin/env bash

mkdir -p build
for app in $(./mill resolve apps._)
do
  ./mill $app.fullLinkJS
done

npm install
npm run build
