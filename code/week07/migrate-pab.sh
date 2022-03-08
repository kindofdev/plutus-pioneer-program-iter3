#!/bin/bash
# cabal run -- rps-pab \
#   --config testnet/pab-config.yml migrate

pabConfigFile=$1

cabal run -- rps-pab \
  --config $pabConfigFile migrate
