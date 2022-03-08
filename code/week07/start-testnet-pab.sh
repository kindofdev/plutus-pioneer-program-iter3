#!/bin/bash
pabConfigFile=$1
pass=$2

cabal run -- rps-pab \
  --config $pabConfigFile webserver \
  --passphrase $pass
  

