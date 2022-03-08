#!/bin/bash
wid=$1
port=$2

curl -H "content-type: application/json" \
    -XGET localhost:$port/v2/wallets/$wid/addresses | jq '.'

