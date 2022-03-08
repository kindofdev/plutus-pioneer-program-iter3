#!/bin/bash
restore_wallet_file=$1
port=$2

curl -H "content-type: application/json" -XPOST \
    -d @$restore_wallet_file \
    localhost:$port/v2/wallets
