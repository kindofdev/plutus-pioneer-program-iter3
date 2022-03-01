#!/bin/bash

curl -H "content-type: application/json" -XPOST \
    -d @testnet/restore_wtest.json \
    localhost:8090/v2/wallets
