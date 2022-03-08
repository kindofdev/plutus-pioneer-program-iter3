#!/bin/bash
add=$1

cardano-cli query utxo \
   $MAGIC \
   --address $add
