#!/bin/bash

playDL=$1
revealDL=$2
choice1=$3
choice2=$4
stake=$5
skipPlayer2=$6
echo "the game starts"

# [wid1', wid2', addr1', addr2', playDL', revealDL', choice1', choice2', stake', skipPlayer2', p1', p2']
cabal run game -- $WALLETID1 $WALLETID2 $ADDRESS1 $ADDRESS2 $playDL $revealDL $choice1 $choice2 $stake $skipPlayer2 9080 9082
