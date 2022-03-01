cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 01.addr) \
    --tx-in ec9a9c508a7c0470ccdb0b01bb450aa65a351bb9fa60fd42d56a9d39e2fdb75b#0 \
    --tx-out "addr_test1qpk25cs26c34hnl4emks4vlxwjplz3l7e5qtx2qex4cw9ym0dmmclkraf3adjk5uwsdm28hsr5544xd9d8efncegnhysl80vwk 50000000 lovelace" \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
