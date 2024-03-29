#!/bin/sh

if [ ! -f work/SKK-JISYO.M ]; then
    mkdir -p work/
    curl -L -o work/SKK-JISYO.S https://github.com/skk-dev/dict/raw/master/SKK-JISYO.M
fi

cargo build

mkdir -p dic
cargo run --bin skk-noun-converter -- \
      work/SKK-JISYO.M > dic/chokan.noun.dic
