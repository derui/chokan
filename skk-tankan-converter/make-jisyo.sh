#!/bin/sh

if [ ! -f work/SKK-JISYO.S ]; then
    mkdir -p work/
    curl -L -o work/SKK-JISYO.S https://github.com/skk-dev/dict/raw/master/SKK-JISYO.S
fi

cargo build

mkdir -p dic
cargo run --bin skk-tankan-converter -- \
      work/SKK-JISYO.S > dic/chokan.tankan.dic
