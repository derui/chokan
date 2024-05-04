#!/bin/sh

if [ ! -f work/SKK-JISYO.jinmei ]; then
    mkdir -p work/
    curl -L -o work/SKK-JISYO.jinmei https://github.com/skk-dev/dict/raw/master/SKK-JISYO.jinmei
fi

mkdir -p dic
cargo run --bin skk-jinmei-converter -- \
      work/SKK-JISYO.jinmei > dic/chokan.jinmei.dic
