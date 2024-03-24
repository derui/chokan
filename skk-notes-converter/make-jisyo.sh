#!/bin/sh

mkdir -p work/
if [ ! -f work/SKK-JISYO.notes ]; then
    mkdir -p work/
    curl -L -o work/SKK-JISYO.notes https://github.com/skk-dev/dict/raw/master/SKK-JISYO.notes

    # いくつか、不正？なのかどうか不明なエントリーがあったので、一旦こちらで補正しておく
    for f in $(/bin/ls ./misc); do
        patch work/SKK-JISYO.notes < ./misc/$f
    done
fi

cargo build

cargo run --bin skk-notes-converter -- work/SKK-JISYO.notes > dic/chokan.notes.dic 2> dic/error.txt
