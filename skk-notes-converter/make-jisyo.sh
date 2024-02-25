#!/bin/sh

if [ ! -f dic/SKK-JISYO.notes ]; then
    mkdir -p dic/
    curl -L -o dic/SKK-JISYO.notes https://github.com/skk-dev/dict/raw/master/SKK-JISYO.notes

    # いくつか、不正？なのかどうか不明なエントリーがあったので、一旦こちらで補正しておく
    for f in $(/bin/ls ./misc); do
        patch dic/SKK-JISYO.notes < ./misc/$f
    done
fi

cargo build

target/debug/skk-notes-converter dic/SKK-JISYO.notes > dic/chokan.jisyo.dat 2> dic/error.txt
