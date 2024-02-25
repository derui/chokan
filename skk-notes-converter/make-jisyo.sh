#!/bin/sh

if [ ! -f /tmp/SKK-JISYO.notes ]; then
    mkdir -p dic/
    curl -L -o dic/SKK-JISYO.notes https://github.com/skk-dev/dict/raw/master/SKK-JISYO.notes

    # いくつか、不正？なのかどうか不明なエントリーがあったので、一旦こちらで補正しておく
    for f in $(/bin/ls ./misc); do
        patch < ./misc/$f
    done
fi

cargo build

target/debug/skk-notes-converter /tmp/SKK-JISYO.notes > chokan.jisyo.dat 2> error.txt
