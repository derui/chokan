#!/bin/sh

if [ ! -f /tmp/SKK-JISYO.notes ]; then
    curl -L -o /tmp/SKK-JISYO.notes https://github.com/skk-dev/dict/raw/master/SKK-JISYO.notes
fi

cargo build

target/debug/skk-notes-converter /tmp/SKK-JISYO.notes > chokan.jisyo.dat
