#!/bin/sh

if [ ! -f work/SKK-JISYO.notes ]; then
    echo "Can not make patch"
    exit 1
fi

rm -f work/original.current
cp work/SKK-JISYO.notes work/original.current

# make patch
next_patch=$(ls misc/ | cut -d'-' -f 1 | awk '{gsub("^0+", "", $1); print $1}' | sort -n | tail -n 1)
next_patch=$(expr $next_patch + 1)

while read line; do
    line=${line// /-}
    echo "make patch $line at $next_patch"
    diff -p work/original.current work/SKK-JISYO.notes > misc/$(printf "%03d" $next_patch)-$line.patch
    cp work/original.current work/original.$(printf "%03d" $next_patch) 
    cp work/SKK-JISYO.notes work/original.current
    next_patch=$(expr $next_patch + 1)
done
