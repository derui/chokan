#!/bin/sh

if [ ! -f dic/SKK-JISYO.notes ]; then
    echo "Can not make patch"
    exit 1
fi

rm -f dic/original.current
cp dic/SKK-JISYO.notes dic/original.current

# make patch
next_patch=$(ls misc/ | cut -d'-' -f 1 | awk '{gsub("^0+", "", $1); print $1}' | sort -n | tail -n 1)
next_patch=$(expr $next_patch + 1)

while read line; do
    line=${line// /-}
    echo "make patch $line at $next_patch"
    diff -p dic/original.current dic/SKK-JISYO.notes > misc/$(printf "%03d" $next_patch)-$line.patch
    cp dic/original.current dic/original.$(printf "%03d" $next_patch) 
    cp dic/SKK-JISYO.notes dic/original.current
    next_patch=$(expr $next_patch + 1)
done
