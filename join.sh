#!/bin/sh

echo > "words_low.asm"

for NAME in `cat "dict_low.asm" | grep ".include \"words/" | awk 'BEGIN { FS="[/\"]" } { print $3 }'`; do
    cat "words/$NAME" >> "words_low.asm"
    echo >> "words_low.asm"
done
