#!/bin/bash

mixer=$(amixer get Master | grep 'Front Left:')

muted=$(amixer get Master | grep 'Front Left:' | cut -d ' ' -f 9)
if [ "$muted" == "[off]" ]; then
    echo "[x]"
else
    echo $mixer | cut -d ' ' -f 5 
fi

