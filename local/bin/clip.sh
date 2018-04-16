#!/usr/bin/env bash
set -eu

if which pbcopy &> /dev/null ; then
    pbcopy
elif which clip.exe &>/dev/null ; then
    clip.exe
else
    xsel --clipboard --input
fi
