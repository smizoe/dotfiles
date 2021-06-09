#!/usr/bin/env bash
set -eu

resource="$(cat -)"

if which open &> /dev/null ; then
    open "${resource}" &> /dev/null
elif which powershell.exe &> /dev/null ; then
    powershell.exe -Command "${resource}" &> /dev/null
else
    xdg-open "${resource}" &> /dev/null
fi
