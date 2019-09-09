#!/usr/bin/env bash
set -eu

if which pbpaste &> /dev/null ; then
    pbpaste
elif which powershell.exe &> /dev/null ; then
    dos2unixCmd=$(which dos2unix.exe 2>/dev/null || echo "dos2unix" )
    powershell.exe -Command "Get-Clipboard" | "$dos2unixCmd"
else
    xsel --clipboard --output
fi
