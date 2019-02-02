#!/usr/bin/env bash
set -eu

if which pbpaste &> /dev/null ; then
    pbpaste
elif which powershell.exe &> /dev/null ; then
    powershell.exe -Command "Get-Clipboard" | tr -d '\r\n'
else
    xsel --clipboard --output
fi
