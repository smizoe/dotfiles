#!/bin/bash

DIR="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")" && pwd)"
cd "${DIR}"
ansible-playbook -i 'localhost,' -c local setup.yml -K --ask-vault-pass ${OPTS} "$@"
