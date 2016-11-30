#!/bin/bash

if [ -f /etc/arch-release ] ; then
  OPTS="-e ansible_python_interpreter=/usr/bin/python2 -e virtualenv_executable=virtualenv2"
fi

DIR="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")" && pwd)"
cd "${DIR}"
ansible-playbook -i 'localhost,' -c local setup.yml -K --ask-vault-pass ${OPTS} "$@"
