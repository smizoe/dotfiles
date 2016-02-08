#!/bin/bash

if [ -f /etc/arch-release ] ; then
  OPTS="-e ansible_python_interpreter=/usr/bin/python2 -e virtualenv_executable=virtualenv2"
fi
ansible-playbook -i '127.0.0.1,' -c local setup.yml -K --ask-vault-pass ${OPTS} "$@"
