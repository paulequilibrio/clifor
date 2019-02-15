#!/usr/bin/env bash

ostype=$(uname -s)

case "$ostype" in
  ("Linux")
    inotifywait --quiet --recursive --monitor --excludei '(build|data|example$)' \
    --event close_write . ../../source | while read -r line; do
      clear
      make
    done
  ;;
  ("Darwin")
    fswatch --recursive --exclude '(build|data|example$)' . ../source | (while read; do
      # clear
      echo -e '====================================================================\n'
      make
    done)
  ;;
esac
