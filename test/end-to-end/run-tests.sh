#!/usr/bin/env bash

expected ()    { echo "    Expected:"; echo -e "\033[93m$*\033[0m" | sed 's/^/\t/'; }
actual ()      { echo "    Actual:";   echo -e "\033[91m$*\033[0m" | sed 's/^/\t/'; }
show_values () { expected "'$expected'"; actual   "'$actual'"; }
fail ()        { echo -e "\033[91mx ${title}\033[0m"; show_values; }
pass ()        { echo -e "\033[92m✓ ${title}\033[0m"; }

show_ouput () {
  local type output
  type="$1"
  output="$2"
  echo "    $type:"
  echo -e "\033[96m${output}\033[0m" | sed 's/^/\t/'
}

assert_equals () {
  local actual expected title
  expected="$1"
  actual="$2"
  title="$3"
  [ "$expected" == "$actual" ] && pass || fail
}

assert () {
  local type expected options output command
  type="$1"
  expected="$2"
  options="$3"
  command="./$binary $options"
  stdout="$($command 2>/tmp/clifor_stderr)"
  stderr="$(cat </tmp/clifor_stderr)"
  case $type in
    ('stderr') output="$stderr" ;;
    (*) output="$stdout" ;;
  esac
  assert_equals "$expected" "$output" "$command"
  [ -n "$verbose" ] && (show_ouput 'stdout' "'$stdout'"; show_ouput 'stderr' "'$stderr'") || echo -n ''
}

binary='example'
verbose='y'

echo -e "Testing $binary\n"
assert 'stdout' '0.1.0' '-v'
# assert 'stdout' '0.1.0' '--version'
# assert 'stdout' '' ''
# assert 'stderr' '[ ERROR ] invalid option: -' '-'
# assert 'stderr' '[ ERROR ] invalid option: i' 'i'
# assert 'stdout' 'help' '-h'
# assert 'stdout' 'help' '--help'
# assert 'stderr' 'example' '--input-file -b -c -i -j -zap -o --cla - -- -v'










# inotifywait --quiet --recursive --monitor --excludei '(build|data|example.bin)' --event close_write . ../../source | while read -r line; do clear; make; done
