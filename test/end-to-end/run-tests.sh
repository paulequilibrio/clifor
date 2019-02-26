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
  options="$2"
  expected="$3"
  command="./$binary $options"
  stderr="$($command 2>&1 >/tmp/clifor_stdout)"
  stdout="$(cat /tmp/clifor_stdout | tr -d \\0)"
  case $type in
    ('stderr') output="$stderr" ;;
    (*) output="$stdout" ;;
  esac
  assert_equals "$expected" "$output" "$command"
  [ -n "$verbose" ] && (show_ouput 'stdout' "'$stdout'"; show_ouput 'stderr' "'$stderr'") || echo -n ''
}

binary='example'
echo -e "Testing $binary\n"

assert 'stderr' '' '[ ERROR ] Missing required option: -i (--input-file)'
assert 'stderr' '-' '[ ERROR ] Unknow option: -'
assert 'stderr' 'i' '[ ERROR ] Unknow option: i'
assert 'stderr' '-i' '[ ERROR ] Missing required value for option: -i <FILEPATH>'
assert 'stderr' '-i a' '[ ERROR ] Missing required option: -o (--output-file)'
assert 'stdout' '' ''
assert 'stdout' '--version' '0.1.0'
assert 'stdout' '-v' '0.1.0'
assert 'stderr' '--help' ''
assert 'stderr' '-h' ''
assert 'stdout' '-i a -o b --quiet' ''
assert 'stdout' '-i a -o b' 'input: a, output: b'
verbose='y'










# inotifywait --quiet --recursive --monitor --excludei '(build|data|example.bin)' --event close_write . ../../source | while read -r line; do clear; make; done
