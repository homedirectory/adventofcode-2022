#!/usr/bin/env bash

print_usage() {
    echo "usage: $0 file.ml"
}

[[ $# -lt 1 ]] && {
    print_usage
    exit 1
}

dune utop ../lib/ -- -init <(echo "#use \"$1\"")
