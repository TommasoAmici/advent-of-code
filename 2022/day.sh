#!/bin/sh
_help() {
  printf "Sets up working environment for each new day challenge.
Loads session cookie from .env file.

USAGE:
  day.sh <DAY>

OPTIONS:
  -h  Print help information
"
}

while getopts "h" option; do
  case $option in
  h | *) _help && return ;;
  esac
done

DAY=$1
YEAR=2022
NAME=aoc-$(printf %02d "$DAY")

cargo new --vcs none --bin "$NAME"
cp template.rs "$NAME/src/main.rs"

cat <<EOF >"$NAME/README.md"
# Day ${DAY}
EOF

# Get data
mkdir -p "$NAME/data"

# read cookie from .env file
set -o allexport
. "./.env"
set +o allexport

curl -H "Cookie: session=${COOKIE}" "https://adventofcode.com/${YEAR}/day/${DAY}/input" >"$NAME/data/input.txt"
