#!/bin/bash
#
# Script to update relative links
#
# Intended to be 'idempotent': running the script again on an already-fixed
# tree should have no effect

DIR="$1"

files=$(find "$DIR" -name "*.md")

sed -i='' "s/[^\/]\(\.\.\/\.\.\/\.\.\/project\)/..\/..\/\1/" $files
sed -i='' "s/[^\/]\(\.\.\/\.\.\/\.\.\/akka-\)/..\/..\/\1/" $files
sed -i='' "s/[^\/]\(\.\.\/\.\.\/\.\.\/\.\.\/akka-\)/..\/..\/\1/" $files
sed -i='' "s/[^\/]\.\.\/\.\.\/src\/main\/protobuf/..\/..\/protobuf/" $files
