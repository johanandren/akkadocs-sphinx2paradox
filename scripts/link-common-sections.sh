#!/bin/sh

# This script is *not* idempotent, only to be used after a fresh migration

# src/main/paradox:
DIR="$1"

cd $DIR

files=$(find "$DIR" -name "*.md" | grep -v "common/may-change" | grep -v "dev/multi" | grep -v "additional" | grep -v "intro/getting" | grep -v project/migration)

for i in additional common dev general guide intro project security; do
  find $i -type f | while read line ; do cp $line scala/`dirname $line` ; done
  sed -i='' "s/($i/(java\/$i/" java.md
  sed -i='' "s/\(\* \[security\)/* [java\/guide](java\/guide\/index.md)\n\1/" java.md
  sed -i='' "s/\.\.\/java/../g" java/intro/index-java.md

  sed -i='' "s/($i/(scala\/$i/" scala.md
  sed -i='' "s/\(\* \[security\)/* [scala\/guide](scala\/guide\/index.md)\n\1/" scala.md
  sed -i='' "s/\.\.\/scala/../g" java/intro/index-scala.md
  sed -i='' "s/\.\.\/scala/..\/..\/scala/g" java/common/may-change.md

  sed -i='' "s/\.\.\/$i/..\/scala\/$i/g" $files

  commonfiles=$(find "$DIR/scala/$i" -name "*.md")
  sed -i='' "s/\.\.\/scala/..\/..\/scala/g" $commonfiles
  sed -i='' "s/\.\.\/java/..\/..\/java/g" $commonfiles
done

