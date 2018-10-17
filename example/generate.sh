#!/bin/sh

pandoc -f gfm -t html5 --template ./template.html \
  --filter ./PandocPagetitle.sh -o index.html index.md
