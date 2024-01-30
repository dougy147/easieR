#!/bin/sh

SOURCE=$(readlink -f "$1")
base="${SOURCE%.*}"
ext="${SOURCE##*.}"
DEST="${ext}_i18n.R"

grep -Eo '\"[^"]*\"' $SOURCE | sort | uniq > "./$DEST"
