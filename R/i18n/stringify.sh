#!/bin/sh

SOURCE=$1
base=$(echo "${SOURCE%.*}" | sed "s/.*\///")
#ext="${SOURCE##*.}"
DEST="${base}_i18n.R"
#echo $DEST

grep -Eo '\"[^"]*\"' $SOURCE | sort | uniq > "$(pwd)/$DEST"
#grep -Eo '\"[^"]*\"' $SOURCE | uniq > "$(pwd)/$DEST"
