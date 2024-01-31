#!/bin/sh

'
- 1: Extract every text between double quotes (") in scripts contained in "/R/" directory.
- 2: Sort and remove duplicates (should be optional)
- 3: Store those strings in a similarly named file in "/R/i18n/"
'

SOURCE=$1
base=$(echo "${SOURCE%.*}" | sed "s/.*\///")
#ext="${SOURCE##*.}"
DEST="${base}_i18n.R"
#echo $DEST

grep -Eo '\"[^"]*\"' $SOURCE | sort | uniq > "$(pwd)/$DEST"
#grep -Eo '\"[^"]*\"' $SOURCE | uniq > "$(pwd)/$DEST"
