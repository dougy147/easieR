#!/bin/sh

#'
#- 1: Extract every text between double quotes (") in scripts contained in "/R/" directory.
#- 2: Sort and remove duplicates (should be optional)
#- 3: Store those strings in a similarly named file in "/R/i18n/"
#'

SOURCE=$1
base=$(echo "${SOURCE%.*}" | sed "s/.*\///")
DEST="${base}_i18n.R"

if [[ -z $SOURCE ]]; then
	echo "Usage: ./stringify.sh source_file"
	exit 1
else
	echo "INFO: this script misses strings with linebreaks (e.g. "\\n")\n"
fi

if [[ -f "$(pwd)/$DEST" ]]; then
	while read line ; do
		if [[ $(grep -o "${line}" "$SOURCE" | head -n 1) == ${line} ]]; then
			continue
		else
			echo "${line}"
		fi;
	done < <(cat ${SOURCE} | grep -Po '\"[^"]*\"' | sort | uniq)
else
	grep -Po '\"[^"]*\"' ${SOURCE} | sort | uniq > "$(pwd)/$DEST"
fi
