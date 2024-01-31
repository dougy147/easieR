#!/bin/sh

# Useful for translation of new coding files

# ./updatify.sh [source_file] file_to_translate
# - 1 : Grab a single translation file (i18n)
# - 2 : Check if the strings it contains are already translated in other translation files
# - 3 : if yes, store the variable's name directly in the current translation file
# - ... : do the same for the rest

if [[ -z $2 ]]; then
	SOURCE="*"
	DEST="$1"
else
	SOURCE="$1"
	DEST="$2"
fi

if [[ -z $1 ]]; then
	echo "Usage: ./updatify.sh [source_file] file_to_translate"
	exit
fi

#DEST="$1"
for file in $SOURCE; do
	SOURCE_NAME=$(echo "${file%.*}" | sed "s/.*\///");
	DEST_NAME=$(echo "${DEST%.*}" | sed "s/.*\///");
	if [[ ${SOURCE_NAME} == ${DEST_NAME} ]]; then
		continue
	fi
	echo "Comparing '$DEST_NAME' with '$SOURCE_NAME'"
	while read -r line_src; do
		var_src=$(echo $line_src | sed "s/\ *<-\s*.*//");
		string_src=$(echo $line_src | sed "s/^.*<-\ *//");
		while read -r line_dest; do
			if [[ ${string_src} == ${line_dest} ]]; then
				echo " > (0) Found identical strings, giving same variable name."
				sed -i "s/${line_dest}/${line_src}/g" ${DEST}
				continue
			fi
			var_dest=$(echo $line_dest | sed "s/\ *<-\s*.*//");
			string_dest=$(echo $line_dest | sed "s/^.*<-\ *//");
			if [[ ${string_src} == ${string_dest} ]]; then
				echo " > (1) Found identical strings, giving same variable name."
				sed -i "s/${line_dest}/${line_src}/g" ${DEST}
				continue
			fi
		done < ${DEST}
	done < ${file}
done
