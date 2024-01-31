#!/bin/sh

# Reduce variables namespace by giving same names to identical strings

# ./variablify.sh [source_file] file_to_take_example_of
# - 1 : Grab a single translation file (i18n)
# - 2 : Check if the strings it contains are already in another translation files
# - 3 : if yes, store the variable's name directly in the other translation file
# - ... : do the same for the rest

if [[ -z $2 ]]; then
	SOURCE="*.R"
	DEST="$1"
else
	SOURCE="$1"
	DEST="$2"
fi

if [[ -z $1 ]]; then
	echo "Usage: ./variablify.sh [source_file] file_to_take_example_of"
	exit
fi

for file in ${SOURCE}; do
	SOURCE_NAME=$(echo "${file%.*}" | sed "s/.*\///");
	DEST_NAME=$(echo "${DEST%.*}" | sed "s/.*\///");
	if [[ ${SOURCE_NAME} == ${DEST_NAME} ]]; then
		continue
	fi
	echo "* Comparing '$DEST_NAME' with '$SOURCE_NAME'"
	while read -r line_src; do
		var_src=$(echo $line_src | sed "s/\ *<-\s*.*//");
		string_src=$(echo $line_src | sed "s/^.*<-\ *//");
		if [[ -z $string_src ]]; then
			continue
		fi
		while read -r line_dest; do
			if [[ ${line_src} == ${line_dest} ]]; then
				continue
			fi
			if [[ ${string_src} == ${line_dest} ]]; then
				echo "  > (0) Found identical strings, giving same variable name."
				echo "     | ${line_dest} => ${line_src}"
				sed -i "s/${line_src}/${line_dest}/g" ${file}
				continue
			fi
			var_dest=$(echo $line_dest | sed "s/\ *<-\s*.*//");
			string_dest=$(echo $line_dest | sed "s/^.*<-\ *//");
			if [[ ${string_src} == ${string_dest} ]]; then
				echo "  > (1) Found identical strings, giving same variable name."
				echo "     | ${line_dest} => ${line_src}"
				sed -i "s/${line_src}/${line_dest}/g" ${file}
				continue
			fi
		done < ${DEST}
	done < ${file}
done
