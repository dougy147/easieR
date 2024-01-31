#!/bin/sh

# ./updatify.sh file_to_translate

DEST="$1"
for file in *; do
	SOURCE_NAME=$(echo "${file%.*}" | sed "s/.*\///");
	DEST_NAME=$(echo "${DEST%.*}" | sed "s/.*\///");
	if [[ ${SOURCE_NAME} == ${DEST_NAME} ]]; then
		echo "same"
#		exit
	fi
	echo $SOURCE_NAME
	while read -r line_src; do
		var_src=$(echo $line_src | sed "s/\ *<-\s*.*//");
		string_src=$(echo $line_src | sed "s/^.*<-\ *//");
		while read -r line_dest; do
			if [[ ${string_src} == ${line_dest} ]]; then
				#exit
				sed -i "s/${line_dest}/${line_src}/g" ${DEST}
				continue
			fi
			var_dest=$(echo $line_dest | sed "s/\ *<-\s*.*//");
			string_dest=$(echo $line_dest | sed "s/^.*<-\ *//");
			#sed -i "s/${string}/${var}/g" "$DEST"
			if [[ ${string_src} == ${string_dest} ]]; then
				#exit
				sed -i "s/${line_dest}/${line_src}/g" ${DEST}
				continue
			fi
		done < ${DEST}
	done < ${file}
done
