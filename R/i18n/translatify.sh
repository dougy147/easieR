#!/bin/sh

# ./translatify.sh transation_file

for file in *; do
	SOURCE=$file;
	base=$(echo "${SOURCE%.*}" | sed "s/.*\///");
	DEST="../$(echo ${base} | sed "s/_i18n//").R";
	if [[ -f $DEST ]]; then
		while read -r line; do
			var=$(echo $line | sed "s/\ *<-·*$//");
			string=$(echo $line | sed "s/^.*<-\ *//");
			sed -i "s/$string/$var/g" "$DEST"
		done < ${file}
		echo "$DEST" found
	else
		echo "$DEST" not found. Continuing.
	fi;
done
