#!/bin/sh

# ./translatify.sh [translation_file_i18n]

# - 1: Grab a translation_file (i18n)
# - 2: Find the coding file it corresponds to
# - 3: Replace in the coding file every occurrence of strings by variables' names

if [[ -z $1 ]]; then
	FILE='*'
else
	FILE=$1
fi

read -p "Start modifying files in './R' ? (y/N) " ans
if [[ ! $(echo $ans | grep -io "y") ]]; then
	echo "Canceled."
	exit
fi

for file in $FILE; do
	SOURCE=$file;
	base=$(echo "${SOURCE%.*}" | sed "s/.*\///");
	DEST="../$(echo ${base} | sed "s/_i18n//").R";
	if [[ -f $DEST ]]; then
		while read -r line; do
			#var=$(echo $line | sed "s/\ *<-\s*.*//");
			#string=$(echo $line | sed "s/^.*<-\ *//");
			var=$(echo $line | sed "s/\ *<-\s*.*//" | sed "s/\&/\\\&/g");
			string=$(echo $line | sed "s/^.*<-\ *//" | sed "s/\&/\\\&/g");
			sed -i "s/${string}/${var}/g" "$DEST"
		done < ${file}
		echo "$DEST" done.
	else
		echo "$DEST" not found. Continuing.
	fi;
done
