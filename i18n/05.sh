#!/bin/sh

#while read line; do
#	var=$(echo $line | sed "s/\ *<-\s*.*//" | sed "s/\&/\\\&/g");
#	#string=$(echo $line | sed "s/^.*<-\ *//" | sed "s/\&/\\\&/g");
#	reg="${var}[< ]"
#	if [[ $(cat ./lang_en_EN.R | grep -Eo "$reg" | wc -l) -eq 0 ]]; then
#		echo "$line" >> ./lang_en_EN.R
#	fi
#done < <(cat ./lang_fr_FR.R)

touch ./lang_fr_FR_short.R
while read line; do
	var=$(echo $line | sed "s/\ *<-\s*.*//" | sed "s/\&/\\\&/g");
	reg="${var}[< ]"
	if [[ $(cat ./lang_fr_FR_short.R | grep -Eo "$reg" | wc -l) -eq 0 ]]; then
		echo -E "$line" >> ./lang_fr_FR_short.R
	fi
done < <(cat ./lang_fr_FR.R)

#while read line; do
#	var=$(echo $line | sed "s/\ *<-\s*.*//" | sed "s/\&/\\\&/g");
#	reg="${var}[< ]"
#	if [[ $(cat ./lang_en_EN_2.R | grep -Eo "$reg" | wc -l) -eq 0 ]]; then
#		echo "$line" >> ./lang_en_EN_2.R
#	fi
#done < <(cat ./lang_en_EN.R)
