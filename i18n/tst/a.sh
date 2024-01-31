# Check if english var is in french dict
while read line;
	do
		if [[ $(grep $line lang_fr_FR.R| wc -l) -eq 0 ]]; then
			echo "trouble for $line"
		fi;
	done < $(cat ./lang_en_EN.R)

