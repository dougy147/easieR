#!/bin/bash


compter_avec_guillemets() {
	combien_avec=$(cat * | grep -o "\"$line\"" | wc -l)
}

compter_sans_guillemets() {
	combien_sans=$(cat * | grep -o "$line" | wc -l)
}

while read line;
do echo $line;
compter_avec_guillemets ;
compter_sans_guillemets ;
if [[ $combien_avec == $combien_sans ]];
then echo "$line" >> 1_pas_des_noms_de_variables;
else echo "$line" >> 1_probablement_noms_de_variables;
fi;
done < stock_sans_guillemets
