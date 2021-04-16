#!/bin/bash

ligne_fichier1=1
ligne_fichier2=1

lire_ligne_fichier1() {
	fr=$(echo $(sed -n "$ligne_fichier1"p jeu_donnees_fr))
	ligne_fichier1=$(($ligne_fichier1+1))
}

lire_ligne_fichier2() {
	anglais=$(echo $(sed -n "$ligne_fichier2"p jeu_donnees_en))
	ligne_fichier2=$(($ligne_fichier2+1))
}


while read line;
do echo "test";
lire_ligne_fichier1 ;
lire_ligne_fichier2 ;
for file in /home/luc/documents/easieR/R/*;
do sed -i "s/$fr/$anglais/g" $file;
done;
done < a_traduire
