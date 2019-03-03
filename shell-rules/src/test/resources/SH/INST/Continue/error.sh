#!/bin/bash
echo "---------------------"
echo "- SH.INST.Continue_ -"
echo "-    KO             -"
echo "---------------------"

# Ce script converti tous les noms de fichiers du repertoire courant ou il s'execute .
# Il transforme le nom du fichier contenant des lettres majuscules en leur 
# equivalent en minuscule .
# S'il n'y a aucun caractere a transformer dans le nom, l'instruction continue permet 
# de passer au nom de fichier suivant dans la liste 

LISTE="$(ls)"

for nom in $LISTE; do

   if [[ "$nom" != *[[:upper:]]* ]]; then
      continue
   fi

   ORIGINE="$nom"
   NOUVEAU=`echo $nom | tr 'A-Z' 'a-z'`

   echo "Nouveau nom pour [$ORIGINE] est [$NOUVEAU]"

done
