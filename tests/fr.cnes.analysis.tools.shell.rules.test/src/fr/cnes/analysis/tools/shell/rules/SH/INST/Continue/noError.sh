#!/bin/bash
echo "---------------------"
echo "- SH.INST.Continue_ -"
echo "-    OK             -"
echo "---------------------"

# Ce script converti tous les noms de fichiers du repertoire courant ou il s'execute .
# Il transforme le nom du fichier contenant des lettres majuscules en leur 
# equivalent en minuscule .

LISTE="$(ls)"

for nom in $LISTE; do

   if [[ "$nom" == *[[:upper:]]* ]]; then
      ORIGINE="$nom"
      NOUVEAU=`echo $nom | tr 'A-Z' 'a-z'`

      echo "Nouveau nom pour [$ORIGINE] est [$NOUVEAU]"
   fi

done
