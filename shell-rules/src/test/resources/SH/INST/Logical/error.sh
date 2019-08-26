#!/bin/bash

echo "-------------------"
echo "- SH.INST.Logical -"
echo "-    KO           -"
echo "-------------------"

#set -x
#set -e

# Rechercher tous les fichiers .c presents dans le repertoire courant, 
# les compiler et tester le resultat de compilation LISTE=$(ls *.c)

for fichier in $LISTE
do
   [ -s $fichier ] && gcc -Wall -c $fichier && echo "-- Compil [$fichier] -> ok"
done
