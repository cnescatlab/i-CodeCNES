#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.FILEPATH"
echo "Fichier KO de test"
echo "------------------------------------------"

fichier_texte=/tmp/fichier_temporaire_$$.txt
fichier_texte_new=/tmp/fichier_temporaire_$$_new.txt

if [ -f $fichier_texte ] || [ -f $fichier_texte_new ]; then
   rm -f $fichier_texte
   rm -f $fichier_texte_new
fi
