#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.FilePath"
echo "Fichier OK de test"
echo "------------------------------------------"

if [ -f $fichier_texte ] || [ -f $fichier_texte_new ]; then
   rm -f $fichier_texte
   rm -f $fichier_texte_new
fi
