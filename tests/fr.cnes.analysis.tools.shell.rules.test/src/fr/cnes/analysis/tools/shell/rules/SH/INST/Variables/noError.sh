#!/bin/bash

echo "---------------------"
echo "- SH.INST.Variables -"
echo "-   GOOD FILE       -"
echo "---------------------"

x=23
y=22

# ce script effectue un traitement consistant à etraire d'un fichier des
# lignes respondant au critere passe en parametre

# variables
FICHIER_DONNEE="_data_good_example.txt"

# prerequis: creer un fichier de donnees
if [ ! -f "${FICHIER_DONNEE}" ]
then
  echo "Write some text into the file" > "${FICHIER_DONNEE}"
fi

# recherche
grep "file" "${FICHIER_DONNEE}"
echo ${y} fichiers traités sur "$x"
${TOOLS_DIR}/Log.ksh -e "$MODULE" "$SALMON_INFO_LOG" I "Normal termination of EWAN conversion on ${FILE_IN}"
