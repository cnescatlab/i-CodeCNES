#!/bin/bash

echo "---------------------"
echo "- SH.INST.Variables -"
echo "-   BAD FILE        -"
echo "---------------------"

# ce script effectue un traitement consistant à etraire d'un fichier des
# lignes respondant au critere passe en parametre

# variables
FICHIER_DONNEE="_data_bad_example.txt"

# prerequis: creer un fichier de donnees
if [ ! -f "${FICHIER_DONNEE}" ]
then
  echo "This is a bad example according to SH.INST.variable rule" > $FICHIER_DONNEE
fi

# recherche
grep "rule" $FICHIER_DONNEE

${TOOLS_DIR}/Log.ksh -e $MODULE $SALMON_INFO_LOG I "Normal termination of EWAN conversion on $FILE_IN"
