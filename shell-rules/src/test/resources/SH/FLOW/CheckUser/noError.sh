#!/bin/bash

repertoire=/tmp
script=$(basename $0)

# Lorsque l'utilisateur courant est root, on demande a 
# l'operateur s'il souhaite poursuivre le traitement

current_user="$(id -u -n)"

if [ "${current_user}" == "root" ]; then
   echo "! Execution avec les droits de $current_user !"
   echo -n "! Voulez-vous continuer (Oui,Non) ?"
   read reponse
   if [ "${reponse}" == "N" ] || [ "${reponse}" == "Non" ] || [ 
		"${reponse}" == "n" ] || [ "${reponse}" == "non" ]; then
      echo "Sortie du script ..."
      exit
   fi
fi

echo "Vous executez ce script en tant que USER : $currebt_user"
echo "- Suppression de tous les fichiers -"
echo "- rm -rf /tmp/*" 
rm -rf /tmp/*
