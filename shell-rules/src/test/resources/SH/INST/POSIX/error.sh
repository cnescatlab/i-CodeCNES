#!/bin/bash
# set -x
echo "------------------"
echo "- SH.INST.POSIX  -"
echo "-      KO        -"
echo "------------------"
# Non respect de la regle SH.INST.POSIX : utilisation de la commande whoami (qui ne fait pas partie de la liste des utilitaires POSIX) 
#

# --- Definition de variables 
script=$(basename $0)

# --- Chaine a rechercher dans 1er parametre du script, si pas present chaine par defaut
findstring=$1
[ -z "${findstring}" ] && findstring="ntpd"

# --- Fichier dans lequel il faut effectuer la recherche, si parametre pas present fichier par defaut
logfile=$2
[ -z "${logfile}" ] && logfile=/var/log/syslog

# --- Si l'utilisateur est "root", on demande confirmation a l'utilisateur de confirmer la poursuite du traitement .
# --- Avant de declencher la recherche, on verifie que l'utilisateur dispose bien du droit de lecture sur le fichier .

#
# --- On commence par extraire le nom de l'utilisateur courant qui a appele le script 
# ---    plusieurs methodes possibles : 
# ---       - echo "$(whoami)" : la plus simple mais ne respecte pas POSIX
# ---       - echo "$(id -u -n)" : respecte POSIX
# 

current_user="$(whoami)"
echo "!!! Vous executez le script $script avec les droits de l'utilisateur $current_user !!!"
if [ "${current_user}" == "root" ]; then
   echo -n "!!! Voulez-vous continuer (Oui,Non) ?"
   read reponse
   if [ "${reponse}" == "N" ] || [ "${reponse}" == "Non" ] || [ "${reponse}" == "n" ] || [ "${reponse}" == "non" ]; then
      echo "Sortie du script ..."
      exit
   fi 
else 
	 echo -n "Vous ne poudrez pas continuer...."
fi
