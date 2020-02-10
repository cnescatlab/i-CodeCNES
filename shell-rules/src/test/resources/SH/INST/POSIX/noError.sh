#!/bin/bash
echo "------------------"
echo "- SH.INST.POSIX  -"
echo "-      OK        -"
echo "------------------"

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
# ---       - echo "$(whoami)" : la plus simple mais pas POSIX
# ---       - echo "$(id -u -n)" : respecte POSIX
# 

current_user="$(id -u -n)"
echo "!!! Vous executez le script $script avec les droits de l'utilisateur $current_user !!!"
if [ "${current_user}" == "root" ]; then
   echo -n "!!! Voulez-vous continuer (Oui,Non) ?"
   read reponse
   if [ "${reponse}" == "N" ] || [ "${reponse}" == "Non" ] || [ "${reponse}" == "n" ] || [ "${reponse}" == "non" ]; then
      echo "Sortie du script ..."
   fi 
else 
	 echo -n "Vous ne poudrez pas continuer...."
fi

echo >&2 "$NAME_ $VERSION_ - $PURPOSE_
Usage: $SYNOPSIS_
Requires: $REQUIRES_
Options:
     -i, <input_dir>, path to where to look for files. Only needed if current
         dir is not used as input dir.
     -o, <output_dir>, path to where to copy the found files. In case files with
         same filenames are found, all copied files have a \"_<n>\" appended
         incrementally to the prefix where n starts at 1.
     -s, +|-<n>, file size in bytes; +n for greater then n; -n for less then n;
         n for exactly n.
     -p, \"<pattern>\", search file pattern as accepted by find's -name option; no
         case distinction is made.
     -v, verbose
     -h, usage and options (this help)
     -l, see this script"
     
LOG_DIR="`echo $0 | sed -e 's/.tar.gz//g'`"
