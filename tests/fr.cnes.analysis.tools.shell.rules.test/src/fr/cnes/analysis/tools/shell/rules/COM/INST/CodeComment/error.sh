#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.CODECOMMENT"
echo "Fichier KO de TU"
echo "Le problem de ce fichier est qu'il y a des instruction commentees dans le code source"
echo "------------------------------------------"

# Ce script fonctionne dans les environnements SunOS, Solaris, HP-UX et Linux (bash ou ksh). 
# Il réalise la fonction d'affichage de sa propre taille. 
#  
#
case `uname` in
   SunOS)
      version=`uname -r | cut -d. -f1`
      if [ $version -eq 5 ]
      then
         echo "Solaris 2"
         #ls -l $1 | awk -e '{print $5}';
      else
         echo "SunOS"
         #ls -l $1 | awk -e '{print $4}'; #SET nothing, nor the log.
		 # export OSversion="SunOS"
      fi
      ;;
   HP-UX|Linux)
		echo "Linux"
         #ls -l $1 | awk '{print $5}';
		 # break
      ;;
   *) echo Systeme `uname` non reconnu.
      #exit 1;
      ;;
esac

# declare -g -A toolVersion
# eval export NSAT="\"$nsat\""
# exec kill $S $PP
