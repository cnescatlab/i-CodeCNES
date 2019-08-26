#!/bin/bash
echo "--------------------"
echo "- SH.INST.SetShift -"
echo "-    KO            -"
echo "--------------------"

# set.sh
#
# *** MAUVAIS EXEMPLE ***
#
# Ce programme utilise extensivement set afin de
# modifier ses propres arguments, dans une boucle
# de traitement des arguments.
# !!! OK, mais pourquoi nt le probleme ?
#  faut-il pas 
set -x 
function getopts_internal()
{

	if [ $# -ne 0 ] 
	then    
	   case $1 in       
	      -[dD][iI]*)          
	         if [ -d $2 ]          
	         then             
	            repertoire=$2             
	            shift; shift             
	            set - bidon $* $repertoire/*          
	         fi
	         ;;       
	      -*)
	         echo "Option $1 inconnue, ignoree"
	         ;;       
	      *)
	         if [ -f $1 ]
	         then
	            fichiers="$fichiers $1"
	         elif [ -d $1 ]
	         then
	            repertoire=$1
	            shift
	            set - bidon $* -DIR $repertoire
	         fi
	         ;;
	   esac
	   shift
	fi
	
	echo FICHIERS TROUVES : 
	echo $fichiers
}

getopts_internal error.sh

