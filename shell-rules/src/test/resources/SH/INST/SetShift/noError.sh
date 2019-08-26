#!/bin/bash

echo "--------------------"
echo "- SH.INST.SetShift -"
echo "-    OK            -"
echo "--------------------"
#
# Ce programme utilise getopts pour extraire ses arguments d'appel 
#

# --------------------------------------------------------
# usage : pas de parametre
# --------------------------------------------------------
function usage()
{
   echo "usage: $0 -d <directory> -f <file>"
}

# --------------------------------------------------------
#
# --------------------------------------------------------
function getopts_internal()
{
   echo "called function : getopts_internal $*"
   user=""
   host=""
   optspec="d:f:"

   # --- Y a t'il au moins des parametres ?
   if [ $# -eq 0 ]
   then
      usage
   else

      while getopts "$optspec" OPTION
      do
         case "${OPTION}" in

            d) 
               directory=$OPTARG 
            ;;

            f) 
               file=$OPTARG 
            ;;

            \?) 
               echo "Invalid option: -$OPTARG" >&2
               exit 1 
            ;;

            :) 
               echo "Option -$OPTARG requires an argument." >&2
               exit 1 
            ;;

         esac
         # --- Pour info : indice du parametre, valeur 
         # echo "$OPTION" $OPTIND $OPTARG
      done

      # --- On verifie que les variables directory ou file ont pu etre extraites
      if [[ -z $directory ]] && [[ -z $file ]]
      then
         usage
      else
         echo "FICHIERS TROUVES :"
      fi

      # On peut maintenant effectuer le traitement 
      if [[ ! -z $directory ]]
      then
         ls $directory
      fi 
      if [[ ! -z $file ]]
      then
         ls $file
      fi    
   fi

   # --- Pour info : quelles valeurs de parametres a t'on extrait ?
   # echo "   getopts_internal found : directory = $directory, file = $file"
   # echo " "
}

# ----------------------------------------------------------
# Main
# ----------------------------------------------------------

# --- Mise en oeuvre des controles avec fonctions interne 'getopts'
# --- Cas nominal : parametres passes par la ligne de commande
getopts_internal -d noError.sh

