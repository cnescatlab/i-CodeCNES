#!/bin/bash

echo "----------------"
echo "- SH.ERR.HELP  -"
echo "-    KO        -"
echo "----------------"

# Ce script n'implemente pas de commande specifique pour informer l'utilisateur sur les differentes options possibles d'appel.
# Le script implemente bien une fonction de type usage mais n'y fait appl qu'en cas d'erreur.

# --- Variables globales
   uid=""
   name=""

# --------------------------------------------------------
# usage : pas de parametre 
# --------------------------------------------------------
function usage()
{
   echo "usage: $0 -u <uid> -n <name>"
   echo "   Cette commande effectue la recherche dans la liste des process actifs en memoire, ceux qui"
   echo "   sont rattaches a l'utilisateur (-u) ou dans lesquels on retrouve le nom (-n). "
}

# --------------------------------------------------------
#
# --------------------------------------------------------
function getopts_internal()
{
   opt_trouve=0
   optspec="u:n:"

   # --- Y a t'il au moins des parametres ?
   if [ $# -eq 0 ]; then
      return 0
   else

      while getopts "$optspec" OPTION
      do
         case "${OPTION}" in
            n)
               name=$OPTARG
               let opt_trouve=$opt_trouve+2
            ;;
            u)
               uid=$OPTARG
               let opt_trouve=$opt_trouve+4
            ;;
            \?)
               echo "Invalid option: -$OPTARG" >&2
               usage
               return 128
            ;;
            :)
               echo "Option -$OPTARG requires an argument." >&2
               usage
               return 128
            ;;
         esac
      done
   fi

   # --- Pour info : quelles valeurs de parametres a t'on extrait ?
   echo "   getopts_internal found : uid = $uid, name = $name"
   echo " "
   return $opt_trouve
}

# ----------------------------------------------------------
#       M A I N 
# ----------------------------------------------------------

   # -----------------------------------------------------------------
   # --- Mise en oeuvre des controles avec fonction interne 'getopts'
   # -----------------------------------------------------------------

   # --- Cas nominal : parametres passes par la ligne de commande 
   getopts_internal "$@" 
   filtre=$?

   # En cas d'erreur sur les param d'appel, sortie inmmediate 
   if [[ $filtre -eq 128 ]]
   then
      exit
   fi

   # --- Construction des options de la commande ps
   ps_opt=""

   if [ $(( $filtre & 4 )) -eq 4 ]
   then
      echo "Filtrer la commande ps avec uid=$uid" 
      ps_opt=$ps_opt" -U $uid"
   fi 

   if [ $(( $filtre & 2 )) -eq 2 ]
   then
      echo "Filtrer la commande ps avec name=$name" 
      ps_opt=$ps_opt" -C $name"
   fi 

   # --- Appel de la commande 
   if [[ -z $ps_opt ]]
   then
      printf " Appel de la commande : ps -elf\n"
      ps -elf
   else
      printf " Appel de la commande : ps $ps_opt elf\n"
      ps $ps_opt elf
   fi
