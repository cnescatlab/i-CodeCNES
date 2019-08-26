#!/bin/bash

echo "----------------"
echo "- SH.ERR.HELP  -"
echo "-    OK        -"
echo "----------------"

# Par defaut, pour extraire les options du script, on utilisera la fonction (builtin) interne du shell 
# Cependant, celle-ci est limitee au traitement des options courtes (-u,-v) plutot que longues (--user, --version) .
# Le traitement des options longues necessite, l'utilisation de la commande externe getopt ou le developpement d'une 
#    fonction shell specifique (telle que getopt_long -> http://stchaz.free.fr/getopts_long) 
# Par ailleurs, si le shell utilise ne dispose pas de cette fonction d'extraction des options, 
#    alors passer par la fonction externe: c'est le cas pour c-shell 

# --- Variables globales
   uid=""
   name=""

# --------------------------------------------------------
# usage : pas de parametre 
# --------------------------------------------------------
function usage()
{
   echo "usage: $0 -u <uid> -n <name> -h"
   echo "   Cette commande effectue la recherche dans la liste des process actifs en memoire, ceux qui"
   echo "   sont rattaches a l'utilisateur (-u) ou dans lesquels on retrouve le nom (-n). "
   echo "   L'option -h permet de faire afficher cet usage. "
}

# --------------------------------------------------------
function getopts_internal()
{
   opt_trouve=0
   optspec="u:n:h"

   # --- Y a t'il au moins des parametres ?
   if [ $# -eq 0 ]; then
      return 0
   else

      while getopts "$optspec" OPTION
      do
         case "${OPTION}" in
            h)
               usage
               let opt_trouve=$opt_trouve+1
            ;;
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

# ----------------------------------------------------------------
   # --- Mise en oeuvre des controles avec fonction interne 'getopts'
# ----------------------------------------------------------------

   # --- Cas nominal : parametres passes par la ligne de commande 
   getopts_internal "$@" 
   filtre=$?

   # En cas d'erreur dans les parametres d'appel ou juste afficher l'aide, sortie immediate du script
   if [[ $filtre -eq 128 ]] || [[ $filtre -eq 1 ]]
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
