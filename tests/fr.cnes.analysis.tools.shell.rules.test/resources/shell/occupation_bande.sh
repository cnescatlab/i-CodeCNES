#!/bin/sh
# set -x

###############################################################
# utilisation_bande.sh
VERSION="3.5"
#
# Script permettant de faire la somme des espaces sauvegardes
# sur une meme bande dans le but de determiner l'espace utlise
# sur cette derniere.
#
# Stephane DIDIER - OSIATIS
#
# 3.3 : Version de base
#
# 3.4 : * Il n'est plus necessaire de mettre a jour le BASE_DIR
#         lors des mises a jour des scripts.
#
#
###############################################################


###############################################################
## Affiche l'aide d'utilisation du script                    ##
###############################################################
help() {

echo " "
echo "Usage :"
echo "     $0 [bande]"
echo " "
echo " * arg 1"
echo "   bande  : numero de la bande dont on veut estimer le"
echo "            taux d'occupation."
echo " "
}

###############################################################
## Teste si tous les arguments sont presents lors de         ##
## l'execution du script. Si ce n'est pas le cas : aide      ##
###############################################################
do_you_need_help() {

# si l'argument est vide ou que c'est pas un chiffre ==> aide puis exit
if [ -z "$1" ]||[ -z "`echo $1 | grep '^[0-9][0-9]*[0-9]*$'`" ]; then
	help
	exit 1
fi

}

###############################################################
## Recherche le repertoire de base du script                 ##
###############################################################
search_base_dir() {

BASE_DIR=$0
BASE_NAME=`basename $0`

while [ ! -d $BASE_DIR ]||[ -z "`echo $BASE_DIR | grep '^/'`" ]; do
   case $BASE_DIR in
       ${BASE_NAME})    BASE_DIR="`which ${BASE_NAME}`" ;;
       ./${BASE_NAME})  BASE_DIR="`pwd`" ;;
       /*/${BASE_NAME}) BASE_DIR="`dirname $0`" ;;
       */${BASE_NAME})  x=`pwd`; cd `dirname $BASE_DIR`; BASE_DIR=`pwd`; cd $x ;;
   esac
done

BASE_DIR="`echo $BASE_DIR | sed -e 's^/BIN$^^g'`"

}

###############################################################
###############################################################
###############################################################

do_you_need_help $1
search_base_dir

grep ",$1," ${BASE_DIR}/LOGS/SAVE_* | grep 'Ok' | cut -f2 -d':' | nawk '
   BEGIN { FS=","; ok=0; total=0
         }

         { if ( $3 == bande ) {
              total+=$5
              ok=1
           }
         }

   END   { if ( ok == 1 ) {
              print "Environ " total " Mo ont deja ete ecrits sur la bande " bande ".\n"
           } else {
              print "La bande " bande " n existe pas.\n"
           }
         }
' bande=$1
