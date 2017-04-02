#!/bin/sh
# set -x

###############################################################
# cherche.sh
VERSION="3.5"
#
# Script de recherche de fichiers sauvegardes
#
# Stephane DIDIER - OSIATIS
#
# 3.2 : version de base
#
# 3.3 : * permet une recherche triee par nom de fichier au lieu
#         de par date par defaut.
#       * Gere la recherche independamment des noms des fichiers
#         d'archives tar.gz.
#
# 3.4 : * Il n'est plus necessaire de mettre a jour le BASE_DIR
#         lors des mises a jour des scripts.
#
# 3.5 : * Verifie qu'il n'y a pas de sauvegarde qui tourne.
#
#
# RESTE A FAIRE :
#
# - Decompresser les fichier de recherche dans un repertoire
#   temporaire pour permettre une recherche pendant une
#   sauvegarde et pour ne pas polluer le repertoire LOGS.
#
#
###############################################################

###############################################################
## Procedure d'affichage de l'aide                           ##
###############################################################
help() {
cat <<EOF

	Usage :
	        # $0 {-i} {-byfile} station 'fichier/expr_reguliere_grep a chercher' {[-|=|+]date_recherche}

	OPTIONS :
		-i : ne prend pas en compte la casse des caracteres
		     lors de la recherche.

		-byfile : le tri effectue par partition, par fichier et enfin par date. Cela permet de
                          retrouver rapidement tous les derniers niveaux de sauvegarde pour un fichier.

        date_recherche : date au format 20040813 pour le 13 aout 2004
                         +date : recherche un fichier partir de la date indiquee
                         -date : recherche un fichier avant la date indiquee
                         =date : recherche un fichier le jour de la date indiquee
	
EOF
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
## Verifie qu'il n'y a pas de sauvegarde en cours            ##
###############################################################
verify_process() {

if [ ! -z "`ps -e | grep 'sauve.sh'`" ]; then
   echo "Une sauvegarde est en cours..."
   exit 1
fi

}


###############################################################
## Programme principal                                       ##
###############################################################

search_base_dir
verify_process

while [ "`echo $1 | cut -c1`" = "-" ]; do
   case $1 in
      -i) OPTION="-i";;
      -byfile) TRI="byfile";;
      *) echo "	/!\ Option $1 non reconnue."; help; exit 1;;
   esac
   shift
done


if [ $# -lt 2 ]; then
   echo "	/!\ Erreur de syntaxe."
   help
   exit 1
fi

x=`pwd`; cd ${BASE_DIR}
rm -f /tmp/results.$$ /tmp/results.end.$$ 2>/dev/null

# Genere la liste des logs a scruter selon la recherche de date
if [ ! -z "$3" ]; then
   OPERATOR="`echo $3 | cut -c1`"
   DATE="`echo $3 | cut -c2-`"
   case $OPERATOR in 
      +) OPERATOR="-ge";;
      -) OPERATOR="-le";;
      =) OPERATOR="-eq";;
      *) echo "	/!\ Operateur de recherche par date incorrect"; help; exit 1;;
   esac
   cat /dev/null > /tmp/search.$$
   for FIC in `ls -1 LOGS/*.tar.gz | sort`; do
      DATE_FIC="`echo $FIC | cut -f1 -d_ | cut -f2 -d/`"
      if [ $DATE_FIC $OPERATOR $DATE ]; then echo $FIC >> /tmp/search.$$; fi
   done
else
   # Si pas de date definie, recherche dans TOUS les logs.
   ls -1 LOGS/*.tar.gz | sort > /tmp/search.$$
fi

echo " " 1>&2
echo "Recherche de $2 sur $1 :" 1>&2
echo " " 1>&2

for COMP in `cat /tmp/search.$$`; do
   LOG_DIR="`echo $COMP | sed -e 's/.tar.gz//g'`"
   echo "Decompression de $COMP dans $LOG_DIR..."
   gzip -dc ${COMP} | (cd LOGS; tar xf - )
   cd $LOG_DIR
   for FIC in `ls -rt SAVE_*$1*`; do
      echo "Traitement du fichier $LOG_DIR/$FIC..." 1>&2
      if [ ! -z "`grep $OPTION $2 ${FIC}`" ]; then
         POSITION=`echo ${FIC}  | awk 'BEGIN { FS = "_" } { print $12 }'`
         DATE=`echo ${FIC}      | awk 'BEGIN { FS = "_" } { print $2"_"$3 }'`
         BANDE=`echo ${FIC}     | awk 'BEGIN { FS = "_" } { print $10 }'`
         FILESYSTEM=`echo ${FIC}| awk 'BEGIN { FS = "_" } { print $6  }' | /usr/bin/sed 's/!/\//g'`
         NIVEAU=`echo ${FIC}    | awk 'BEGIN { FS = "_" } { print $8  }'`
         printf "|=%-14s|%5s|%4s|%1s|=%-19s|=%s\n" \
                 ============== ===== ==== = =================== ======================== >>/tmp/results.$$
         printf "| %-14s|%5s|%4s|%1s| %-19s| %s\n" DATE BANDE POS. N PARTITION FICHIER >>/tmp/results.$$
         FOUND=`printf "|%15s|%5d|%4d|%1d| %-19s|" ${DATE} ${BANDE} ${POSITION} ${NIVEAU} ${FILESYSTEM}`
         grep $OPTION $2 ${FIC} | awk '{ printf "%s %s\n", found, $1 }' found="${FOUND}" >>/tmp/results.$$
      fi
   done
   cd ${BASE_DIR}
   rm -r $LOG_DIR
done

cd $x; rm -f /tmp/search.$$

if [ -s /tmp/results.$$ ]; then
   echo "\nResultats de la recherche de $2 sur $1 :\n" 2>&1

   case $TRI in 
   	byfile)	# Tri par partition, puis par fichier et enfin par date
		sort -t'|' -db -k 6,7 -k 2,2 /tmp/results.$$ | \
                   grep -v '^| DATE' | grep -v '^|=====' > /tmp/results.end.$$
         	printf "|=%-14s|%5s|%4s|%1s|=%-19s|=%s\n" \
                 ============== ===== ==== = =================== ======================== >>/tmp/results.end.$$
         	printf "| %-14s|%5s|%4s|%1s| %-19s| %s\n" \
		 DATE BANDE POS. N PARTITION FICHIER >>/tmp/results.end.$$
		;;
        *)	# Pas de tri suplementaire autre que par date
		cat /tmp/results.$$ > /tmp/results.end.$$
		;;
   esac

   more /tmp/results.end.$$
   rm /tmp/results.$$ /tmp/results.end.$$
else
   echo "\nPas de resultat...\n" 1>&2
fi
