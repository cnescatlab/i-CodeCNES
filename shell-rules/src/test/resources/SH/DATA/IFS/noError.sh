#!/bin/bash
#set -x

#
# *** BON EXEMPLE ***
#

# La regle Tr.IFS interdit de modifier le contenu de la variable systeme IFS
file="/tmp/domains.txt"
if [ -f $file ]; then
   rm -f $file
fi

# Alimentation du fichier 
echo "cyberciti.biz|192.168.1.1|/home/httpd|ftpcbzuser" >>$file
echo "unixcraft.com|192.168.1.2|/home/httpd|ftpnixuser" >>$file

# Boucle de traitement pour extraire chaque champ de la ligne
LISTE="$(cat $file)"
for i in $LISTE
do

# --- Extraction des differents champs de la ligne 
	domaine=`echo "$i" | awk -F\| '{print $1}'`
	ip=`echo "$i" | awk -F\| '{print $2}'`
	webroot=`echo "$i" | awk -F\| '{print $3}'`
	utilisateur=`echo "$i" | awk -F\| '{print $4}'`

# --- Affichage des informations extraites 
        printf "*** Domaine : %s ...\n" $domaine
        printf "*** Adresse IP : %s ...\n" $ip
        printf "*** Repertoire WEB root : %s %s ...\n" $webroot
        printf "*** Utilisateur FTP : %s ...\n\n" $utilisateur
done
echo "Path du script=$PATH"
