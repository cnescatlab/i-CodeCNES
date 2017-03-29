#!/bin/bash

#
# *** MAUVAIS EXEMPLE ***
#

# La regle Tr.IFS interdit de modifier le contenu de la variable systeme IFS
file="/tmp/domains.txt"
if [ -f $file ]; then
   rm -f $file
fi

# Alimentation du fichier 
echo "cyberciti.biz|192.168.1.1|/home/httpd|ftpcbzuser" >>$file
echo "unixcraft.com|192.168.1.2|/home/httpd|ftpnixuser" >>$file

# Redefinition du contenu de la variable systeme IFS : violation de la regle
IFS='|'

# Boucle de traitement pour extraire chaque champ de la ligne
while read -r domaine ip webroot utilisateur
do
        printf "*** Domaine : %s ...\n" $domaine
        printf "*** Adresse IP : %s ...\n" $ip
        printf "*** Repertoire WEB root : %s %s ...\n" $webroot
        printf "*** Utilisateur FTP : %s ...\n\n" $utilisateur
done < "$file"
