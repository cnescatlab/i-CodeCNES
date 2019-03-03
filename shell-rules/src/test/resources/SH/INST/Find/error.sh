#!/bin/bash 

#
# --- MAUVAIS EXEMPLE 
#

#
# La regle indique qu'il ne faut pas utiliser la commande ls pour extraire la liste des
# dans le repertoire courant par exemple . Lui preferer la commande find 
#

# Prerequis : creer des fichiers test
#
if [ ! -f "un fichier avec espaces dans le nom" ]; then
   touch "un fichier avec espaces dans le nom"
fi
if [ ! -f "un_fichier_sans_espaces_dans_le_nom" ]; then
   touch un_fichier_sans_espaces_dans_le_nom
fi

# Verification
ls -al un*

/bin/ls -al

# On doit obtenir une liste telle que :
#    total 8
#    drwxr-xr-x 2 postgres postgres 4096 Oct 21 16:06 .
#    drwxr-xr-x 5 postgres postgres 4096 Oct 21 16:05 ..
#    -rw-r--r-- 1 postgres postgres    0 Oct 21 16:05 un fichier avec espaces dans le nom
#    -rw-r--r-- 1 postgres postgres    0 Oct 21 16:06 un_fichier_sans_espaces_dans_le_nom

# Autre verification
find . -type f -name un\*

# On doit obtenir une liste telle que :
#    ./un fichier avec espaces dans le nom
#    ./un_fichier_sans_espaces_dans_le_nom
#

liste=$(ls)
for file in ${liste} 
do
  echo "fichier:[$file]"
done
