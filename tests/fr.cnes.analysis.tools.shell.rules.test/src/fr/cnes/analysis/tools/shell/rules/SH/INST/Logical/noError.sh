#!/bin/bash
set -x

echo "-------------------"
echo "- SH.INST.Logical -"
echo "-    OK           -"
echo "-------------------"

# Exemple de respect de la regle : Tr.TestsLogiques 
# A) Test de la presence du compilateur gcc sur la plate-forme 
#    Si pas trouve, sortie inconditionelle du script
#    Syntaxe && (||) autorisee afin d'alleger le script 
# B) Dans la boucle, decoupage des instructions if pour plus de clarte 

# Verification de la presence du compilateur sur le serveur
compilateur=gcc
path_compilateur=`which $compilateur`
[ -z "$path_compilateur" ] && echo "Compilateur GCC non installe" && exit 

# Rechercher tous les fichiers .c presents dans le repertoire courant, 
# les compiler et tester le resultat de compilation 
LISTE=$(ls *.c)

for fichier in $LISTE
do
   if [ -s $fichier ]
   then
      if gcc -Wall -c $fichier 
      then
         echo " --- Compilation de [$fichier] -> ok"
      else
         echo " --- Compilation de [$fichier] -> ko"
      fi
   fi
done
