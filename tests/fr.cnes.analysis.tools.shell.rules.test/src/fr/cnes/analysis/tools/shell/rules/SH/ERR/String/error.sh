#!/bin/bash

echo "------------------"
echo "- SH.ERR.String  -"
echo "-    KO          -"
echo "------------------"

#
# *** MAUVAIS EXEMPLE ***
# *** Si la variable info est vide, le script detecte une erreur telle que 
# *** ./shell_Tr.TestChaine_ko.sh: line 12: [: ==: unary operator expected
#

echo -n "Saisir une chaine de caractere (quelconque) :"
read info

if [ $info == 'bonjour' ]
then 
   echo 'bonsoir'
else
   echo "Informations saisies :"$info
fi
