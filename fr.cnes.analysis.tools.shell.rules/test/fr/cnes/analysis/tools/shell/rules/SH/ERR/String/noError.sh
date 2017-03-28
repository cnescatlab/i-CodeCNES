#!/bin/bash

echo "------------------"
echo "- SH.ERR.String  -"
echo "-    OK          -"
echo "------------------"

#
# *** BON EXEMPLE ***
# *** le double [[ permet de se proteger du cas ou, la variable shell info est vide 
#

echo -n "Saisir une chaine de caractere (quelconque) :"
read info

if [[ $info == 'bonjour' ]]
then 
   echo 'bonsoir'
else
   echo "Informations saisies :"$info
fi
