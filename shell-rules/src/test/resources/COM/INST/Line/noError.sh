#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.LINE"
echo "Fichier OK de TU"
echo "------------------------------------------"

while :
do
	read -p "Saisir deux nombres ( - 1 pour terminer ) : " a b
	if [ $a -eq -1 ]
	then
		break
	fi
	if test $a -lt 0 -o $a -gt 1 -o $a -eq 0 ; then
    	echo 1
	fi
	total=$(( a + b ))
	echo "le resultat de l'addition de $a + $b = $total"
done
