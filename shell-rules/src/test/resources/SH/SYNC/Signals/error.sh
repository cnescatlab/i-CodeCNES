#!/bin/bash
echo "--------------------"
echo "- SH.SYNC.Signals  -"
echo "-    KO            -"
echo "--------------------"
# Les signaux sont pr�cis�s avec leur num�ros :
#    - HUP  -> 1
#    - INT  -> 2
#    - QUIT -> 3
#    - TERM -> 15
# Le script est fonctionnel mais le code est moins lisible 
#

# Variables globales 
fichier=/tmp/test_trap_truc
copie=$fichier.old

# Fonction appelee sur reception d'un signal prevu  
function nettoie
{
	# Suppression de la copie de fichier 
	if [ -f $copie ]
	then
		env
		printf "   Suppression du fichier:%s\n" $copie
		rm $copie
	fi
	exit
}

#
# Main
#

# Si le fichier n'existe pas, on le cree
if [ ! -f $fichier ]
then
   touch $fichier
   echo "coucou" >$fichier
   date >$fichier
fi

# Copie du fichier 
cp $fichier $copie

# Controle visuel 
echo " "
ls -al $fichier
cat $fichier
echo " "
ls -al $copie

# Affichage du PID du shell (pour effectuer kill -HUP <pid>)
echo "shell PID:"$$

# Si reception des signaux HUP, INT, QUIT, TERM alors appel de la fonction de nettoyage
trap nettoie 1 2 3 15

# Boucle sans fin
while :
do
   sleep 5
done
