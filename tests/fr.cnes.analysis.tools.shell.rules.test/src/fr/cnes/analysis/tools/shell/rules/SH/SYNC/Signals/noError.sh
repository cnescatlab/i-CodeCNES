#/bin/bash

echo "--------------------"
echo "- SH.SYNC.Signals  -"
echo "-    OK            -"
echo "--------------------"
# Variables globales 
fichier=/tmp/test_trap_truc
copie=$fichier.old

# Fonction appelee sur r�ception d'un signal pr�vu  
function nettoie
{
	# Suppression de la copie de fichier 
	if [ -f $copie ]
	then
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
trap nettoie HUP INT QUIT TERM

# Boucle sans fin
while :
do
   sleep 5
done
